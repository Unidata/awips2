/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.gfe.isc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;

import com.raytheon.edex.plugin.gfe.isc.IscSendRecord.IscSendState;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * ISC send task aggregator/queue for Camel
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling    Initial creation
 * May 08, 2012 600        dgilling    Re-work logic for handling PENDING
 *                                     records.
 * Feb 07, 2014 2357       rjpeter     iscSendNotification uri.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscSendQueue {

    // how we'll organize the temporary queue
    private class JobSetQueueKey {

        private final ParmID pid;

        private final IscSendState state;

        public JobSetQueueKey(ParmID pid, IscSendState state) {
            this.pid = pid;
            this.state = state;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = (prime * result) + ((pid == null) ? 0 : pid.hashCode());
            result = (prime * result)
                    + ((state == null) ? 0 : state.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }

            JobSetQueueKey other = (JobSetQueueKey) obj;
            if (pid == null) {
                if (other.pid != null) {
                    return false;
                }
            } else if (!pid.equals(other.pid)) {
                return false;
            }
            if (state != other.state) {
                return false;
            }
            return true;
        }

        /**
         * @return the pid
         */
        public ParmID getPid() {
            return pid;
        }

        /**
         * @return the state
         */
        public IscSendState getState() {
            return state;
        }
    }

    private static final transient IUFStatusHandler handler = UFStatus
            .getHandler(IscSendQueue.class);

    private int timeoutMillis = 60000;

    private final Map<JobSetQueueKey, List<IscSendRecord>> jobSet = new HashMap<JobSetQueueKey, List<IscSendRecord>>();

    private static final IscSendQueue instance = new IscSendQueue();

    public static IscSendQueue getInstance() {
        return instance;
    }

    /**
     * This function will send a collection of ISC send jobs to the
     * ISCSendQueue. Any EDEX code that wishes to enqueue ISC send jobs, should
     * use this function.
     * 
     * @param sendJobs
     *            A collection of ISC send jobs to add to the queue.
     */
    public static void sendToQueue(Collection<IscSendRecord> sendJobs) {
        try {
            byte[] messages = SerializationUtil.transformToThrift(sendJobs);
            EDEXUtil.getMessageProducer().sendAsyncUri(
                    "jms-durable:queue:iscSendNotification", messages);
        } catch (SerializationException e) {
            handler.error("Unable to serialize IscSendRecords.", e);
        } catch (EdexException e) {
            handler.error("Unable to post IscSendRecords to IscSendQueue.", e);
        } catch (Exception e) {
            handler.error(
                    "Exception encountered in IscSendQueue.sendToQueue().", e);
        }
    }

    /**
     * This is the Camel/Spring event-driven route start point for adding new
     * items to the ISCSendQueue. Any jobs found on the route's queue will be
     * added to the internal memory queue jobSet.
     * 
     * @param newSendJobs
     *            A collection of ISC send jobs to add to the queue.
     */
    public void addSendJobs(Collection<IscSendRecord> newSendJobs) {
        mergeSendJobs(newSendJobs);
    }

    private void mergeSendJobs(Collection<IscSendRecord> newSendJobs) {
        synchronized (this) {
            for (IscSendRecord job : newSendJobs) {
                if (job.getState().equals(IscSendState.QUEUED)) {
                    addToJobSetQueue(job);

                    // remove any matches from pending queue
                    // remove the pending entry if no times remain.
                    if (job.getXmlDest().isEmpty()) {
                        JobSetQueueKey key = new JobSetQueueKey(
                                job.getParmID(), IscSendState.PENDING);
                        List<IscSendRecord> pending = jobSet.get(key);
                        if (pending != null) {
                            removeTime(pending, job.getTimeRange());

                            if (pending.isEmpty()) {
                                jobSet.remove(key);
                            }
                        }
                    }
                } else {
                    JobSetQueueKey key = new JobSetQueueKey(job.getParmID(),
                            IscSendState.PENDING);
                    List<IscSendRecord> pending = jobSet.get(key);
                    if (pending != null) {
                        mergeTime(pending, job);
                    } else {
                        addToJobSetQueue(job);
                    }
                }
            }
        }
    }

    private void mergeTime(List<IscSendRecord> pending, IscSendRecord toMerge) {
        // insert the time range and sort by time range
        pending.add(toMerge);
        Collections.sort(pending, new Comparator<IscSendRecord>() {

            @Override
            public int compare(IscSendRecord o1, IscSendRecord o2) {
                return o1.getTimeRange().compareTo(o2.getTimeRange());
            }
        });

        // Now combine time ranges if we can
        int i = 0;
        while (i <= (pending.size() - 2)) {
            TimeRange time = pending.get(i).getTimeRange();
            TimeRange time1 = pending.get(i + 1).getTimeRange();

            if ((time.overlaps(time1)) || (time.isAdjacentTo(time1))) {
                // combine the time ranges
                TimeRange combinedTR = time.join(time1);
                pending.get(i).setTimeRange(combinedTR);
                pending.remove(i + 1);
                // we don't increment i, because we want to check the new one
                // next
            } else {
                // nothing to do but increment i
                i++;
            }
        }
    }

    private void removeTime(List<IscSendRecord> records, TimeRange replacementTR) {
        // go backwards since we are deleting entries
        for (int i = records.size() - 1; i >= 0; i--) {
            IscSendRecord record = records.get(i);
            TimeRange origTimeRange = record.getTimeRange();
            if (origTimeRange.overlaps(replacementTR)) {
                // what is remaining?
                TimeRange itime = origTimeRange.intersection(replacementTR);

                // entire range matched
                if (itime.equals(origTimeRange)) {
                    records.remove(i);
                } else if (origTimeRange.getStart().equals(itime.getStart())) {
                    // match at beginning
                    record.setTimeRange((new TimeRange(itime.getEnd(),
                            origTimeRange.getEnd())));
                } else if (origTimeRange.getEnd().equals(itime.getEnd())) {
                    // match at end
                    record.setTimeRange(new TimeRange(origTimeRange.getStart(),
                            itime.getStart()));
                } else {
                    // match in the middle, requires a split
                    TimeRange t1 = new TimeRange(origTimeRange.getStart(),
                            itime.getStart());
                    TimeRange t2 = new TimeRange(itime.getEnd(),
                            origTimeRange.getEnd());
                    records.remove(i);

                    try {
                        IscSendRecord before = record.clone();
                        before.setTimeRange(t1);
                        before.setInsertTime(new Date());
                        records.add(before);

                        IscSendRecord after = record.clone();
                        after.setTimeRange(t2);
                        after.setInsertTime(new Date());
                        records.add(after);
                    } catch (CloneNotSupportedException e) {
                        // no-op
                    }
                }
            }
        }
    }

    /**
     * This method will flush the in-memory ISC send job queue to the database.
     * Additionally, some merging will take place. If 2 queued jobs have the
     * same send state, ParmID, XML Destinations data and the send time ranges
     * are adjacent or overlapping, then the 2 jobs will be merged into one.
     * Additionally, if there exists 2 jobs and the only difference between the
     * 2 is send state (one is QUEUED and the other is PENDING), then the
     * PENDING state job will be removed.
     */
    @SuppressWarnings("unchecked")
    public void fireSendJobs() {
        List<IscSendRecord> newJobs = Collections.emptyList();
        synchronized (this) {
            if (!jobSet.isEmpty()) {
                newJobs = new ArrayList<IscSendRecord>();
                for (List<IscSendRecord> recordList : jobSet.values()) {
                    newJobs.addAll(recordList);
                }
                jobSet.clear();
            }
        }

        CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
        Session s = null;
        Transaction tx = null;
        IscSendRecord oldRecord = null;

        for (IscSendRecord record : newJobs) {
            try {
                s = cd.getHibernateTemplate().getSessionFactory().openSession();
                tx = s.beginTransaction();
                boolean foundDupe = false;
                boolean foundMerge = false;

                // find any exact dupe records and simply refresh the
                // insert time
                Criteria dupeCrit = s.createCriteria(IscSendRecord.class);
                Map<String, Object> critMap = new HashMap<String, Object>(5);
                critMap.put("parmID", record.getParmID());
                critMap.put("state", record.getState());
                critMap.put("xmlDest", record.getXmlDest());
                critMap.put("timeRange.start", record.getTimeRange().getStart());
                critMap.put("timeRange.end", record.getTimeRange().getEnd());
                dupeCrit.add(Restrictions.allEq(critMap));
                List<IscSendRecord> dupes = dupeCrit.list();

                for (IscSendRecord dupe : dupes) {
                    oldRecord = (IscSendRecord) s.get(IscSendRecord.class,
                            dupe.getKey(), LockOptions.UPGRADE);
                    if (oldRecord != null) {
                        Date newInsertTime = record.getInsertTime();
                        if (oldRecord.getInsertTime().compareTo(newInsertTime) < 0) {
                            oldRecord.setInsertTime(newInsertTime);
                        }
                        s.update(oldRecord);
                        foundDupe = true;
                    }
                }

                // as in the mergeSendJobs(), remove time from any matching
                // PENDING records
                // And merge TimeRanges of PENDING records with existing PENDING
                // records
                if (record.getState().equals(IscSendState.QUEUED)) {
                    if (record.getXmlDest().isEmpty()) {
                        removeTimeFromDbPending(record, s);
                    }
                } else {
                    // find any jobs that can be merged with this one
                    foundMerge = mergeRecordIntoDb(record, s);
                }

                if (!foundDupe && !foundMerge) {
                    s.save(record);
                }

                tx.commit();
            } catch (Throwable t) {
                handler.handle(Priority.ERROR, "Error adding ISC send job ["
                        + record.toString() + "] to database queue", t);

                if (tx != null) {
                    try {
                        tx.rollback();
                    } catch (HibernateException e) {
                        handler.handle(
                                Priority.ERROR,
                                "Error rolling back ISC send job lock transaction",
                                e);
                    }
                }
            } finally {
                if (s != null) {
                    try {
                        s.close();
                    } catch (HibernateException e) {
                        handler.handle(Priority.ERROR,
                                "Error closing ISC send job lock session", e);
                    }
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void removeTimeFromDbPending(IscSendRecord record, Session s) {
        Criteria pendingCrit = s.createCriteria(IscSendRecord.class);
        Map<String, Object> critMap = new HashMap<String, Object>(3);
        critMap.put("parmID", record.getParmID());
        critMap.put("state", IscSendState.PENDING);
        critMap.put("xmlDest", record.getXmlDest());
        pendingCrit.add(Restrictions.allEq(critMap));
        TimeRange replacementTR = record.getTimeRange();
        pendingCrit.add(Restrictions.and(
                Restrictions.le("timeRange.start", replacementTR.getEnd()),
                Restrictions.ge("timeRange.end", replacementTR.getStart())));
        List<IscSendRecord> overlappingRecords = pendingCrit.list();

        for (IscSendRecord overlapRec : overlappingRecords) {
            IscSendRecord recToModify = (IscSendRecord) s.get(
                    IscSendRecord.class, overlapRec.getKey(),
                    LockOptions.UPGRADE);
            if (recToModify != null) {
                TimeRange origTR = recToModify.getTimeRange();
                TimeRange itime = origTR.intersection(replacementTR);

                if (itime.equals(origTR)) {
                    s.delete(recToModify);
                } else if (itime.getStart().equals(origTR.getStart())) {
                    recToModify.setTimeRange(new TimeRange(itime.getEnd(),
                            origTR.getEnd()));
                    s.update(recToModify);
                } else if (itime.getEnd().equals(origTR.getEnd())) {
                    recToModify.setTimeRange(new TimeRange(origTR.getStart(),
                            itime.getStart()));
                    s.update(recToModify);
                } else {
                    try {
                        IscSendRecord before = recToModify.clone();
                        before.setTimeRange(new TimeRange(origTR.getStart(),
                                itime.getStart()));
                        before.setInsertTime(new Date());

                        IscSendRecord after = recToModify.clone();
                        after.setTimeRange(new TimeRange(itime.getEnd(), origTR
                                .getEnd()));
                        after.setInsertTime(new Date());

                        s.save(before);
                        s.save(after);
                    } catch (CloneNotSupportedException e) {
                        // no-op
                    }

                    s.delete(recToModify);
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private boolean mergeRecordIntoDb(IscSendRecord record, Session s) {
        Criteria mergeCrit = s.createCriteria(IscSendRecord.class);
        Map<String, Object> critMap = new HashMap<String, Object>(3);
        critMap.put("parmID", record.getParmID());
        critMap.put("state", record.getState());
        critMap.put("xmlDest", record.getXmlDest());
        mergeCrit.add(Restrictions.allEq(critMap));

        List<IscSendRecord> possibleMerges = mergeCrit.list();
        boolean continueMerging = true;
        boolean merged = false;
        while (continueMerging) {
            continueMerging = false;

            for (int i = possibleMerges.size() - 1; i >= 0; i--) {
                TimeRange trToMerge = record.getTimeRange();
                TimeRange trExisting = possibleMerges.get(i).getTimeRange();
                if ((trToMerge.isAdjacentTo(trExisting))
                        || (trToMerge.overlaps(trExisting))) {
                    TimeRange combinedTR = trToMerge.join(trExisting);
                    record.setTimeRange(combinedTR);

                    // delete old record from DB and this internal list as its
                    // time range is now merged into our new record
                    IscSendRecord recordToDelete = (IscSendRecord) s
                            .get(IscSendRecord.class, possibleMerges.get(i)
                                    .getKey(), LockOptions.UPGRADE);
                    if (recordToDelete != null) {
                        try {
                            s.delete(recordToDelete);
                            possibleMerges.remove(i);
                            merged = true;
                            continueMerging = true;
                        } catch (Exception e) {
                            handler.handle(Priority.WARN,
                                    "IscSendRecord merge failed.", e);
                        }
                    }
                }
            }
        }

        // ensure we haven't created a dupe record and save to the DB
        if (merged) {
            List<IscSendRecord> dupes = Collections.emptyList();
            try {
                Criteria dupeCrit = s.createCriteria(IscSendRecord.class);
                Map<String, Object> dupeCritMap = new HashMap<String, Object>(5);
                dupeCritMap.put("parmID", record.getParmID());
                dupeCritMap.put("state", record.getState());
                dupeCritMap.put("xmlDest", record.getXmlDest());
                dupeCritMap.put("timeRange.start", record.getTimeRange()
                        .getStart());
                dupeCritMap
                        .put("timeRange.end", record.getTimeRange().getEnd());
                dupeCrit.add(Restrictions.allEq(dupeCritMap));
                dupes = dupeCrit.list();
            } catch (Exception e) {
                handler.handle(Priority.WARN,
                        "Failed to retrieve possible duplicate records.", e);
            }

            try {
                // because of how our unique constraints are defined, there can
                // only ever be one possible match to a record
                if (dupes.isEmpty()) {
                    s.save(record);
                } else {
                    IscSendRecord dupeRecord = (IscSendRecord) s.get(
                            IscSendRecord.class, dupes.get(0).getKey(),
                            LockOptions.UPGRADE);
                    if (dupeRecord != null) {
                        Date newInsertTime = record.getInsertTime();
                        if (dupeRecord.getInsertTime().compareTo(newInsertTime) < 0) {
                            dupeRecord.setInsertTime(newInsertTime);
                        }
                        s.update(dupeRecord);
                    }
                }
            } catch (Exception e) {
                handler.handle(Priority.WARN, "Failed to save merged record.",
                        e);
                merged = false;
            }
        }

        return merged;
    }

    @SuppressWarnings("unchecked")
    public void sendPending(String siteId) {
        synchronized (this) {
            // update records not yet flushed to the database
            for (JobSetQueueKey key : jobSet.keySet()) {
                if ((key.getState().equals(IscSendState.PENDING))
                        && (key.getPid().getDbId().getSiteId().equals(siteId))) {
                    List<IscSendRecord> recordList = jobSet.remove(key);
                    for (IscSendRecord record : recordList) {
                        record.setState(IscSendState.QUEUED);
                        record.setInsertTime(new Date());
                        addToJobSetQueue(record);
                    }
                }
            }
        }

        StatelessSession lookupSess = null;
        CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
        List<IscSendRecord> pendingToSending = null;
        try {
            lookupSess = dao.getHibernateTemplate().getSessionFactory()
                    .openStatelessSession();

            Criteria pendingCrit = lookupSess
                    .createCriteria(IscSendRecord.class);
            pendingCrit.add(Restrictions.and(Restrictions.eq("state",
                    IscSendState.PENDING), Restrictions.like("parmID", ":"
                    + siteId + "_", MatchMode.ANYWHERE)));
            pendingToSending = pendingCrit.list();
        } catch (Throwable t) {
            handler.error("Error querying for PENDING ISC send jobs", t);
            pendingToSending = Collections.emptyList();
        } finally {
            if (lookupSess != null) {
                try {
                    lookupSess.close();
                } catch (HibernateException e) {
                    handler.handle(
                            Priority.WARN,
                            "Unable to close pending to queued ISC send job lookup session",
                            e);
                }
            }
        }

        List<IscSendRecord> sendRecords = new ArrayList<IscSendRecord>(
                pendingToSending.size());
        for (IscSendRecord rec : pendingToSending) {
            Session dbModSess = null;
            Transaction tx = null;

            try {
                dbModSess = dao.getHibernateTemplate().getSessionFactory()
                        .openSession();
                tx = dbModSess.beginTransaction();

                IscSendRecord oldRecord = (IscSendRecord) dbModSess.get(
                        IscSendRecord.class, rec.getKey(), LockOptions.UPGRADE);
                if (oldRecord != null) {
                    IscSendRecord sendRec = oldRecord.clone();
                    dbModSess.delete(oldRecord);
                    sendRec.setState(IscSendState.QUEUED);
                    sendRec.setInsertTime(new Date());
                    sendRecords.add(sendRec);
                }
                tx.commit();

            } catch (Throwable t) {
                handler.error("Failed to move record from Pending to Queued.",
                        t);
                if (tx != null) {
                    try {
                        tx.rollback();
                    } catch (HibernateException e) {
                        handler.handle(
                                Priority.WARN,
                                "Unable to rollback pending to queued ISC send job session",
                                e);
                    }
                }
            } finally {
                if (dbModSess != null) {
                    try {
                        dbModSess.close();
                    } catch (HibernateException e) {
                        handler.handle(
                                Priority.WARN,
                                "Unable to close pending to queued ISC send job modification session",
                                e);
                    }
                }
            }
        }

        mergeSendJobs(sendRecords);
    }

    private void addToJobSetQueue(IscSendRecord record) {
        JobSetQueueKey key = new JobSetQueueKey(record.getParmID(),
                record.getState());
        List<IscSendRecord> recordList = jobSet.get(key);
        if (recordList == null) {
            recordList = new ArrayList<IscSendRecord>();
            jobSet.put(key, recordList);
        }
        recordList.add(record);
    }

    public void setTimeoutMillis(int timeoutMillis) {
        this.timeoutMillis = timeoutMillis;
    }

    public int getTimeoutMillis() {
        return timeoutMillis;
    }
}
