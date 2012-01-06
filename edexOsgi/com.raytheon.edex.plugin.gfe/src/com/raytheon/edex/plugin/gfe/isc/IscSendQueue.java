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

import com.raytheon.edex.plugin.gfe.isc.IscSendRecordPK.IscSendState;
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
 * Oct 20, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscSendQueue {

    private static final transient IUFStatusHandler handler = UFStatus
            .getHandler(IscSendQueue.class);

    private int timeoutMillis = 60000;

    private Map<IscSendRecordPK, IscSendRecord> jobSet = new HashMap<IscSendRecordPK, IscSendRecord>();

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
                    "jms-iscsend:queue:iscSendNotification", messages);
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
                // in AWIPS1 if a QUEUED job matched any PENDING jobs, the
                // pending job was removed.
                if (job.getId().getState().equals(IscSendState.QUEUED)) {
                    try {
                        IscSendRecordPK recToDelete = (IscSendRecordPK) job
                                .getId().clone();
                        recToDelete.setState(IscSendState.PENDING);
                        jobSet.remove(recToDelete);
                    } catch (CloneNotSupportedException e) {
                        handler.handle(Priority.WARN,
                                "Clone of IscSendRecordPK failed.", e);
                    }
                }

                // additionally, AWIPS1 merged records with matching parm and
                // xmlDest info, if the TimeRanges of data to be sent could be
                // merged together into one contiguous TimeRange
                IscSendRecordPK mergeId = null;
                for (IscSendRecordPK id : jobSet.keySet()) {
                    if (job.getId().isMergeableWith(id)) {
                        mergeId = id;
                        break;
                    }
                }
                if (mergeId != null) {
                    IscSendRecord oldRecord = jobSet.get(mergeId);
                    Date newInsertTime = job.getInsertTime();
                    if (oldRecord.getInsertTime().compareTo(newInsertTime) < 0) {
                        oldRecord.setInsertTime(newInsertTime);
                    }
                    TimeRange newTR = oldRecord.getId().getTimeRange()
                            .combineWith(job.getId().getTimeRange());
                    oldRecord.getId().setTimeRange(newTR);
                } else {
                    jobSet.put(job.getId(), job);
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
    public void fireSendJobs() {
        List<IscSendRecord> newJobs = Collections.emptyList();
        synchronized (this) {
            if (!jobSet.isEmpty()) {
                newJobs = new ArrayList<IscSendRecord>(jobSet.values());
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
                oldRecord = (IscSendRecord) s.get(IscSendRecord.class,
                        record.getId(), LockOptions.UPGRADE);
                if (oldRecord != null) {
                    Date newInsertTime = record.getInsertTime();
                    if (oldRecord.getInsertTime().compareTo(newInsertTime) < 0) {
                        oldRecord.setInsertTime(newInsertTime);
                    }
                    s.update(oldRecord);
                    foundDupe = true;
                }

                // as in the mergeSendJobs(), delete any matching
                // PENDING records
                if (record.getId().getState().equals(IscSendState.QUEUED)) {
                    try {
                        IscSendRecordPK recToDelete = (IscSendRecordPK) record
                                .getId().clone();
                        recToDelete.setState(IscSendState.PENDING);
                        IscSendRecord toDelete = (IscSendRecord) s.get(
                                IscSendRecord.class, recToDelete,
                                LockOptions.UPGRADE);
                        if (toDelete != null) {
                            s.delete(toDelete);
                        }
                    } catch (CloneNotSupportedException e) {
                        handler.handle(Priority.WARN,
                                "Clone of IscSendRecordPK failed.", e);
                    }
                }

                // find any jobs that can be merged with this one
                if (!foundDupe) {
                    foundMerge = mergeRecordIntoDb(record, s);
                    if (!foundMerge) {
                        s.save(record);
                    }
                }

                tx.commit();
            } catch (Throwable t) {
                handler.handle(Priority.ERROR, "Error adding ISC send job ["
                        + record.getId() + "] to database queue", t);

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
    private boolean mergeRecordIntoDb(IscSendRecord record, Session s) {
        Criteria mergeCrit = s.createCriteria(IscSendRecord.class);
        Map<String, Object> critMap = new HashMap<String, Object>(3);
        critMap.put("id.parmID", record.getId().getParmID());
        critMap.put("id.state", record.getId().getState());
        critMap.put("id.xmlDest", record.getId().getXmlDest());
        mergeCrit.add(Restrictions.allEq(critMap));

        List<IscSendRecord> possibleMerges = mergeCrit.list();
        for (IscSendRecord rec : possibleMerges) {
            TimeRange trToMerge = record.getId().getTimeRange();
            TimeRange trExisting = rec.getId().getTimeRange();
            if (trToMerge.isAdjacentTo(trExisting)
                    || trToMerge.overlaps(trExisting)) {
                IscSendRecord oldRecord = (IscSendRecord) s.get(
                        IscSendRecord.class, rec.getId(), LockOptions.UPGRADE);
                if (oldRecord != null) {
                    try {
                        s.delete(oldRecord);
                        IscSendRecord newRecord = (IscSendRecord) oldRecord
                                .clone();
                        TimeRange mergedTR = trExisting.combineWith(trToMerge);
                        newRecord.getId().setTimeRange(mergedTR);
                        newRecord.setInsertTime(record.getInsertTime());

                        // ensure we have not created a duplicate record by
                        // merging the time ranges
                        IscSendRecord dupeRecord = (IscSendRecord) s.get(
                                IscSendRecord.class, newRecord.getId(),
                                LockOptions.UPGRADE);
                        if (dupeRecord != null) {
                            Date newInsertTime = newRecord.getInsertTime();
                            if (dupeRecord.getInsertTime().compareTo(
                                    newInsertTime) < 0) {
                                dupeRecord.setInsertTime(newInsertTime);
                            }
                            s.update(dupeRecord);
                        } else {
                            s.save(newRecord);
                        }
                    } catch (Exception e) {
                        handler.handle(Priority.WARN,
                                "IscSendRecord merge failed.", e);
                    }

                    return true;
                }
            }
        }

        return false;
    }

    @SuppressWarnings("unchecked")
    public void sendPending(String siteId) {
        synchronized (this) {
            // update records not yet flushed to the database
            for (IscSendRecordPK key : jobSet.keySet()) {
                if ((key.getState().equals(IscSendState.PENDING))
                        && (key.getParmID().getDbId().getSiteId()
                                .equals(siteId))) {
                    IscSendRecord record = jobSet.remove(key);
                    key.setState(IscSendState.QUEUED);
                    record.setId(key);
                    record.setInsertTime(new Date());
                    jobSet.put(key, record);
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
            pendingCrit.add(Restrictions.and(Restrictions.eq("id.state",
                    IscSendState.PENDING), Restrictions.like("id.parmID", ":"
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
                        IscSendRecord.class, rec.getId(), LockOptions.UPGRADE);
                if (oldRecord != null) {
                    IscSendRecord sendRec = (IscSendRecord) oldRecord.clone();
                    dbModSess.delete(oldRecord);
                    sendRec.getId().setState(IscSendState.QUEUED);
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

    public void setTimeoutMillis(int timeoutMillis) {
        this.timeoutMillis = timeoutMillis;
    }

    public int getTimeoutMillis() {
        return timeoutMillis;
    }
}
