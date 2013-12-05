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
package com.raytheon.viz.warngen.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.PracticeWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.util.AnnotationUtil;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.core.mode.CAVEMode;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Class used to manage warning data dynamically
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2011            mschenke     Initial creation
 * Feb 12, 2013 1500       mschenke     Refactored to not request full records and only request full 
 *                                      record when actually retrieving for use
 * Apr 22, 2013            jsanchez     Set the issue time for follow up warnings.
 * May 07, 2013 1973       rferrel      Corrections when getting Issue time.
 * May 10, 2013 1951       rjpeter      Updated ugcZones references
 * May 31, 2013 DR 16264   D. Friedman  Fix query in prepare method.
 * Jun 05, 2013 DR 16279   D. Friedman  Fix updating of issuance time for followups.
 * Jul 22, 2013 2176       jsanchez     Set the raw message for an EXT.
 * Aug 14, 2013 DR 16483   Qinglu Lin   Fixed no option issue in WarnGen dropdown menu after
 *                                      issuance of an CANCON and restart of CAVE.
 * Oct 16, 2013 2439       rferrel      Restrict retrieval of warnings to prevent getting future warnings.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CurrentWarnings {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CurrentWarnings.class);

    public static interface IWarningsArrivedListener {
        public void warningsArrived();
    }

    private static class WarningKey {

        private final String phensig;

        private final String etn;

        private WarningKey(String phensig, String etn) {
            this.phensig = String.valueOf(phensig);
            this.etn = String.valueOf(etn);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = prime + etn.hashCode();
            return prime * result + phensig.hashCode();
        }

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
            WarningKey other = (WarningKey) obj;
            if (etn.equals(other.etn) == false) {
                return false;
            } else if (phensig.equals(other.phensig) == false) {
                return false;
            }
            return true;
        }

    }

    private static final Map<String, CurrentWarnings> instanceMap = new HashMap<String, CurrentWarnings>();

    private static final Set<IWarningsArrivedListener> listeners = Collections
            .synchronizedSet(new HashSet<IWarningsArrivedListener>());

    static {
        String tableName = AnnotationUtil.getTableName(getWarningClass());
        IAlertObserver alertObserver = new IAlertObserver() {
            @Override
            public void alertArrived(Collection<AlertMessage> alertMessages) {
                CurrentWarnings.alertArrived(alertMessages);
            }
        };
        CAVEMode caveMode = CAVEMode.getMode();
        boolean isOperational = (CAVEMode.OPERATIONAL.equals(caveMode)
                || CAVEMode.TEST.equals(caveMode) ? true : false);
        if (isOperational) {
            ProductAlertObserver.addObserver(tableName, alertObserver);
        } else {
            ProductAlertObserver.addCustomObserver(tableName, tableName,
                    alertObserver);
        }
    }

    /**
     * Add a warnings arrived listener, listeners will get notified when new
     * warnings have been ingested in edex
     * 
     * @param listener
     */
    public static void addListener(IWarningsArrivedListener listener) {
        listeners.add(listener);
    }

    /**
     * Remove the listener
     * 
     * @param listener
     */
    public static void removeListener(IWarningsArrivedListener listener) {
        listeners.remove(listener);
    }

    /**
     * Get the CurrentWarnings instance
     * 
     * @param localizedSite
     *            3 letter localized site
     * @return
     */
    public static synchronized CurrentWarnings getInstance(String localizedSite) {
        CurrentWarnings warnings = instanceMap.get(localizedSite);
        if (warnings == null) {
            warnings = new CurrentWarnings(SiteMap.getInstance()
                    .getSite4LetterId(localizedSite));
            instanceMap.put(localizedSite, warnings);
        }
        return warnings;
    }

    private final String officeId;

    private final Map<String, AbstractWarningRecord> recordsMap = new HashMap<String, AbstractWarningRecord>();

    private final Map<WarningKey, List<AbstractWarningRecord>> warningMap = new HashMap<WarningKey, List<AbstractWarningRecord>>() {

        private static final long serialVersionUID = 1L;

        @Override
        public List<AbstractWarningRecord> get(Object key) {
            List<AbstractWarningRecord> records = super.get(key);
            if (records != null) {
                records = prepare(records);
            }
            return records;
        }
    };

    /**
     * Singleton constructor.
     * 
     * @param officeId
     */
    private CurrentWarnings(String officeId) {
        this.officeId = officeId;
        initializeData();

        // This assumes the instances stays around for the life of the JVM.
        ISimulatedTimeChangeListener changeListener = new ISimulatedTimeChangeListener() {

            @Override
            public void timechanged() {
                initializeData();
            }
        };
        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(
                changeListener);
    }

    /**
     * request and populate data
     */
    private void initializeData() {
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put("officeid", new RequestConstraint(officeId));
        Calendar time = TimeUtil.newCalendar();
        constraints.put("issueTime",
                new RequestConstraint(TimeUtil.formatDate(time),
                        ConstraintType.LESS_THAN_EQUALS));

        long t0 = System.currentTimeMillis();
        List<AbstractWarningRecord> warnings = requestRecords(constraints);
        System.out.println("Time to request CurrentWarnings records: "
                + (System.currentTimeMillis() - t0) + "ms");
        processRecords(warnings);
    }

    /**
     * get the latest record for each etn/phensig combo available
     * 
     * @return
     */
    public List<AbstractWarningRecord> getCurrentWarnings() {
        List<AbstractWarningRecord> rval = new ArrayList<AbstractWarningRecord>();

        synchronized (officeId) {
            List<WarningKey> keys = new ArrayList<WarningKey>(
                    warningMap.keySet());
            for (WarningKey key : keys) {
                AbstractWarningRecord tmp = getNewestByTracking(key.etn,
                        key.phensig);
                if ((tmp != null) && (rval.contains(tmp) == false)) {
                    rval.add(tmp);
                }
            }
        }

        return rval;
    }

    /**
     * Get a list of the correctable warning records for the phensigs. Or all
     * correctable warnings if phensigs is null
     * 
     * @param phenSigs
     * @return
     */
    public List<AbstractWarningRecord> getCorrectableWarnings(
            AbstractWarningRecord warnRec) {
        List<AbstractWarningRecord> rval = new ArrayList<AbstractWarningRecord>();
        Calendar current = TimeUtil.newCalendar();
        Calendar end = Calendar.getInstance();

        synchronized (officeId) {
            List<AbstractWarningRecord> records = warningMap.get(toKey(
                    warnRec.getPhensig(), warnRec.getEtn()));
            for (AbstractWarningRecord warning : records) {
                WarningAction action = WarningAction.valueOf(warning.getAct());
                end.setTime(warning.getStartTime().getTime());
                end.add(Calendar.MINUTE, 10);
                TimeRange t = new TimeRange(warning.getStartTime().getTime(),
                        end.getTime());
                if (((action == WarningAction.NEW)
                        || (action == WarningAction.CON) || (action == WarningAction.EXT))
                        && t.contains(current.getTime())) {
                    rval.add(warning);
                } else if ((action == WarningAction.CAN)
                        || (action == WarningAction.EXP)) {
                    rval.clear();
                    return rval;
                }
            }
        }

        return rval;
    }

    public AbstractWarningRecord getNewestByTracking(String etn, String phensig) {
        AbstractWarningRecord rval = null;
        synchronized (officeId) {
            List<AbstractWarningRecord> keyWarnings = warningMap.get(toKey(
                    phensig, etn));
            if (keyWarnings != null) {
                // filter out "future" warnings.
                List<AbstractWarningRecord> warnings = null;
                if (SimulatedTime.getSystemTime().isRealTime()) {
                    warnings = keyWarnings;
                } else {
                    warnings = new ArrayList<AbstractWarningRecord>(
                            keyWarnings.size());
                    long currentTime = TimeUtil.newCalendar().getTimeInMillis();
                    for (AbstractWarningRecord warning : keyWarnings) {
                        if (warning.getIssueTime().getTimeInMillis() <= currentTime) {
                            warnings.add(warning);
                        }
                    }
                }

                // See if we have a NEW warning
                for (AbstractWarningRecord warning : warnings) {
                    if (getAction(warning.getAct()) == WarningAction.NEW) {
                        rval = warning;
                    }
                }

                // Replace the NEW Warning with a COR if we have one
                for (AbstractWarningRecord warning : warnings) {
                    // FIXME: This was written before you could correct
                    // followups.
                    if (getAction(warning.getAct()) == WarningAction.COR) {
                        rval = warning;
                    }
                }

                // Update the end time if we have an EXT record
                for (AbstractWarningRecord warning : warnings) {
                    if (getAction(warning.getAct()) == WarningAction.EXT) {
                        if (rval != null) {
                            rval.setEndTime(warning.getEndTime());
                            rval.setIssueTime(warning.getIssueTime());
                            rval.setRawmessage(warning.getRawmessage());
                        }
                    }
                }

                // Update the geometry,counties, and ugc zones if we have a CON
                for (AbstractWarningRecord warning : warnings) {
                    if (getAction(warning.getAct()) == WarningAction.CON) {
                        if (rval != null) {
                            // rval.setAct("CON");
                            rval.setGeometry(warning.getGeometry());
                            rval.setCountyheader(warning.getCountyheader());
                            rval.setUgcZones(warning.getUgcZones());
                            rval.setLoc(warning.getLoc());
                            rval.setRawmessage(warning.getRawmessage());
                            rval.setIssueTime(warning.getIssueTime());
                        }
                    }
                }

                // If warning was canceled (CAN) or has expired (EXP), check if
                // county headers match. If so, rval = null. Otherwise check to
                // see if rval has any UGCZones that the warning does not have.
                // If there
                // are no new UGCZones, set rval to null.
                for (AbstractWarningRecord warning : warnings) {
                    WarningAction action = getAction(warning.getAct());
                    if ((action == WarningAction.CAN)
                            || (action == WarningAction.EXP)) {
                        if ((rval != null)
                                && (warning.getCountyheader().equals(
                                        rval.getCountyheader()) || warning
                                        .getUgcZones().containsAll(
                                                rval.getUgcZones()))) {
                            rval = null;
                        }
                    }
                }
            }
        }
        return rval;
    }

    /**
     * This method returns a single record representing the latest followup
     * product issued for a given etn and action type
     * 
     * @param etn
     *            etn to search
     * @param phensig
     *            phensig to search
     * @return Latest Warning
     */
    public AbstractWarningRecord getFollowUpByTracking(String etn,
            String phensig, WarningAction[] acts) {
        AbstractWarningRecord rval = null;
        synchronized (officeId) {
            List<AbstractWarningRecord> warnings = warningMap.get(toKey(
                    phensig, etn));
            if (warnings != null) {
                Calendar c = TimeUtil.newCalendar();
                c.add(Calendar.MINUTE, -10);
                TimeRange t = new TimeRange(c.getTime(), TimeUtil.newDate());

                for (AbstractWarningRecord warning : warnings) {
                    if (t.contains(warning.getIssueTime().getTime())) {
                        WarningAction action = getAction(warning.getAct());
                        for (WarningAction act : acts) {
                            if (action == act) {
                                rval = warning;
                            }
                        }
                    }
                }
            }
        }
        return rval;
    }

    /**
     * This method returns a single record representing the latest cancelled
     * product issued for a given vtec
     * 
     * @param etn
     * @param phensig
     * @return
     */
    public AbstractWarningRecord getCancelledByTracking(String etn,
            String phensig) {
        AbstractWarningRecord rval = null;
        synchronized (officeId) {
            List<AbstractWarningRecord> warnings = warningMap.get(toKey(
                    phensig, etn));
            if (warnings != null) {
                AbstractWarningRecord cancelProd = null;
                AbstractWarningRecord newProd = null;
                boolean conMatchesCan = false;
                ArrayList<AbstractWarningRecord> conProds = new ArrayList<AbstractWarningRecord>();
                Calendar c = TimeUtil.newCalendar();
                c.add(Calendar.MINUTE, -10);
                TimeRange t = new TimeRange(c.getTime(), TimeUtil.newDate());
                for (AbstractWarningRecord warning : warnings) {
                    WarningAction action = getAction(warning.getAct());
                    if (t.contains(warning.getIssueTime().getTime())
                            && (action == WarningAction.CAN)) {
                        cancelProd = warning;
                    }
                    if (action == WarningAction.NEW) {
                        newProd = warning;
                    }
                    if (action == WarningAction.CON) {
                        conProds.add(warning);
                    }
                }

                //
                for (AbstractWarningRecord rec : conProds) {
                    if (FipsUtil.containsSameCountiesOrZones(rec.getUgcZones(),
                            cancelProd.getUgcZones())) {
                        conMatchesCan = true;
                    }
                }

                if (cancelProd.getUgcZones().size() == newProd.getUgcZones()
                        .size()) {
                    // Change nothing
                    rval = cancelProd;
                } else if (conMatchesCan) {
                    // A partial CON/CAN was issued earlier, followed by a final
                    // CAN. We
                    // are correcting the final CAN
                    rval = cancelProd;
                } else {
                    // A partial CON/CAN was issued, we are correcting that CAN
                    // TODO: potential problem is that the coordinates listed in
                    // the
                    // original CON/CAN CAN segment match the con, while this
                    // will
                    // create a product which does not.
                    rval = cancelProd;
                    Geometry g1 = newProd.getGeometry();
                    Geometry g2 = conProds.get(conProds.size() - 1)
                            .getGeometry();
                    rval.setGeometry(g1.difference(g2));
                }
            }
        }
        return rval;
    }

    /**
     * Prepares a collection of records to be returned
     * 
     * @param records
     * @return
     */
    private List<AbstractWarningRecord> prepare(
            List<AbstractWarningRecord> records) {
        List<AbstractWarningRecord> prepared = new ArrayList<AbstractWarningRecord>();
        DbQueryRequest request = new DbQueryRequest();
        Set<String> dataURIs = new HashSet<String>();
        for (AbstractWarningRecord record : records) {
            if (record.getGeometry() == null) {
                dataURIs.add(record.getDataURI());
            } else {
                prepared.add(record);
            }
        }

        if (dataURIs.size() > 0) {
            long t0 = System.currentTimeMillis();
            RequestConstraint constraint = new RequestConstraint(null,
                    ConstraintType.IN);
            constraint.setConstraintValueList(dataURIs.toArray(new String[0]));
            request.addConstraint("dataURI", constraint);
            request.setEntityClass(getWarningClass());
            try {
                List<AbstractWarningRecord> toProcess = new ArrayList<AbstractWarningRecord>();
                DbQueryResponse response = (DbQueryResponse) ThriftClient
                        .sendRequest(request);
                for (AbstractWarningRecord record : response
                        .getEntityObjects(AbstractWarningRecord.class)) {
                    toProcess.add(record);
                }
                processRecords(toProcess);
                prepared.addAll(toProcess);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            System.out.println("Time to prepare " + records.size()
                    + " records = " + (System.currentTimeMillis() - t0) + "ms");
        }

        return prepared;
    }

    private void processRecords(List<AbstractWarningRecord> warnings) {
        synchronized (officeId) {
            for (AbstractWarningRecord record : warnings) {
                recordsMap.put(record.getDataURI(), record);
            }

            List<AbstractWarningRecord> records = new ArrayList<AbstractWarningRecord>(
                    recordsMap.values());

            // Sort by issue time when null fall back to insert time.
            Collections.sort(records, new Comparator<AbstractWarningRecord>() {
                @Override
                public int compare(AbstractWarningRecord o1,
                        AbstractWarningRecord o2) {
                    Calendar c1 = o1.getIssueTime();
                    if (c1 == null) {
                        c1 = o1.getInsertTime();
                    }
                    Calendar c2 = o2.getIssueTime();
                    if (c2 == null) {
                        c2 = o2.getInsertTime();
                    }
                    return c1.compareTo(c2);
                }
            });

            Map<WarningKey, List<AbstractWarningRecord>> tmpMap = new HashMap<WarningKey, List<AbstractWarningRecord>>();
            for (AbstractWarningRecord record : records) {
                WarningKey key = toKey(record.getPhensig(), record.getEtn());
                List<AbstractWarningRecord> recordsByKey = tmpMap.get(key);
                if (recordsByKey == null) {
                    recordsByKey = new LinkedList<AbstractWarningRecord>();
                    tmpMap.put(key, recordsByKey);
                }
                recordsByKey.add(record);
            }

            warningMap.clear();
            warningMap.putAll(tmpMap);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(com.raytheon.uf.viz
     * .core.alerts.AlertMessage[])
     */
    private static void alertArrived(Collection<AlertMessage> alertMessages) {
        List<String> dataURIs = new ArrayList<String>(alertMessages.size());
        for (AlertMessage am : alertMessages) {
            if (instanceMap.containsKey(String.valueOf(am.decodedAlert
                    .get("xxxid")))) {
                dataURIs.add(am.dataURI);
            }
        }

        if (dataURIs.size() == 0) {
            return;
        }

        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        RequestConstraint constraint = new RequestConstraint(null,
                ConstraintType.IN);
        for (String dataURI : dataURIs) {
            constraint.addToConstraintValueList(dataURI);
        }
        constraints.put("dataURI", constraint);

        List<AbstractWarningRecord> newRecords = requestRecords(constraints);

        Map<String, List<AbstractWarningRecord>> recordMap = new HashMap<String, List<AbstractWarningRecord>>();
        for (AbstractWarningRecord rec : newRecords) {
            // This used the key rec.getOfficeid() which can be null; which
            // can drop alerts when more then one new Record.
            // Changed to use the same key as the put.
            List<AbstractWarningRecord> recs = recordMap.get(rec.getXxxid());
            if (recs == null) {
                recs = new ArrayList<AbstractWarningRecord>();
                recordMap.put(rec.getXxxid(), recs);
            }
            recs.add(rec);
        }

        for (String key : recordMap.keySet()) {
            CurrentWarnings cw = instanceMap.get(key);
            if (cw != null) {
                cw.prepare(recordMap.get(key));
            }
        }

        for (IWarningsArrivedListener listener : listeners) {
            listener.warningsArrived();
        }
    }

    /**
     * Converts phensig and etn to a unique key
     * 
     * @param phensig
     * @param etn
     * @return
     */
    private static WarningKey toKey(String phensig, String etn) {
        return new WarningKey(phensig, etn);
    }

    /**
     * Convenience method to convert String act to WarningAction
     * 
     * @param act
     * @return
     */
    private static WarningAction getAction(String act) {
        return WarningAction.valueOf(act);
    }

    /**
     * Give the starter set of request constraints, request the warning records
     * 
     * @param constraints
     * @return
     */
    private static List<AbstractWarningRecord> requestRecords(
            Map<String, RequestConstraint> constraints) {
        Map<String, RequestConstraint> constraintsCopy = new HashMap<String, RequestConstraint>(
                constraints);
        // Ensures we won't request any records we wont use
        constraintsCopy.put("geometry", new RequestConstraint(null,
                ConstraintType.ISNOTNULL));
        List<AbstractWarningRecord> newRecords = new ArrayList<AbstractWarningRecord>();

        try {
            String insertTimeField = "insertTime";
            String dataURIField = "dataURI";
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints(constraintsCopy);
            request.setEntityClass(getWarningClass());
            request.addRequestField(dataURIField);
            request.addRequestField(insertTimeField);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            for (Map<String, Object> result : response.getResults()) {
                String dataURI = (String) result.get(dataURIField);
                AbstractWarningRecord record = (AbstractWarningRecord) RecordFactory
                        .getInstance().loadRecordFromUri(dataURI);
                record.setInsertTime((Calendar) result.get(insertTimeField));
                newRecords.add(record);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error retreiving warnings",
                    e);
        }
        return newRecords;
    }

    private static Class<? extends AbstractWarningRecord> getWarningClass() {
        CAVEMode caveMode = CAVEMode.getMode();
        boolean isOperational = (CAVEMode.OPERATIONAL.equals(caveMode)
                || CAVEMode.TEST.equals(caveMode) ? true : false);
        if (isOperational) {
            return WarningRecord.class;
        } else {
            return PracticeWarningRecord.class;
        }
    }
}
