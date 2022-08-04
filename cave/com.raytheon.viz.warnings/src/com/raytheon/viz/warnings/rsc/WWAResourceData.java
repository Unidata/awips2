package com.raytheon.viz.warnings.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.PracticeWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 *
 * Resource data for Watches, Warnings, and Advisories
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2011            jsanchez     Initial creation
 * Oct 25, 2013 2249       rferrel     getAvailableTimes always returns a non-empty list.
 * Apr 28, 2014 DR 17310   D. Friedman Handle null VTEC fields.
 * Aug 28, 2014 ASM #15682 D. Friedman Refactor for WouWcnWatchesResourceData.
 * Apr 09, 2018 #6963      dgilling    Support Practice mode.
 *
 * </pre>
 *
 * @author jsanchez
 */

@XmlAccessorType(XmlAccessType.NONE)
public class WWAResourceData extends AbstractRequestableResourceData {

    /*
     * The optional name of the warning resource.
     */
    @XmlAttribute(required = false)
    protected String name;

    @XmlAttribute(required = false)
    protected boolean hideSampling;

    protected Collection<AbstractWarningRecord> records;

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        boolean watchResource = false;
        records = new ArrayList<>(objects.length);
        if (objects.length > 0) {
            for (int i = 0; i < objects.length; i++) {
                records.add((AbstractWarningRecord) objects[i]);
            }
            watchResource = "A".equals(((AbstractWarningRecord) objects[0])
                    .getSig());
        } else if (loadProperties.isLoadWithoutData()) {
            // I must be trying to load without data, Ill try.
            RequestConstraint phenSig = getMetadataMap().get("phensig");
            watchResource = phenSig != null
                    && phenSig.getConstraintValue().contains(".A");
        }

        if (watchResource) {
            return new WatchesResource(this, loadProperties);
        }

        return new WarningsResource(this, loadProperties);
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] available = getAvailableWarningTimes(getMetadataMap(),
                getBinOffset());

        return available;
    }

    public DataTime[] getAvailableWarningTimes(
            Map<String, RequestConstraint> constraintMap, BinOffset binOffset)
            throws VizException {
        DbQueryResponse response = null;
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(constraintMap);

        String startTimeField = "startTime";
        String endTimeField = "endTime";
        String etn = "etn";
        String phensig = "phensig";
        String act = "act";
        String pil = "pil";
        request.addFields(new String[] { startTimeField, endTimeField, act,
                etn, phensig, pil });

        response = (DbQueryResponse) ThriftClient.sendRequest(request);
        if (response.getResults() == null) {
            return new DataTime[0];
        }

        CAVEMode caveMode = CAVEMode.getMode();
        boolean isOperational = (CAVEMode.OPERATIONAL.equals(caveMode)
                || CAVEMode.TEST.equals(caveMode) ? true : false);

        List<AbstractWarningRecord> warnings = new ArrayList<>();
        for (Map<String, Object> map : response.getResults()) {
            AbstractWarningRecord warnRec = (isOperational ? new WarningRecord()
                    : new PracticeWarningRecord());
            warnRec.setStartTime((Calendar) map.get(startTimeField));
            warnRec.setEndTime((Calendar) map.get(endTimeField));
            warnRec.setAct((String) map.get(act));
            warnRec.setPhensig((String) map.get(phensig));
            warnRec.setEtn((String) map.get(etn));
            warnRec.setPil((String) map.get(pil));
            if (isRecordTimeImportant(warnRec)) {
                warnings.add(warnRec);
            }
        }

        RequestConstraint phenSig = constraintMap.get("phensig");
        TreeSet<DataTime> startTimes = phenSig != null
                && phenSig.getConstraintValue().contains(".A") ? getWatchStartTimes(warnings)
                : getWarningStartTimes(warnings);

        // DR2249
        // When not in real time the commented code allows availableTimes to be
        // empty. This causes Null pointer exceptions when getting frames. If
        // always placing non-realtime causes other problems may want to add
        // only when startTimes is empty:
        // if (SimulatedTime.getSystemTime().isRealTime()) {
        // // Add the current time to the end of the array.
        // startTimes.add(new
        // DataTime(SimulatedTime.getSystemTime().getTime()));
        // }

        // Add current configured system time.
        startTimes.add(new DataTime(TimeUtil.newDate()));

        DataTime[] availableTimes = startTimes.toArray(new DataTime[startTimes
                .size()]);

        return availableTimes;
    }

    protected boolean isRecordTimeImportant(AbstractWarningRecord warnRec) {
        return true;
    }

    private static TreeSet<DataTime> getWarningStartTimes(
            List<AbstractWarningRecord> warnings) {
        /*
         * Only add a data time for CAN or EXP if another warning also exists.
         */
        TreeSet<DataTime> startTimes = new TreeSet<>();
        for (AbstractWarningRecord warnRec : warnings) {
            boolean valid = true;
            WarningAction action = warnRec.getAct() != null ?
                    WarningAction.valueOf(warnRec.getAct()) : null;
            if ((action == WarningAction.CAN || action == WarningAction.EXP) &&
                    warnRec.getEtn() != null && warnRec.getPhensig() != null) {
                valid = false;
                for (AbstractWarningRecord w : warnings) {
                    if (warnRec.equals(w)) {
                        continue;
                    }
                    TimeRange tr = new TimeRange(w.getStartTime(),
                            w.getEndTime());
                    if (tr.contains(warnRec.getStartTime().getTime())
                            && !warnRec.getEtn().equals(w.getEtn())
                            && !warnRec.getPhensig().equals(w.getPhensig())) {
                        valid = true;
                    }
                }
            }

            if (valid) {
                startTimes.add(new DataTime(warnRec.getStartTime()));
            }
        }
        return startTimes;
    }

    private static TreeSet<DataTime> getWatchStartTimes(
            List<AbstractWarningRecord> warnings) {
        TreeSet<DataTime> startTimes = new TreeSet<>();
        for (AbstractWarningRecord watchRec : warnings) {
            startTimes.add(new DataTime(watchRec.getStartTime()));
        }

        return startTimes;
    }

    @Override
    public boolean isRequeryNecessaryOnTimeMatch() {
        return false;
    }

    @Override
    public HashMap<String, RequestConstraint> getMetadataMap() {
        HashMap<String, RequestConstraint> metadataMap = super.getMetadataMap();

        if (metadataMap.containsKey("pluginName")) {
            metadataMap = new HashMap<>(metadataMap);
            CAVEMode caveMode = CAVEMode.getMode();
            String pluginName = (CAVEMode.OPERATIONAL.equals(caveMode)
                    || CAVEMode.TEST.equals(caveMode)) ? "warning"
                            : "practicewarning";
            metadataMap.replace("pluginName",
                    new RequestConstraint(pluginName));
        }

        return metadataMap;
    }
}
