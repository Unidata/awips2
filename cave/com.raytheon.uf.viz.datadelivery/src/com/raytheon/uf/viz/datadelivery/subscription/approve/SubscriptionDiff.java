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
package com.raytheon.uf.viz.datadelivery.subscription.approve;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Find the differences between a Subscription Object and its
 * PendingSubscription Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            mpduff       Initial creation
 * Jul 25, 2012 955        djohnson     Use List instead of ArrayList.
 * Sep 24, 2012 1157       mpduff       Use InitialPendingSubsription.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionDiff {
    /** Subscription start/end date format */
    private final SimpleDateFormat format = new SimpleDateFormat(
            "MM/dd/yyyy HH");

    private final DecimalFormat numFormat = new DecimalFormat("###.##");

    /** Active period start/end date format */
    private final SimpleDateFormat activeFormat = new SimpleDateFormat("MM/dd");

    private final String nl = "\n";

    private final Subscription sub;

    private final InitialPendingSubscription pendingSub;

    private HashMap<String, Boolean> diffMap;

    private ArrayList<String> diffList;

    /**
     * Constructor.
     * 
     * @param subscription
     *            The Subscription object
     * @param pendingSubscription
     *            The PendingSubscription object
     */
    public SubscriptionDiff(Subscription subscription,
            InitialPendingSubscription pendingSubscription) {
        this.sub = subscription;
        this.pendingSub = pendingSubscription;
        init();
    }

    private void init() {
        diffList = new ArrayList<String>();

        String[] columns = DataDeliveryUtils
                .getColumnTitles(TABLE_TYPE.SUBSCRIPTION);

        for (String col : columns) {
            diffList.add(col);
        }
    }

    /**
     * Get the differences.
     * 
     * @return String detailing the differences
     */
    public String getDifferences() {
        StringBuilder buffer = new StringBuilder(2000);

        getMap();

        if (!(sub.getPriority().equals(pendingSub.getPriority()))) {
            diffMap.put("priority", true);
        }

        if (sub.isNotify() != pendingSub.isNotify()) {
            diffMap.put("notify", true);
        }

        if (sub.isFullDataSet() != pendingSub.isFullDataSet()) {
            diffMap.put("fullDataSet", true);
        }

        if (!(sub.getDescription().equals(pendingSub.getDescription()))) {
            diffMap.put("description", true);
        }

        if (sub.getActivePeriodEnd() != null
                && pendingSub.getActivePeriodEnd() == null
                || sub.getActivePeriodEnd() == null
                && pendingSub.getActivePeriodEnd() != null) {
            diffMap.put("activePeriodEnd", true);
        } else {
            if (sub.getActivePeriodEnd() != null
                    && pendingSub.getActivePeriodEnd() != null) {
                if (!(sub.getActivePeriodEnd().equals(pendingSub
                        .getActivePeriodEnd()))) {
                    diffMap.put("activePeriodEnd", true);
                }
            }
        }

        if (sub.getActivePeriodStart() != null
                && pendingSub.getActivePeriodStart() == null
                || sub.getActivePeriodStart() == null
                && pendingSub.getActivePeriodStart() != null) {
            diffMap.put("activePeriodStart", true);
        } else {
            if (sub.getActivePeriodStart() != null
                    && pendingSub.getActivePeriodStart() != null) {
                if (!(sub.getActivePeriodStart().equals(pendingSub
                        .getActivePeriodStart()))) {
                    diffMap.put("activePeriodStart", true);
                }
            }
        }

        Time subTime = sub.getTime();
        Time pendingTime = pendingSub.getTime();

        try {
            if (subTime.getRequestStartAsDate() != null
                    && pendingTime.getRequestStartAsDate() == null
                    || subTime.getRequestStartAsDate() == null
                    && pendingTime.getRequestStartAsDate() != null) {
                diffMap.put("subscriptionStart", true);
            } else {
                if (subTime.getRequestStartAsDate() != null
                        && pendingTime.getRequestStartAsDate() != null) {
                    if (!(subTime.getRequestStartAsDate().equals(pendingTime
                            .getRequestStartAsDate()))) {
                        diffMap.put("subscriptionStart", true);
                    }
                }
            }

            if (subTime.getRequestEndAsDate() != null
                    && pendingTime.getRequestEndAsDate() == null
                    || subTime.getRequestEndAsDate() == null
                    && pendingTime.getRequestEndAsDate() != null) {
                diffMap.put("subscriptionEnd", true);
            } else {
                if (subTime.getRequestEndAsDate() != null
                        && pendingTime.getRequestEndAsDate() != null) {
                    if (!(subTime.getRequestEndAsDate().equals(pendingTime
                            .getRequestEndAsDate()))) {
                        diffMap.put("subscriptionEnd", true);
                    }
                }
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }

        // Check cycle times
        List<Integer> subCycles = subTime.getCycleTimes();
        List<Integer> pendingCycles = pendingTime.getCycleTimes();

        if (!subCycles.containsAll(pendingCycles)
                || !pendingCycles.containsAll(subCycles)) {
            diffMap.put("cycleTimes", true);
        }

        // Check forecast hours
        List<String> subFcstHoursAll = subTime.getFcstHours();
        List<String> pendingFcstHoursAll = pendingTime.getFcstHours();
        List<String> subFcstHours = new ArrayList<String>();
        List<String> pendingFcstHours = new ArrayList<String>();
        for (int i : subTime.getSelectedTimeIndices()) {
            subFcstHours.add(subFcstHoursAll.get(i));
        }

        for (int i : pendingTime.getSelectedTimeIndices()) {
            pendingFcstHours.add(pendingFcstHoursAll.get(i));
        }

        if (!subFcstHours.containsAll(pendingFcstHours)
                || !pendingFcstHours.containsAll(subFcstHours)) {
            diffMap.put("fcstHours", true);
        }

        Coverage cov = sub.getCoverage();
        Coverage pendingCov = pendingSub.getCoverage();

        ReferencedEnvelope env = cov.getRequestEnvelope();
        ReferencedEnvelope pendingEnv = pendingCov.getRequestEnvelope();

        boolean envEqual = env == null ? pendingEnv == null : env
                .equals(pendingEnv);

        if (!envEqual) {
            diffMap.put("coverage", true);
        }

        ArrayList<Parameter> subParamList = sub.getParameter();
        ArrayList<Parameter> pendingSubParamList = pendingSub.getParameter();

        ArrayList<String> subParams = new ArrayList<String>();
        ArrayList<String> pendingSubParams = new ArrayList<String>();

        ArrayList<String> newParameters = new ArrayList<String>();
        ArrayList<String> removedParameters = new ArrayList<String>();

        // Check for new or removed parameters
        for (Parameter p : subParamList) {
            subParams.add(p.getProviderName());
        }

        for (Parameter p : pendingSubParamList) {
            pendingSubParams.add(p.getProviderName());
        }

        // Check for new parameters, if in pending list, but not sub list
        for (String s : pendingSubParams) {
            if (subParams.contains(s) == false) {
                newParameters.add(s);
            }
        }

        // Check for removed parameters, if in sub list, but not pending list
        for (String s : subParams) {
            if (pendingSubParams.contains(s) == false) {
                removedParameters.add(s);
            }
        }

        // get a list of all parameters in both the new and the pending subs
        subParams.removeAll(removedParameters);
        ArrayList<ParameterDiff> parameterDiffList = new ArrayList<ParameterDiff>();
        if (subParamList.size() > pendingSubParamList.size()) {
            for (Parameter p : subParamList) {
                if (subParams.contains(p.getProviderName())) {
                    // See if anything changed for this parameter, which is
                    // layer or fcstHr
                    for (Parameter pendingP : pendingSubParamList) {
                        if (p.getProviderName().equals(
                                pendingP.getProviderName())) {
                            parameterDiffList
                                    .add(new ParameterDiff(p, pendingP));
                            break;
                        }
                    }
                }
            }
        } else {
            for (Parameter pendingP : pendingSubParamList) {
                if (subParams.contains(pendingP.getProviderName())) {
                    // See if anything changed for this parameter, which is
                    // layer or fcstHr
                    for (Parameter p : subParamList) {
                        if (p.getProviderName().equals(
                                pendingP.getProviderName())) {
                            parameterDiffList
                                    .add(new ParameterDiff(p, pendingP));
                            break;
                        }
                    }
                }
            }
        }

        StringBuilder tmpBuffer = new StringBuilder();

        for (ParameterDiff pd : parameterDiffList) {
            if (pd.isDifferent()) {
                tmpBuffer.append(pd.getDiffText());
            }
        }

        buffer.append("Changed Parameters:").append(nl);
        buffer.append("  tmpprs").append(nl);
        buffer.append("    New Levels: 1000, 900, 800, 700").append(nl);
        buffer.append("    Added Levels: 1000, 900, 800").append(nl);
        buffer.append("    Removed Levels 200, 100, 50").append(nl).append(nl);

        if (tmpBuffer.length() > 0) {
            buffer.append("Changed Parameters:").append(nl);
            buffer.append(tmpBuffer.toString());
            buffer.append(nl);
        }

        tmpBuffer = new StringBuilder();

        for (Parameter p : pendingSubParamList) {
            for (String newParameter : newParameters) {
                if (p.getProviderName().equals(newParameter)) {
                    tmpBuffer.append("Parameter: ").append(p.getProviderName())
                            .append(nl);
                    if (p.getLevelType().size() > 0) {
                        List<DataLevelType> dltList = p.getLevelType();
                        for (DataLevelType dlt : dltList) {
                            tmpBuffer.append("LevelId: ").append(dlt.getId())
                                    .append(nl);
                            tmpBuffer.append("Level Type: ")
                                    .append(dlt.getType()).append(nl);

                            if (dlt.getLayer() != null
                                    && dlt.getLayer().size() > 0) {
                                for (Double i : dlt.getLayer()) {
                                    tmpBuffer.append(i).append("  ");
                                }

                                tmpBuffer.append(nl);
                            }

                            List<Integer> selectedIndices = p.getLevels()
                                    .getSelectedLevelIndices();
                            for (int i : selectedIndices) {
                                tmpBuffer.append(
                                        p.getLevels().getLevel().get(i))
                                        .append("  ");
                            }

                            if (selectedIndices.size() > 0) {
                                tmpBuffer.append(nl);
                            }
                        }
                    }
                }
            }
        }

        if (tmpBuffer.length() > 0) {
            buffer.append("New Parameters:").append(nl);
            buffer.append(tmpBuffer.toString());
            buffer.append(nl);
        }

        if (removedParameters.size() > 0) {
            buffer.append("Removed Parameters:").append(nl);
            for (String s : removedParameters) {
                buffer.append("Parameter: ").append(s).append(nl);
                buffer.append(nl);
            }
        }

        if (diffMap.get("priority")) {
            buffer.append("Priority changed from ").append(sub.getPriority());
            buffer.append(" to ").append(pendingSub.getPriority()).append(nl)
                    .append(nl);
        }

        if (diffMap.get("subscriptionStart")) {
            buffer.append("Subscription Start changed from ");
            buffer.append(format.format(sub.getSubscriptionStart()));
            buffer.append(" to ");
            buffer.append(format.format(pendingSub.getSubscriptionStart()))
                    .append(nl).append(nl);
        }

        if (diffMap.get("subscriptionEnd")) {
            if (sub.getActivePeriodEnd() != null) {
                buffer.append("Subscription End changed from ");
                buffer.append(format.format(sub.getSubscriptionEnd()));
                buffer.append(" to ");
            } else {
                buffer.append("Subscription End set to ");
            }

            buffer.append(format.format(pendingSub.getSubscriptionEnd()))
                    .append(nl).append(nl);
        }

        boolean useActiveEnd = true;
        if (diffMap.get("activePeriodStart")) {
            if (sub.getActivePeriodStart() != null
                    && pendingSub.getActivePeriodStart() != null) {
                buffer.append("Subscription Active Period Start:").append(nl);
                buffer.append(activeFormat.format(sub.getActivePeriodStart()))
                        .append(" to ");
                buffer.append(
                        activeFormat.format(pendingSub.getActivePeriodStart()))
                        .append(nl);
            } else if (sub.getActivePeriodStart() == null
                    && pendingSub.getActivePeriodStart() != null) {
                buffer.append("Subscription Active Period Start set to ");
                buffer.append(
                        activeFormat.format(pendingSub.getActivePeriodStart()))
                        .append(nl);
            } else {
                buffer.append("Subscription Active Period has been removed.")
                        .append(nl).append(nl);
                useActiveEnd = false;
            }
        }

        if (diffMap.get("activePeriodEnd")) {
            if (useActiveEnd) {
                if (sub.getActivePeriodEnd() != null
                        && pendingSub.getActivePeriodEnd() != null) {
                    buffer.append("Subscription Active Period End:").append(nl);
                    buffer.append(activeFormat.format(sub.getActivePeriodEnd()))
                            .append(" to ");
                    buffer.append(
                            activeFormat.format(pendingSub.getActivePeriodEnd()))
                            .append(nl);
                } else if (sub.getActivePeriodEnd() == null
                        && pendingSub.getActivePeriodEnd() != null) {
                    buffer.append("Subscription Active Period End set to ");
                    buffer.append(
                            activeFormat.format(pendingSub.getActivePeriodEnd()))
                            .append(nl);
                }
            }
        }

        // TODO - diff cycle times once it is decided how we will handle
        // cycle times in data delivery
        if (diffMap.get("cycleTimes")) {
            buffer.append("Cycle Times changed:").append(nl);
            buffer.append("  New Cycle Times: ");

            for (int i : pendingCycles) {
                buffer.append(i).append("  ");
            }
            buffer.append(nl);

            String s = this.getDiffs(subCycles, pendingCycles);
            if (s != null) {
                buffer.append(s).append(nl);
            }
        }

        if (diffMap.get("fcstHours")) {
            buffer.append("Forecast Hours changed:").append(nl);
            buffer.append("  New Forecast Hours: ");
            for (String s : pendingFcstHours) {
                buffer.append(s).append("  ");
            }
            buffer.append(nl);

            String s = this.getDiffs(subFcstHours, pendingFcstHours);
            if (s != null) {
                buffer.append(s).append(nl);
            }
        }

        if (diffMap.get("notify")) {
            if (pendingSub.isNotify()) {
                buffer.append("Subscription now set to notify when data are available");
            } else {
                buffer.append("Subscription now set to deliver data when available");
            }
            buffer.append(nl).append(nl);
        }

        if (diffMap.get("fullDataSet")) {
            if (pendingSub.isFullDataSet()) {
                buffer.append("Subscription is now for the Full Data Set");
            } else {
                buffer.append("Subscription is now a subset of the original area");
            }
            buffer.append(nl).append(nl);
        }

        if (diffMap.get("coverage")) {
            if (pendingEnv == null) {
                buffer.append("Areal coverage changed to full data set")
                        .append(nl);
            } else {
                if (env == null) {
                    buffer.append("Areal coverage changed to a subset:")
                            .append(nl);
                } else {
                    buffer.append("Areal coverage changed to:").append(nl);
                }
                Coordinate ul = pendingCov.getRequestUpperLeft();
                Coordinate lr = pendingCov.getRequestLowerRight();
                buffer.append("  Upper Left : ").append(numFormat.format(ul.x))
                        .append(", ").append(numFormat.format(ul.y)).append(nl);
                buffer.append("  Lower Right: ").append(numFormat.format(lr.x))
                        .append(", ").append(numFormat.format(lr.y)).append(nl);
            }
            buffer.append(nl);
        }

        if (diffMap.get("description")) {
            buffer.append("Original Description: ");
            buffer.append(sub.getDescription()).append(nl);
            buffer.append("New Description: ");
            buffer.append(pendingSub.getDescription()).append(nl).append(nl);
        }

        buffer.append("Data size changed from 575 to 430").append(nl);

        return buffer.toString();
    }

    private String getDiffs(List<?> originalList, List<?> newList) {
        if (originalList.containsAll(newList)
                && newList.containsAll(originalList)) {
            // lists are the same, return null
            return null;
        }

        List<Object> additionsList = new ArrayList<Object>();
        List<Object> removedList = new ArrayList<Object>();

        // Find additions
        if (!originalList.containsAll(newList)) {
            for (Object o : newList) {
                if (!originalList.contains(o)) {
                    additionsList.add(o);
                }
            }
        }

        // Find removals
        if (!newList.containsAll(originalList)) {
            for (Object o : originalList) {
                if (!newList.contains(o)) {
                    removedList.add(o);
                }
            }
        }

        StringBuilder buffer = new StringBuilder("  ");
        if (additionsList.size() > 0) {
            buffer.append("Added items: ");
            for (Object o : additionsList) {
                buffer.append(o).append("  ");
            }

            buffer.append(nl);
        }
        if (removedList.size() > 0) {
            buffer.append("  Removed items: ");
            for (Object o : removedList) {
                buffer.append(o).append("  ");
            }

            buffer.append(nl);
        }

        return buffer.toString();
    }

    private HashMap<String, Boolean> getMap() {
        if (diffMap == null) {
            diffMap = new HashMap<String, Boolean>();

            diffMap.put("priority", false);
            diffMap.put("subscriptionStart", false);
            diffMap.put("subscriptionEnd", false);
            diffMap.put("activePeriodEnd", false);
            diffMap.put("activePeriodStart", false);
            diffMap.put("cycleTimes", false);
            diffMap.put("fcstHours", false);
            diffMap.put("notify", false);
            diffMap.put("fullDataSet", false);
            diffMap.put("coverage", false);
            diffMap.put("description", false);
            diffMap.put("parameter", false);
        }

        return diffMap;
    }
}
