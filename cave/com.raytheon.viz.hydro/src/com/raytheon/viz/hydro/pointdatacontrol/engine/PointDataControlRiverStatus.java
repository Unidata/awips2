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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.QueryMode;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.TimeModeType;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.ValueType;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.pdc.PDCRiverOptions;

/**
 * Point Data Control River Status
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2008            mpduff     Initial creation
 * Jul 28, 2016 4623       skorolev   Cleanup.
 * </pre>
 * 
 * @author mpduff
 */

public class PointDataControlRiverStatus {
    private static PDCRiverOptions pcOptionsRiver = null;

    private static boolean initialized = false;

    private static boolean firstTime = true;

    private static int previousStageBasis = 99;

    private static boolean stageBasisChanged = false;

    private static PointDataControlManager pdcManager = PointDataControlManager
            .getInstance();

    /**
     * Processes River Threat Index
     * 
     * @param repList
     * @param forceRetrieval
     * @return
     */
    public static List<GageData> processRiverThreatIndex(
            List<GageData> repList, boolean forceRetrieval) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PDCDataManager dataManager = PDCDataManager.getInstance();
        List<GageData> riverStatusList = null;
        double riverValue;
        double actionValue;
        double floodValue;
        RiverStat rsInfo = null;
        Map<String, GageData> riverStatusMap = new HashMap<String, GageData>();

        /*
         * Test to determine if there are any reports to process. If not then
         * return from this routine now.
         */
        if (repList == null) {
            /* Ciao */
            return repList;
        }

        /*
         * Does the user even want the river icons colored based on the threat
         * index?
         */
        if (pcOptions.getRiverStatus() == 0) {
            /* Adios */
            return repList;
        }

        /*
         * Make sure the settings are correct in the point control structure
         * which has been set aside specifically for the processing of river
         * data.
         */
        getRiverOptions();

        if (pcOptionsRiver.getStageBasis() != previousStageBasis) {
            previousStageBasis = pcOptionsRiver.getStageBasis();
            stageBasisChanged = true;
        }

        /*
         * Determine if it is necessary to retrieve the latest riverstatus
         * information
         */
        if ((pcOptionsRiver == null) || stageBasisChanged || forceRetrieval) {
            /*
             * Call "pc_process_onetime" to retrieve the latest river status
             * colors
             */
            riverStatusList = pdcManager.pcProcessOnetime(pcOptionsRiver);

            if (riverStatusList == null) {
                /*
                 * Note that all of the threat indexes should already be set to
                 * missing ('Z') at this point in the report list. Just exit
                 * this function. No more work should be required.
                 */
                return repList;
            }
        }

        for (int i = 0; i < riverStatusList.size(); i++) {
            GageData gd = riverStatusList.get(i);
            riverStatusMap.put(gd.getLid(), gd);
        }

        /*
         * For each station in the report list passed into this routine, check
         * to see if it has a corresponding entry in the river data list. If it
         * does, then find the flood stage, flood flow, action stage, and action
         * flow data for the station and determine the threat index.
         */
        for (int i = 0; i < repList.size(); i++) {
            if (riverStatusMap.containsKey(repList.get(i).getLid())) {
                /* RiverStatus data are available for this lid */
                riverValue = riverStatusMap.get(repList.get(i).getLid())
                        .getValue();

                if (riverValue != PDCConstants.MISSING_VALUE) {
                    repList.get(i).setThreatIndex(
                            ThreatIndex.THREAT_MISSING_STAGE);

                    /*
                     * Retrieve the river status information for this node. This
                     * is what colors the icons on the display.
                     */
                    rsInfo = dataManager
                            .getRiverStatus(repList.get(i).getLid());
                    if (rsInfo != null) {

                        /* Determine if the station is reporting stage or flow. */
                        if (rsInfo.getPe() != null) {
                            if (rsInfo.getPe().toUpperCase().startsWith("Q")) {
                                /* This is a river flow station. */
                                floodValue = rsInfo.getFq();
                                actionValue = rsInfo.getAq();
                            } else {
                                /* This is a river stage station. */
                                floodValue = rsInfo.getFs();
                                actionValue = rsInfo.getAs();
                            }

                            if ((actionValue != PDCConstants.MISSING_VALUE)
                                    && (actionValue != 0.0)) {
                                if (riverValue < actionValue) {
                                    repList.get(i).setThreatIndex(
                                            ThreatIndex.THREAT_NONE);
                                } else {
                                    repList.get(i).setThreatIndex(
                                            ThreatIndex.THREAT_ACTION);
                                }
                            }

                            if ((floodValue != PDCConstants.MISSING_VALUE)
                                    && (floodValue != 0.0)) {
                                if ((riverValue < floodValue)
                                        && (actionValue == PDCConstants.MISSING_VALUE)) {
                                    repList.get(i).setThreatIndex(
                                            ThreatIndex.THREAT_NONE);
                                } else if (riverValue >= floodValue) {
                                    repList.get(i).setThreatIndex(
                                            ThreatIndex.THREAT_FLOOD);
                                }
                            }
                        }
                    }
                }
            }
        }

        return repList;
    }

    /**
     * Gets River Options
     */
    private static synchronized void getRiverOptions() {
        if (!initialized) {
            if (pcOptionsRiver == null) {
                pcOptionsRiver = new PDCRiverOptions();
            }
            pcOptionsRiver.reset();
            initialized = true;
        }

        PDCOptionData pcOptions = PDCOptionData.getInstance();

        if (firstTime) {
            firstTime = false;

            /* Define the required members of the pc_options_RIVER structure. */
            pcOptionsRiver.setProcessMode(pcOptions.getProcessMode());

            pcOptionsRiver.setProcessSelected(0);
            pcOptionsRiver.setQueryMode(QueryMode.AD_HOC_MODE.getQueryMode());
            pcOptionsRiver
                    .setElementType(HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                            .getAdHocDataElementType());
            pcOptionsRiver.setSelectedAdHocElementString("xx");
            pcOptionsRiver.setPcAndpp(0);
            pcOptionsRiver.setPrimary(1);

            pcOptionsRiver.setFilterByTypeSource(0);

            List<String> l = new ArrayList<String>();
            l.add("xx");

            pcOptionsRiver.setTypeSourceChosenList(l);
            pcOptionsRiver.setTypeSourceChosenCount(1);

            /* Time mode settings. The duration is set later in this function. */
            pcOptionsRiver.setTimeMode(TimeModeType.LATEST.getTimeMode());

            pcOptionsRiver
                    .setValidTime(SimulatedTime.getSystemTime().getTime());

            pcOptionsRiver.setDurHours(24);

            pcOptionsRiver.setFilterByHSA(0);

            /* data source filtering */
            pcOptionsRiver.setFilterByDataSource(0);
            pcOptionsRiver.setDataSourceChosenCount(0);

            List<String> al = new ArrayList<String>();
            for (int i = 0; i < pcOptionsRiver.getDataSourceChosenCount(); i++) {
                al.add("dummy");
            }
            pcOptionsRiver
                    .setDataSourcesChosen(al.toArray(new String[al.size()]));

            /*
             * data filter settings. get the data for all locations, not just
             * the forecast points.
             */
            pcOptionsRiver.setSupressMissing(0);
            pcOptionsRiver.setFcstptsOnly(0);

            /* map display options */
            pcOptionsRiver.setValue(1);
            pcOptionsRiver.setId(1);
            pcOptionsRiver.setName(0);
            pcOptionsRiver.setTime(0);
            pcOptionsRiver.setIcon(1);
            pcOptionsRiver.setFloodLevel(0);
            pcOptionsRiver.setValueType(ValueType.TYPE_VALUE.getValueType());
            pcOptionsRiver.setStageBasis(2); // BASIS_MOFO
        }

        /*
         * Do not overwrite the river status duration hours and stage basis if
         * currently in time step mode.
         */
        if (pcOptions.getQueryMode() != QueryMode.TIME_STEP_MODE.getQueryMode()) {
            if ((pcOptions.getSelectedAdHocElementString().startsWith("H"))
                    || (pcOptions.getSelectedAdHocElementString()
                            .startsWith("Q")) || (pcOptions.getPrimary() == 1)) {
                pcOptionsRiver.setDurHours(pcOptions.getDurHours());
                pcOptionsRiver.setStageBasis(pcOptions.getStageBasis());
            } else {
                pcOptionsRiver.setDurHours(24);
                pcOptionsRiver.setStageBasis(2); // BASIS_MOFO
            }
        }
    }
}