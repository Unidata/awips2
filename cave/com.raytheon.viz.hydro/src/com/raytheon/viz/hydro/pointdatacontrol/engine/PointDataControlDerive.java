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
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Curpc;
import com.raytheon.uf.common.dataplugin.shef.tables.CurpcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.CurppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.RawpcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.tables.RawppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.TimeModeType;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.pointdatacontrol.data.IngestFilter;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.pointdatacontrol.util.PDCUtils;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.Observation;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.pdc.PDCOptions;

/**
 * PointData Control Derive.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2008            mpduff     Initial creation
 * Nov 24, 2015 5142       skorolev   Fixed time window issue for precipitation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointDataControlDerive {
    private static final int REPORT_MISSING_BELOW_MIN_PERCENT = 4;

    private static final int PRECIP_PE_BEST = 8;

    private static final int PRECIP_TS_BEST = 64;

    private List<Observation> heightList;

    private List<Observation> qList;

    private List<Riverstatus> riverStatusList;

    private List<Observation> obsList;

    private final List<GageData> dataList = new ArrayList<GageData>();

    private Map<String, List<IngestFilter>> tsRankingMap = null;

    private double change = PDCConstants.MISSING_VALUE;

    /** Token Value */
    private String minDurTokenValue = null;

    /**
     * Minimum percentage of accum interval covered by precip data.
     */
    private float minPercent;

    /**
     * This routine process river data from the RiverStatus table or the Height
     * and Discharge tables. It looks for blocks of river reports with the same
     * lid. It eliminates these multiple reports by using type source ranking
     * from the Ingest table.
     * 
     * @param isOneTime
     *            Is this being run via the runOneTime method?
     * @param pcOptions
     * 
     */
    public void deriveRiverReports(boolean isOneTime, PDCOptions pcOptions) {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        heightList = pdcManager.getObsHeightList();
        qList = pdcManager.getObsDischargeList();
        riverStatusList = pdcManager.getRiverStatusList();

        List<GageData> repObsPtr = null;
        List<GageData> repFcstPtr = null;

        String currentLid = null;
        int startRecord = 0;
        int changeWindow = 0;
        String[] useTS = null;

        /* if no lists have data, return now */
        if (((heightList == null) || (heightList.size() < 1))
                && ((qList == null) || (qList.size() < 1))
                && ((riverStatusList == null) || (riverStatusList.size() < 1))) {
            return;
        }

        /*
         * if riverstatus has data, then it must be because we are looking for
         * the latest data.
         */
        if ((riverStatusList != null) && (riverStatusList.size() > 0)) {
            List<Riverstatus> tempList = new ArrayList<Riverstatus>();
            currentLid = riverStatusList.get(0).getId().getLid();
            startRecord = 0;
            /*
             * loop thru the list and process each lid.
             */
            for (int i = 0; i < riverStatusList.size(); i++) {
                /*
                 * if the lid does not match, then we have a block of data to
                 * process
                 */
                if (!currentLid.equalsIgnoreCase(riverStatusList.get(i).getId()
                        .getLid())) {

                    /*
                     * for each lid, use the primary pe or the specified pe.
                     * when the pe is found, get the appropriate ts data. if
                     * more than one type-source's value for the location, use
                     * the ingestfilter type source rank to determine which one
                     * to use.
                     */
                    useTS = new String[2];
                    useTS[0] = "R";
                    useTS[1] = "P";

                    repObsPtr = processLidRS(useTS, tempList, startRecord,
                            repObsPtr, pcOptions);

                    useTS = new String[1];
                    useTS[0] = "F";
                    repFcstPtr = processLidRS(useTS, tempList, startRecord,
                            repFcstPtr, pcOptions);

                    /* set up data for next pass through the input list */
                    currentLid = riverStatusList.get(i).getId().getLid();
                    startRecord = i;
                    tempList.clear();
                    tempList.add(riverStatusList.get(i));
                } else {
                    tempList.add(riverStatusList.get(i));
                }
            }
            if (tempList.size() > 0) {
                useTS = new String[2];
                useTS[0] = "R";
                useTS[1] = "P";
                repObsPtr = processLidRS(useTS, tempList, startRecord,
                        repObsPtr, pcOptions);

                useTS = new String[1];
                useTS[0] = "F";
                repFcstPtr = processLidRS(useTS, tempList, startRecord,
                        repFcstPtr, pcOptions);
            }
        } else {
            /*
             * if riverstatus does not have data, then the height and/or
             * discharge lists have the data. if getting data for the primary
             * pe, then data may be loaded in both the Height list and the
             * Discharge list, so need to consider both lists. if not the
             * primary pe, only one list will have data.
             */

            /*
             * Get the change hour window if the selected time mode is
             * VALUE_CHANGE.
             */
            if (pcOptions.getTimeMode() == PDCConstants.TimeModeType.VALUE_CHANGE
                    .getTimeMode()) {
                changeWindow = PDCUtils.getChangeHourWindow();
                changeWindow *= 3600;
                changeWindow /= 2;
            }

            if ((heightList != null) && !heightList.isEmpty()) {
                List<Observation> tmpList = heightList;
                heightList = new ArrayList<>(tmpList.size());
                heightList.addAll(tmpList);
                tmpList = null;
                repObsPtr = processList(heightList, changeWindow);
            }

            if ((qList != null) && (qList.size() > 0)) {
                repObsPtr = processList(qList, changeWindow);
            }
        }

        if (isOneTime) {
            pdcManager.setFcst2ReportList(repFcstPtr);
            pdcManager.setObs2ReportList(repObsPtr);
        } else {
            pdcManager.setFcstReportList(repFcstPtr);

            /*
             * We don't want to trigger a draw here so set flag to false.
             */
            displayManager.setDrawStation(false);
            pdcManager.setObsReportList(repObsPtr);
            displayManager.setDrawStation(true);
        }
    }

    /**
     * Takes a linked list of data, given in either a list of Observation
     * records or LatestObsValue records, which contains multiple records for
     * each location. This function runs through this list and finds the best
     * value for each location, then returns a linked lists of reports.
     * 
     * @return
     */
    public List<GageData> deriveOtherReports() {
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        /* This list is one of obs or latestobsvalue */
        obsList = pdcManager.getObservationList();

        int changeWindow = 0;
        String currentLid = null;

        if ((obsList == null) || (obsList.size() == 0)) {
            return null;
        }

        currentLid = obsList.get(0).getLid();
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * Get the change hour window if the selected time mode is VALUE_CHANGE.
         */
        if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE.getTimeMode()) {
            changeWindow = PDCUtils.getChangeHourWindow();
            changeWindow *= 3600;
            changeWindow /= 2;
        }

        List<Observation> tempObsList = new ArrayList<Observation>();

        for (int i = 0; i < obsList.size(); i++) {
            if (!currentLid.equals(obsList.get(i).getLid())) {
                processLidObs(tempObsList, changeWindow);

                /*
                 * set up data for next pass through the input linked list
                 */
                currentLid = obsList.get(i).getLid();
                tempObsList.clear();
                tempObsList.add(obsList.get(i));
            } else {
                /* the lids do match so increment the count */
                tempObsList.add(obsList.get(i));
            }
        }

        /* process the leftover block of data at the end of the list */

        if (tempObsList.size() > 0) {
            processLidObs(tempObsList, changeWindow);
        }

        return dataList;
    }

    private List<GageData> processList(List<Observation> obsList,
            int changeWindow) {
        /* loop thru the list and process each lid. */
        String currentLid = obsList.get(0).getLid();
        int startRecord = 0;
        List<Observation> tempObsList = new ArrayList<Observation>();
        List<GageData> returnList = new ArrayList<GageData>();

        for (int i = 0; i < obsList.size(); i++) {
            /*
             * if the lid does not match, then we have a block of data to
             * process
             */
            if ((currentLid == null) || (obsList.get(i) == null)) {
                continue;
            }
            if (!currentLid.equals(obsList.get(i).getLid())) {

                /*
                 * for each lid, use the primary pe or the specified pe. when
                 * the pe is found, get the appropriate ts data. if more than
                 * one type-source's value for the location, use the
                 * ingestfilter type source rank to determine which one to use.
                 */

                returnList = processLidObsRiver(tempObsList, 0, changeWindow,
                        returnList);

                /*
                 * set up data for next pass through the input linked list
                 */
                currentLid = obsList.get(i).getLid();
                startRecord = i;
                tempObsList.clear();
            } else {
                /* the lids do match so increment the count */
                tempObsList.add(obsList.get(i));
            }
        }

        /* process the leftover block of data at the end of the list */

        if (tempObsList.size() > 0) {
            returnList = processLidObsRiver(tempObsList, startRecord,
                    changeWindow, returnList);
        }

        return returnList;
    }

    /**
     * Compare the TS rankings.
     * 
     * @param ts1
     *            TS to compare
     * @param ts2
     *            TS to compare
     * @param filterList
     *            IngestFilter list holding the TS Rank
     * @return -1, 0 or 1
     */
    private int compareTSRank(String ts1, String ts2,
            List<IngestFilter> filterList, String pe) {
        int rank1 = 0;
        int rank2 = 0;
        int result = 0;

        if (filterList != null) {
            for (IngestFilter filter : filterList) {
                if (filter.getTs().equals(ts1) && filter.getPe().equals(pe)) {
                    rank1 = filter.getTsRank();
                }

                if (filter.getTs().equals(ts2) && filter.getPe().equals(pe)) {
                    rank2 = filter.getTsRank();
                }
            }

            if (rank1 > rank2) {
                result = 1;
            } else if (rank1 < rank2) {
                result = -1;
            } else {
                result = 0;
            }
        }

        return result;
    }

    /**
     * Processes the river observations. For a block of river observations with
     * the same lid, determine which is the best using the TS ranking from the
     * IngestFilter Table.
     * 
     * @param startRecord
     *            Starting record
     * @param lidCount
     *            Number of lids
     * @param changeWindow
     *            Number of hours
     * @param obsList
     * @param inputList
     * @return
     */
    private List<GageData> processLidObsRiver(List<Observation> obsList,
            int startRecord, int changeWindow, List<GageData> inputList) {
        Observation bestObs = null;
        GageData oneRpt = new GageData();
        List<GageData> outputList = new ArrayList<GageData>();

        /*
         * get the best value for this location from the set of records. The
         * pointer returned is of the type Observation. It is possible that no
         * data are found because this functions handles requests for both
         * observed and forecast data, and usually forecast data are not
         * provided.
         */
        bestObs = deriveReportObsRiver(obsList, startRecord, changeWindow);

        if (bestObs != null) {
            oneRpt.setLid(bestObs.getLid());
            oneRpt.setPe(bestObs.getPe());
            oneRpt.setDur(bestObs.getDur());
            oneRpt.setTs(bestObs.getTs());
            oneRpt.setExtremum(bestObs.getExtremum());
            oneRpt.setProbability(-1);
            oneRpt.setShefQualCode("Z");
            oneRpt.setQuality_code(PDCConstants.DEFAULT_QC_VALUE);
            oneRpt.setValue(bestObs.getValue());
            oneRpt.setValidtime(bestObs.getObstime());

            /* initialize the list */
            if (inputList != null) {
                outputList = inputList;
            }

            outputList.add(oneRpt);
        } else {
            /* No data was found, so return the original list */
            outputList = inputList;
        }

        return outputList;
    }

    /**
     * This routine searches a block of RiverStatus river reports that have
     * duplicate lids. It attempts to find the best report out of the group
     * using TS-ranking information in the IngestFilter table. This best report
     * is inserted into the output report list which is ultimately used to
     * display and tabulate pointdata.
     * 
     * @param useTs
     *            Type Source
     * @param rsList
     *            Riverstatus list
     * @param startRecord
     * @param inputList
     *            GageData list
     * @param pcOptions
     * @return
     */
    private List<GageData> processLidRS(String[] useTs,
            List<Riverstatus> rsList, int startRecord,
            List<GageData> inputList, PDCOptions pcOptions) {
        Riverstatus best = null;
        GageData oneRpt = new GageData();
        List<GageData> outputList = new ArrayList<GageData>();

        /*
         * get the best value for this location from the set of records. The
         * pointer returned is of the type RiverStatus. it is possible that no
         * data are found because this functions handles requests for both
         * observed and forecast data, and usually forecast data are not
         * provided.
         */

        best = deriveReportRS(useTs, rsList, startRecord, pcOptions);

        /*
         * if data was found, then copy the best report into the linked list of
         * Reports
         */

        if (best != null) {
            oneRpt.setLid(best.getId().getLid());
            oneRpt.setPe(best.getId().getPe());
            oneRpt.setDur(best.getDur());
            oneRpt.setTs(best.getId().getTs());
            oneRpt.setExtremum(best.getExtremum());
            oneRpt.setProbability(best.getProbability());
            oneRpt.setShefQualCode("Z");
            oneRpt.setQuality_code(PDCConstants.DEFAULT_QC_VALUE);
            oneRpt.setValue(best.getValue());
            oneRpt.setValidtime(best.getValidtime());
            oneRpt.setBasistime(best.getBasistime());

            /* initialize the list */
            if (inputList != null) {
                outputList = inputList;
            }

            outputList.add(oneRpt);
        } else {
            /* No data was found, so return the original list */
            outputList = inputList;
        }

        return outputList;
    }

    /**
     * Process Lid Obs
     * 
     * @param tempObsList
     * @param changeWindow
     */
    private void processLidObs(List<Observation> tempObsList, int changeWindow) {
        GageData oneRpt = new GageData();
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * get the best value for this location from the set of records. The
         * pointer returned is of the type Observation.
         */
        Observation best = deriveReportObs(tempObsList, changeWindow);

        /* copy the best report into the linked list of Reports */
        if (best != null) {
            /* copy fields */
            oneRpt.setLid(best.getLid());
            oneRpt.setPe(best.getPe());
            oneRpt.setDur(best.getDur());
            oneRpt.setTs(best.getTs());
            oneRpt.setExtremum(best.getExtremum());
            oneRpt.setProbability(-1);
            oneRpt.setShefQualCode(best.getShefQualCode());
            oneRpt.setQuality_code(best.getQualityCode());

            if (pcOptions.getTimeMode() != TimeModeType.VALUE_CHANGE
                    .getTimeMode()) {
                oneRpt.setValue(best.getValue());
            } else {
                oneRpt.setValue(change);
            }

            oneRpt.setValidtime(best.getObstime());
            oneRpt.setBasistime(null);

            dataList.add(oneRpt);
        }
    }

    /**
     * given a linked list of data, and the start and end position of the data
     * for a single location and the number of rows inclusive in this block of
     * records, this function determines the "best" value for the location.
     * 
     * @param obsList
     *            The list of Observations
     * @param startRecord
     *            The starting record
     * @param changeWindow
     *            Number of hours
     * @return
     */
    private Observation deriveReportObs(List<Observation> obsList,
            int changeWindow) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PDCDataManager manager = PDCDataManager.getInstance();
        Observation bestObs = null;
        Observation bestObs2 = null;
        String currentTS = null;
        String bestTS = null;
        long currentDiff;
        long bestDiff;
        int firstObsFound = 0;
        int lastObsFound = 0;

        if (obsList == null) {
            return null;
        }

        bestTS = obsList.get(0).getTs();
        currentTS = obsList.get(0).getTs();

        /*
         * loop thru the records and find the best type-source to use for the
         * physical element.
         */

        tsRankingMap = manager.getIngestFilterData();

        for (int i = 0; i < obsList.size(); i++) {
            /* anytime a new ts is noted, then see if it the best one */
            if (obsList.get(i).getTs().compareTo(currentTS) != 0) {

                /*
                 * since there is more than one type-source, go get the ingest
                 * filter entries for this location for later use. do not filter
                 * the request by duration or extremum, since that is done via
                 * the original query, if at all
                 */

                /*
                 * compare the ts just found with the best one and set it to the
                 * best ts if it has a higher rank
                 */
                int result = compareTSRank(obsList.get(i).getTs(), bestTS,
                        tsRankingMap.get(obsList.get(i).getLid()),
                        obsList.get(i).getPe());
                if (result < 0) {
                    bestTS = obsList.get(i).getTs();
                }

                /* update for subsequent passes */
                currentTS = obsList.get(i).getTs();
            }
        }

        /*
         * now at this point, we know what the best ts is in the linked list.
         * loop thru the list, considering only those that match this ts, and
         * find the appropriate value to return based on the time mode.
         */
        bestDiff = PDCConstants.MISSING_VALUE;

        for (int i = 0; i < obsList.size(); i++) {
            if (bestTS.compareTo(obsList.get(i).getTs()) == 0) {
                if ((pcOptions.getTimeMode() == TimeModeType.LATEST
                        .getTimeMode())
                        || (pcOptions.getTimeMode() == TimeModeType.MAXSELECT
                                .getTimeMode())
                        || (pcOptions.getTimeMode() == TimeModeType.MINSELECT
                                .getTimeMode())) {
                    /*
                     * for all but the SETTIME and VALUE_CHANGE modes, use the
                     * first value, which is the latest
                     */
                    return obsList.get(i);

                } else if (pcOptions.getTimeMode() == TimeModeType.SETTIME
                        .getTimeMode()) {
                    currentDiff = Math.abs(pcOptions.getValidTime().getTime()
                            - obsList.get(i).getObstime().getTime());

                    if ((bestDiff == PDCConstants.MISSING_VALUE)
                            || (currentDiff < bestDiff)) {
                        bestDiff = currentDiff;
                        bestObs = obsList.get(i);
                    }

                    if (bestDiff == 0) {
                        return bestObs;
                    }
                } else if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                        .getTimeMode()) {
                    /* Find the best observation in the upper time window. */
                    if (firstObsFound == 0) {
                        currentDiff = Math.abs(pcOptions.getValidTime()
                                .getTime()
                                - obsList.get(i).getObstime().getTime());

                        /* Is the observation inside the window? */
                        if (currentDiff <= changeWindow) {
                            /*
                             * Is the observation the closest to the center time
                             * so far?
                             */
                            if ((bestDiff == PDCConstants.MISSING_VALUE)
                                    || (currentDiff <= bestDiff)) {
                                bestDiff = currentDiff;
                                bestObs = obsList.get(i);
                            } else {
                                firstObsFound = 1;
                                bestDiff = PDCConstants.MISSING_VALUE;
                                --i;
                                continue;
                            }
                        } else {
                            if (bestDiff != PDCConstants.MISSING_VALUE) {
                                firstObsFound = 1;
                                bestDiff = PDCConstants.MISSING_VALUE;
                                --i;
                                continue;
                            }
                        }
                    } else if (lastObsFound == 0) {
                        /* Find the best observation in the lower time window. */
                        currentDiff = Math
                                .abs(pcOptions.getValidTime().getTime()
                                        - (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR));

                        /* Is the observation inside the window? */
                        if (currentDiff <= changeWindow) {
                            /*
                             * Is the observation the closest to the center time
                             * so far?
                             */
                            if ((bestDiff == PDCConstants.MISSING_VALUE)
                                    || (currentDiff <= bestDiff)) {
                                bestDiff = currentDiff;
                                bestObs2 = obsList.get(i);
                            } else {
                                lastObsFound = 1;
                            }
                        } else {
                            if (bestDiff != PDCConstants.MISSING_VALUE) {
                                lastObsFound = 1;
                            }
                        }
                    } else {
                        /*
                         * Both observations necessary to compute the change
                         * have been found.
                         */
                        change = bestObs.getValue() - bestObs2.getValue();
                        return bestObs;
                    }
                }
            }
        }

        if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE.getTimeMode()) {
            if ((firstObsFound == 1)
                    && (bestDiff != PDCConstants.MISSING_VALUE)) {
                change = bestObs.getValue() - bestObs2.getValue();
            }
        }

        return bestObs;
    }

    /**
     * For a list of river observations retrieved from the Height or Discharge
     * tables in the IHFS database, processes a block of observations with the
     * same lid. The best observation from this block is selected through the
     * use of TS ranking. The TS ranking information is taken from the
     * IngestFilter table.
     * 
     * @param obsList
     *            The list of Observations
     * @param startRecord
     *            The starting record
     * @param changeWindow
     *            Number of hours
     * @return
     */
    private Observation deriveReportObsRiver(List<Observation> obsList,
            int startRecord, int changeWindow) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PDCDataManager manager = PDCDataManager.getInstance();

        Observation bestObs = null;
        Observation bestObs2 = null;
        String currentTS = null;
        String usePE = null;
        String bestTS = null;
        boolean recordFound = false;
        long currentDiff;
        int firstObsFound = 0;
        int lastObsFound = 0;

        /* determine which physical element to use for this location */
        if (pcOptions.getPrimary() == 1) {
            usePE = "HG";
            if (startRecord >= obsList.size()) {
                return bestObs;
            }

            /* try and find the pe specified for this location */
            RiverStat rsInfo = manager.getRiverStatus(obsList.get(startRecord)
                    .getLid());

            if (rsInfo != null) {
                usePE = rsInfo.getPe();
            }
        } else {
            /* explicit pe specified so use it */
            usePE = pcOptions.getSelectedAdHocElementString();
        }

        /*
         * loop thru the records and find the best type-source to use for the
         * physical element.
         */

        recordFound = false;

        tsRankingMap = manager.getIngestFilterData();

        for (int i = 0; i < obsList.size(); i++) {
            Observation o = obsList.get(i);
            if ((o == null) || (usePE == null)) {
                continue;
            }
            if (usePE.equals(obsList.get(i).getPe())) {
                /* now that we have a matching record, initialize the best ts */
                if (!recordFound) {
                    bestTS = obsList.get(i).getTs();
                    currentTS = obsList.get(i).getTs();
                    recordFound = true;
                }

                if (!obsList.get(i).getTs().equals(currentTS)) {
                    /*
                     * since there is more than one type-source, go get the
                     * ingest filter entries for this location for later use. do
                     * not filter the request by duration or extremum, since
                     * that is done via the original query, if at all
                     */

                    /*
                     * compare the ts just found with the best one and set it to
                     * the best ts if it has a higher rank
                     */

                    int result = compareTSRank(obsList.get(i).getTs(), bestTS,
                            tsRankingMap.get(obsList.get(i).getLid()), obsList
                                    .get(i).getPe());
                    if (result < 0) {
                        bestTS = obsList.get(i).getTs();
                    }

                    /* update for subsequent passes */
                    currentTS = obsList.get(i).getTs();
                }
            }
        }
        /* if the pe was not found, then return with nothing */
        if (!recordFound) {
            return bestObs;
        }

        /*
         * now at this point, we know what pe to use and what the best ts is in
         * the linked list. loop thru the list, considering only those that
         * match this pe and ts
         */

        double bestDiff = PDCConstants.MISSING_VALUE;

        for (int i = 0; i < obsList.size(); i++) {
            if (usePE.equals(obsList.get(i).getPe())
                    && bestTS.equals(obsList.get(i).getTs())) {
                if ((pcOptions.getTimeMode() == PDCConstants.TimeModeType.LATEST
                        .getTimeMode())
                        || (pcOptions.getTimeMode() == PDCConstants.TimeModeType.MAXSELECT
                                .getTimeMode())
                        || (pcOptions.getTimeMode() == PDCConstants.TimeModeType.MINSELECT
                                .getTimeMode())) {
                    /*
                     * for all but the SETTIME and VALUE_CHANGE modes, use the
                     * first value, which is the latest.
                     */
                    return obsList.get(i);
                } else if (pcOptions.getTimeMode() == PDCConstants.TimeModeType.SETTIME
                        .getTimeMode()) {
                    currentDiff = Math
                            .abs(pcOptions.getValidTime().getTime()
                                    - SimulatedTime.getSystemTime().getTime()
                                            .getTime());

                    /*
                     * Is the observation the closest to the center time so far?
                     */
                    if ((bestDiff == PDCConstants.MISSING_VALUE)
                            || (currentDiff <= bestDiff)) {
                        bestDiff = currentDiff;
                        bestObs = obsList.get(i);
                    }

                    if (bestDiff == 0) {
                        return bestObs;
                    }
                    break;
                } else if (pcOptions.getTimeMode() == PDCConstants.TimeModeType.VALUE_CHANGE
                        .getTimeMode()) {
                    /* Find the best observation in the upper time window. */
                    if (firstObsFound == 0) {
                        currentDiff = Math.abs(pcOptions.getValidTime()
                                .getTime()
                                - SimulatedTime.getSystemTime().getTime()
                                        .getTime());

                        /* Is the observation inside the window? */
                        if (currentDiff / (PDCConstants.MILLIS_PER_MINUTE * 60) <= changeWindow) {
                            /*
                             * Is the observation the closest to the center time
                             * so far?
                             */
                            if ((bestDiff == PDCConstants.MISSING_VALUE)
                                    || (currentDiff <= bestDiff)) {
                                bestDiff = currentDiff;
                                bestObs = heightList.get(i);
                            } else {
                                firstObsFound = 1;
                                bestDiff = PDCConstants.MISSING_VALUE;

                                /*
                                 * Retain the current observation so that it can
                                 * be tested against the lower time bound of the
                                 * change interval.
                                 */
                                --i;
                                continue;
                            }
                        } else {
                            if (bestDiff != PDCConstants.MISSING_VALUE) {
                                firstObsFound = 1;
                                bestDiff = PDCConstants.MISSING_VALUE;

                                /*
                                 * Retain the current observation so that it can
                                 * be tested against the lower time bound of the
                                 * change interval.
                                 */
                                --i;
                                continue;
                            }
                        }
                    } else if (lastObsFound == 0) {
                        currentDiff = Math.abs(pcOptions.getValidTime()
                                .getTime()
                                - SimulatedTime.getSystemTime().getTime()
                                        .getTime());

                        /* Find the best observation in the lower time window. */

                        /* Is the observation inside the window? */
                        if (currentDiff <= changeWindow) {
                            /*
                             * Is the observation the closest to the center time
                             * so far?
                             */
                            if ((bestDiff == PDCConstants.MISSING_VALUE)
                                    || (currentDiff <= bestDiff)) {
                                bestDiff = currentDiff;
                                bestObs2 = heightList.get(i);
                            } else {
                                lastObsFound = 1;
                            }
                        } else {
                            if (bestDiff != PDCConstants.MISSING_VALUE) {
                                lastObsFound = 1;
                            }
                        }
                    } else {
                        if (bestObs != null && bestObs2 != null) {
                            /*
                             * Both observations necessary to compute the change
                             * have been found.
                             */
                            change = bestObs.getValue() - bestObs2.getValue();
                        }
                        return bestObs;
                    }
                    break;
                }
            }
        }

        if (pcOptions.getTimeMode() == PDCConstants.TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            if ((firstObsFound == 1)
                    && (bestDiff != PDCConstants.MISSING_VALUE)) {
                change = bestObs.getValue() - bestObs2.getValue();
            }
        }

        return bestObs;
    }

    /**
     * To select the best RiverStatus report out of a block of RiverStatus
     * reports with duplicate lids. TS ranking is used to determine which report
     * is the best one.
     * 
     * @param useTs
     *            Type Source
     * @param rsList
     *            Riverstatus list
     * @param startRecord
     * @param pcOptions
     * @return
     */
    private Riverstatus deriveReportRS(String[] useTs,
            List<Riverstatus> rsList, int startRecord, PDCOptions pcOptions) {

        PDCDataManager manager = PDCDataManager.getInstance();
        Riverstatus best = null;
        String usePE = null;
        RiverStat rsInfo = null;
        boolean recordFound = false;
        String bestTs = null;
        String currTs = null;

        if (pcOptions.getPrimary() == 1) {
            /* default primary pe to HG */
            usePE = "HG";

            /* try and find the pe specified for this location */
            rsInfo = manager.getRiverStatus(riverStatusList.get(startRecord)
                    .getId().getLid());

            if (rsInfo != null) {
                if (rsInfo.getPe() != null) {
                    usePE = rsInfo.getPe();
                }
            }
        } else {
            /* explicit pe specified so use it */
            usePE = pcOptions.getSelectedAdHocElementString();
        }

        /*
         * loop thru the records and find the best type-source to use for the
         * physical element.
         */
        for (int i = 0; i < rsList.size(); i++) {
            /*
             * need to check that the pe matches and that the proper type of
             * type-source is found - i.e. either observed/processed or forecast
             */
            boolean match = false;
            if (usePE.equalsIgnoreCase(rsList.get(i).getId().getPe())) {
                for (int j = 0; j < useTs.length; j++) {
                    if (rsList.get(i).getId().getTs().contains(useTs[j])) {
                        match = true;
                        break;
                    }
                }
            }

            if (match) {
                tsRankingMap = manager.getIngestFilterData();

                /* now that we have a matching record, initialize the best ts */
                if (!recordFound) {
                    bestTs = rsList.get(i).getId().getTs();
                    currTs = rsList.get(i).getId().getTs();
                    recordFound = true;
                }

                if (!rsList.get(i).getId().getTs().equalsIgnoreCase(currTs)) {
                    /*
                     * since there is more than one type-source, go get the
                     * ingest filter entries for this location for later use. do
                     * not filter the request by duration or extremum, since
                     * that is done via the original query, if at all
                     */

                    int result = compareTSRank(rsList.get(i).getId().getTs(),
                            bestTs,
                            tsRankingMap.get(rsList.get(i).getId().getLid()),
                            rsList.get(i).getId().getPe());
                    if (result < 0) {
                        bestTs = rsList.get(i).getId().getTs();
                    }

                    /* update for subsequent passes */
                    currTs = rsList.get(i).getId().getTs();
                }
            }
        }

        /* if the pe was not found, then return with nothing */
        if (!recordFound) {
            return best;
        }

        /*
         * now at this point, we know what pe to use and what the best ts is in
         * the linked list. loop thru the list and get the value for the lid,pe,
         * ts.
         */

        for (int i = 0; i < rsList.size(); i++) {
            if (usePE.equalsIgnoreCase(rsList.get(i).getId().getPe())
                    && bestTs.equalsIgnoreCase(rsList.get(i).getId().getTs())) {
                /*
                 * since the RiverStatus key is lid, pe, ts, once we found a
                 * match, we can leave
                 */
                best = rsList.get(i);
                break;
            }
        }

        return best;
    }

    public List<GageData> deriveRainReports() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        ArrayList<GageData> reportList = new ArrayList<GageData>();
        long beginTime;
        long endTime;
        int precipSettings = PRECIP_PE_BEST | PRECIP_TS_BEST
                | REPORT_MISSING_BELOW_MIN_PERCENT;
        short endingTimeMatch = -1;

        List<Curpc> pcList = pdcManager.getPcList();
        List<Curpp> ppList = pdcManager.getPpList();

        /* if neither list has data, return now */
        if ((pcList == null) && (ppList == null)) {
            return null;
        }

        if (minDurTokenValue == null) {
            /* Minimum percentage of accum interval covered by precip data. */
            final String min_dur_token = "hv_min_dur_filled";

            AppsDefaults ad = AppsDefaults.getInstance();
            minDurTokenValue = ad.getToken(min_dur_token);
            if ((minDurTokenValue != null) && (minDurTokenValue.length() > 0)) {
                minPercent = Float.parseFloat(minDurTokenValue);
                if ((minPercent < 0.0) || (minPercent > 1.0)) {
                    minPercent = 0.0f;
                }
            }
        }

        /* set the time window accordingly */
        if (pcOptions.getTimeMode() == PDCConstants.TimeModeType.LATEST
                .getTimeMode()) {
            /* need to get a date set to the current hour */
            Date date = SimulatedTime.getSystemTime().getTime();
            Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            now.setTime(date);
            now.set(Calendar.MINUTE, 0);
            now.set(Calendar.SECOND, 0);

            beginTime = now.getTimeInMillis()
                    - (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR);
            endTime = now.getTimeInMillis();
        } else {
            beginTime = pcOptions.getValidTime().getTime()
                    - (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR);
            endTime = pcOptions.getValidTime().getTime()
                    + (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR);
        }

        ArrayList<Rawpc> rawPcList = new ArrayList<Rawpc>();
        ArrayList<Rawpp> rawPpList = new ArrayList<Rawpp>();
        if (pcList != null) {
            for (Curpc cp : pcList) {
                rawPcList.add(convertCurpc2Rawpc(cp));
            }
        }

        if (ppList != null) {
            for (Curpp pp : ppList) {
                rawPpList.add(convertCurpp2Rawpp(pp));
            }
        }

        Date beginDate = SimulatedTime.getSystemTime().getTime();
        beginDate.setTime(beginTime);
        Date endDate = SimulatedTime.getSystemTime().getTime();
        endDate.setTime(endTime);

        GetTotalPrecip gtp = new GetTotalPrecip();
        reportList = gtp.getTotalRawPrecip(rawPcList, rawPpList, beginDate,
                endDate, endingTimeMatch, minPercent, precipSettings);

        return reportList;
    }

    /**
     * Converts a Curpc object to a Rawpc object.
     * 
     * @param cpc
     *            The Curpc object to convert
     * @return The converted Rawpc object
     */
    private Rawpc convertCurpc2Rawpc(Curpc cpc) {
        Rawpc rpc = new Rawpc();
        RawpcId rawId = new RawpcId();
        CurpcId curId = cpc.getId();

        rawId.setExtremum(curId.getExtremum());
        rawId.setLid(curId.getLid());
        rawId.setObstime(curId.getObstime());
        rawId.setTs(curId.getTs());
        rpc.setId(rawId);

        rpc.setDur(cpc.getDur());
        rpc.setPe(cpc.getPe());
        rpc.setPostingtime(cpc.getPostingtime());
        rpc.setProductId(cpc.getProductId());
        rpc.setProducttime(cpc.getProducttime());
        rpc.setQualityCode(cpc.getQualityCode());
        rpc.setRevision(cpc.getRevision());
        rpc.setShefQualCode(cpc.getShefQualCode());
        rpc.setValue(cpc.getValue());

        return rpc;
    }

    /**
     * Converts a Curpp object to a Rawpp object.
     * 
     * @param cpp
     *            The Curpp object to convert
     * @return The converted Rawpp object
     */
    private Rawpp convertCurpp2Rawpp(Curpp cpp) {
        Rawpp rpp = new Rawpp();

        RawppId rawId = new RawppId();
        CurppId curId = cpp.getId();

        rawId.setExtremum(curId.getExtremum());
        rawId.setLid(curId.getLid());
        rawId.setObstime(curId.getObstime());
        rawId.setTs(curId.getTs());
        rawId.setDur(curId.getDur());

        rpp.setId(rawId);

        rpp.setPe(cpp.getPe());
        rpp.setPostingtime(cpp.getPostingtime());
        rpp.setProductId(cpp.getProductId());
        rpp.setProducttime(cpp.getProducttime());
        rpp.setQualityCode(cpp.getQualityCode());
        rpp.setRevision(cpp.getRevision());
        rpp.setShefQualCode(cpp.getShefQualCode());
        rpp.setValue(cpp.getValue());
        return rpp;
    }

    /**
     * @return the heightList
     */
    public List<Observation> getHeightList() {
        return heightList;
    }

    /**
     * @param heightList
     *            the heightList to set
     */
    public void setHeightList(List<Observation> heightList) {
        this.heightList = heightList;
    }

    /**
     * @return the qList
     */
    public List<Observation> getQList() {
        return qList;
    }

    /**
     * @param list
     *            the qList to set
     */
    public void setQList(List<Observation> list) {
        qList = list;
    }

    /**
     * @return the riverStatusList
     */
    public List<Riverstatus> getRiverStatusList() {
        return riverStatusList;
    }

    /**
     * @param riverStatusList
     *            the riverStatusList to set
     */
    public void setRiverStatusList(List<Riverstatus> riverStatusList) {
        this.riverStatusList = riverStatusList;
    }
}