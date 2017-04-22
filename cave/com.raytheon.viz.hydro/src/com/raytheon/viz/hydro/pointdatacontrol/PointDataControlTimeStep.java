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
package com.raytheon.viz.hydro.pointdatacontrol;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.PrecipPeFilter;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.ProcessLineMode;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.RiverStationFilter;
import com.raytheon.viz.hydro.pointdatacontrol.data.PDCFileInfo;
import com.raytheon.viz.hydro.pointdatacontrol.data.StationEntryDetails;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlFilter;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlRiverStatus;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Point Data Control Time Step
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2009             mpduff       Initial creation
 * Jan 25, 2011  #7907     bkowal       Added a new method that would only
 *                                      filter values that had been previously
 *                                      retrieved.
 * Aug 03, 2016  4623      skorolev     Cleanup.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class PointDataControlTimeStep {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataControlTimeStep.class);

    private static final String PDC_DATA_DIR = "pdc_pp_dir";

    private static final int MAX_VALUE_COUNT = (24 * 60);

    private PointDataControlManager pdcManager = PointDataControlManager
            .getInstance();

    private StationEntryDetails stationEntryDetails = null;

    private List<GageData> reportList = new ArrayList<>();

    /**
     * Update the preprocessed data cache.
     */
    public List<GageData> updateData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String dataDir = getDataDirectory();

        try {
            readFile(dataDir);
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "PointDataControlTimeStep.updateData():  Unable to read file "
                            + dataDir, e);

        }

        PDCFileInfo fileInfo = PDCFileInfo.getInstance();

        if (!fileInfo.isSuccess()) {
            statusHandler.handle(Priority.ERROR,
                    "PointDataControlTimeStep.updateData():  Unable to read file "
                            + dataDir);
            return null;
        }

        /*
         * This occurs when the PDC gui has not been called and the primary
         * preset is of time step mode.
         */
        if ((pcOptions.getPcTimeStr() == null)
                || (pcOptions.getPcTimeStr().length() < 1)) {
            Date validTime = pcOptions.getValidTime();
            pcOptions.setPcTimeStr(PDCConstants.DATE_FORMAT.format(validTime));
        }

        boolean inInstantPrecipMode = false;
        int instantPrecipSelection = -1;
        if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.INSTANTANEOUS_PRECIP_TSDE
                .getElementType()) {
            instantPrecipSelection = pcOptions
                    .getInstPrecipAccumTimeSelection();
            inInstantPrecipMode = true;
        }

        String timeString = pcOptions.getPcTimeStr();
        Date validTime;
        try {
            validTime = PDCConstants.DATE_FORMAT.parse(timeString);

            reportList = createReportList(fileInfo, validTime,
                    inInstantPrecipMode, instantPrecipSelection, reportList);

            reportList = PointDataControlFilter
                    .filterReportsAndAddInfo(reportList);

            reportList = filterRiverStationReports(reportList);

            reportList = filterByPrecipPe(reportList);

            reportList = PointDataControlRiverStatus.processRiverThreatIndex(
                    reportList, true);

        } catch (ParseException e) {
            statusHandler.handle(Priority.ERROR, "Error parsing timeString "
                    + timeString, e);
        }

        return reportList;
    }

    /**
     * Filter data
     * 
     * @return
     */
    public List<GageData> onlyFilterData() {
        reportList = PointDataControlFilter.filterReportsAndAddInfo(reportList);

        return this.reportList;
    }

    /**
     * Get's the PDC preprocessed data directory.
     * 
     * @return the directory path
     */
    private String getDataDirectory() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        StringBuilder path = new StringBuilder();

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        path.append(appsDefaults.getToken(PDC_DATA_DIR));

        int element = pcOptions.getTsDataElement();

        /* If the selected element is changed then reload the colorMap */
        if (pdcManager.getSelectedTimeStepElement() != element) {
            if (pdcManager.getColorMap() != null) {
                pdcManager.getColorMap().setChanged(true);
            }
        }
        pdcManager.setSelectedTimeStepElement(element);

        path.append("/");
        path.append(PDCConstants.TIME_STEP_FILE_NAME_ARRAY[element]);

        return path.toString();
    }

    /**
     * Read in the preprocessed files.
     * 
     * @param path
     *            The location of the files
     */
    private void readFile(String path) throws IOException {
        PDCFileInfo fileInfo = PDCFileInfo.getInstance();
        int valueCount = 0;
        Calendar startTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        int incrTime = 0;
        int headerLineCnt = 0;
        String dateString = null;
        String timeString = null;
        ProcessLineMode processLineMode = null;

        // Reset the data storage
        pdcManager.resetStationEntryDetailsMap();

        try (BufferedReader in = new BufferedReader(new FileReader(path))) {
            String line;
            while (((line = in.readLine()) != null)) {
                headerLineCnt++;
                if (headerLineCnt < 7) {
                    if (headerLineCnt == 1) {
                        String[] parts = line.split(" ");
                        // Get the last two elements
                        dateString = parts[parts.length - 2];
                        timeString = parts[parts.length - 1];

                        // Store this for later use
                        try {
                            fileInfo.setTimestepFileCreationTime(PDCConstants.DATE_FORMAT
                                    .parse(dateString + " " + timeString));
                        } catch (ParseException e) {
                            statusHandler.handle(Priority.ERROR,
                                    "Error parsing timeString " + dateString
                                            + " " + timeString, e);
                        }
                        continue;
                    } else if (headerLineCnt < 7) {
                        /* Skip the rest of the first 6 lines (2..6) */
                        continue;
                    }
                } else {
                    if (headerLineCnt == 7) {
                        headerLineCnt++;
                        String[] parts = line.split(" ");
                        try {
                            valueCount = Integer.parseInt(parts[0]);
                            // This value seems a bit low, a few days in the
                            // past
                            startTime.setTimeInMillis(Long.parseLong(parts[1])
                                    * PDCConstants.MILLIS_PER_SECOND);
                            incrTime = Integer.parseInt(parts[2])
                                    * PDCConstants.MILLIS_PER_SECOND;

                            fileInfo.setStartTime(startTime);
                            fileInfo.setIncrementTime(incrTime);
                            fileInfo.setValueCount(valueCount);
                        } catch (NumberFormatException e) {
                            statusHandler.handle(Priority.ERROR,
                                    "Error parsing file info: " + line, e);
                        }
                    } else {
                        if (line.length() > 0) {
                            stationEntryDetails = new StationEntryDetails();
                            processLineMode = ProcessLineMode.PROCESS_NEW_STATION;
                            stationEntryDetails = processLine(line, valueCount,
                                    processLineMode, startTime, incrTime,
                                    stationEntryDetails);

                            processLineMode = ProcessLineMode.PROCESS_DATA_SOURCES;
                            if ((line = in.readLine()) != null) {
                                stationEntryDetails = processLine(line,
                                        valueCount, processLineMode, startTime,
                                        incrTime, stationEntryDetails);
                            }

                            processLineMode = ProcessLineMode.PROCESS_VALUES;
                            if ((line = in.readLine()) != null) {
                                stationEntryDetails = processLine(line,
                                        valueCount, processLineMode, startTime,
                                        incrTime, stationEntryDetails);
                            }
                        }
                    }
                }

            }

            fileInfo.setEntryCount(pdcManager.getStationEntryMap().size());
            fileInfo.setSuccess(true);

        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR, "Error open file " + path, e);
        }
    }

    /**
     * Process Line
     * 
     * @param line
     * @param valueCount
     * @param processLineMode
     * @param startTime
     * @param incrTime
     * @param station
     * @return
     */
    private StationEntryDetails processLine(String line, int valueCount,
            ProcessLineMode processLineMode, Calendar startTime, int incrTime,
            StationEntryDetails station) {

        if (processLineMode == ProcessLineMode.PROCESS_NEW_STATION) {
            station = loadStationInfo(line, station);
        } else if (processLineMode == ProcessLineMode.PROCESS_DATA_SOURCES) {
            station = loadDataSourceInfo(line, station);
        } else if (processLineMode == ProcessLineMode.PROCESS_VALUES) {
            station = loadValuesInfo(line, station, valueCount, startTime,
                    incrTime);
            pdcManager.addStationEntry(station);
        } else {
            statusHandler.handle(Priority.DEBUG,
                    "process_line():  Invalid ProcessLineMode = "
                            + processLineMode);
        }

        return station;
    }

    /**
     * Load the station information into the StationEntryDetails object.
     * 
     * @param line
     *            The line to parse and load
     * @param station
     *            The StationEntryDetails object to load
     */
    private StationEntryDetails loadStationInfo(String line,
            StationEntryDetails station) {
        /* Split the line to get the details */
        String[] parts = line.split(" ", 8);

        if (parts.length == 8) {

            /* Populate the object */
            station.setLid(parts[0]);
            station.setHsa(parts[1]);

            String pedtse = parts[2];
            station.setPe(pedtse.substring(0, 2));
            station.setShefDur(pedtse.substring(2, 3));
            station.setTs(pedtse.substring(3, 5));
            station.setEx(pedtse.substring(5));

            station.setElevation(Double.parseDouble(parts[3]));
            station.setLat(Double.parseDouble(parts[4]));
            station.setLon(Double.parseDouble(parts[5]));

            station.setFcstPt(parts[6]);

        }

        return station;
    }

    /**
     * Load the data source information into the StationEntryDetails object.
     * 
     * @param line
     *            The line to parse and load
     * @param station
     *            The StationEntryDetails object to load
     */
    private StationEntryDetails loadDataSourceInfo(String line,
            StationEntryDetails station) {
        String[] parts = line.split(" ");
        station.setDcp(parts[0]);
        station.setObserver(parts[1]);
        station.setTelemType(parts[2]);

        return station;
    }

    /**
     * Loads ValuesInfo
     * 
     * @param line
     * @param station
     * @param valueCount
     * @param startTime
     * @param incrTime
     * @return
     */
    private StationEntryDetails loadValuesInfo(String line,
            StationEntryDetails station, int valueCount, Calendar startTime,
            int incrTime) {

        int count = 0;

        station.setStartTime(startTime);
        station.setIncrTime(incrTime);

        if (valueCount > MAX_VALUE_COUNT) {
            valueCount = MAX_VALUE_COUNT;
        }

        station.setValueCount(valueCount);

        /* if the entire time series is declared to be missing */
        if (line.startsWith("MISSING")) {
            for (int i = 0; i < valueCount; i++) {
                station.addValue(PDCConstants.MISSING_VALUE);
            }
        } else {
            /* There is actual data in the time series */
            String[] values = line.split(" ");

            while (count < valueCount) {
                station.addValue(Double.parseDouble(values[count]));
                count++;
            }
        }

        return station;
    }

    /**
     * Create the report list.
     * 
     * @param fileInfo
     *            PDCFileInfo Object
     * @param validReportTime
     *            Valid report time Date object
     * @param inInstantPrecipMode
     *            true if in Instantaneous precip mode
     * @param instantPrecipSelection
     *            Instantaneous precip selection
     */
    private List<GageData> createReportList(PDCFileInfo fileInfo,
            Date validReportTime, boolean inInstantPrecipMode,
            int instantPrecipSelection, List<GageData> reportList) {
        final String method = "createReportList()";
        int numValues = fileInfo.getValueCount();
        Calendar startTime = fileInfo.getStartTime();
        int incrTime = fileInfo.getIncrementTime();
        int usefulLoopIndex = -1;
        StationEntryDetails station = null;

        int timeDiff = (int) (validReportTime.getTime() - startTime
                .getTimeInMillis());

        if (incrTime > 0) {
            if (inInstantPrecipMode) {
                usefulLoopIndex = getInstPrecipArrayIndex(instantPrecipSelection);
            } else {
                usefulLoopIndex = timeDiff / incrTime;
            }

            if (!((usefulLoopIndex >= 0) && (usefulLoopIndex < numValues))) {
                usefulLoopIndex = -1;
                statusHandler.handle(
                        Priority.DEBUG,
                        "ERROR in "
                                + method
                                + " problem with startTime = "
                                + PDCConstants.DATE_FORMAT.format(startTime
                                        .getTime())
                                + " Check roundness.  validReportTime = :"
                                + PDCConstants.DATE_FORMAT
                                        .format(validReportTime)
                                + ": startTime = :"
                                + PDCConstants.DATE_FORMAT.format(startTime
                                        .getTime()));
            }
        }

        if (usefulLoopIndex == -1) {
            statusHandler.handle(Priority.INFO, method + " usefulLoopIndex = "
                    + usefulLoopIndex + " numValues = " + numValues);
        }

        /* for each station entry, (there can be more than 1 entry per lid) */
        Map<String, StationEntryDetails> stationMap = pdcManager
                .getStationEntryMap();
        Set<String> stationSet = stationMap.keySet();

        Iterator<String> iter = stationSet.iterator();

        while (iter.hasNext()) {
            station = stationMap.get(iter.next());

            reportList = copyStationDetailsToReportList(station,
                    validReportTime, usefulLoopIndex, reportList);
        }

        return reportList;
    }

    /**
     * Get the Instantaneous Precipitation Array Index.
     * 
     * @param selection
     *            The selection
     * @return the Index
     */
    private int getInstPrecipArrayIndex(int selection) {

        int index = -1;

        if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_30_MINUTES
                .getInstPrecipSelection()) {
            index = 24;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_1_HOUR
                .getInstPrecipSelection()) {
            index = 23;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_2_HOURS
                .getInstPrecipSelection()) {
            index = 22;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_3_HOURS
                .getInstPrecipSelection()) {
            index = 21;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_4_HOURS
                .getInstPrecipSelection()) {
            index = 20;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_6_HOURS
                .getInstPrecipSelection()) {
            index = 18;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_12_HOURS
                .getInstPrecipSelection()) {
            index = 12;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_18_HOURS
                .getInstPrecipSelection()) {
            index = 6;
        } else if (selection == HydroConstants.InstPrecipSelection.PRECIP_TIME_24_HOURS
                .getInstPrecipSelection()) {
            index = 0;
        }

        return index;
    }

    /**
     * Copy the data into the reportList.
     * 
     * @param station
     * @param validTime
     * @param valueIndex
     */
    private List<GageData> copyStationDetailsToReportList(
            StationEntryDetails station, Date validTime, int valueIndex,
            List<GageData> reportList) {
        GageData gd = new GageData();

        gd.setLid(station.getLid());
        gd.setPe(station.getPe());
        gd.setTs(station.getTs());
        gd.setExtremum(station.getEx());
        gd.setProbability(1.0);
        gd.setDur(ParameterCode.Duration.getEnum(station.getShefDur())
                .getValue());
        gd.setShefQualCode("Q");
        gd.setQuality_code(1);
        gd.setName(station.getName());
        gd.setUse(true);

        if ((valueIndex >= 0) && (valueIndex < station.getValueCount())) {
            gd.setValue(station.getValueArray().get(valueIndex));
            gd.setValue2(station.getValueArray().get(valueIndex));
        } else {
            gd.setValue(PDCConstants.MISSING_VALUE);
            gd.setValue2(PDCConstants.MISSING_VALUE);
        }

        gd.setValidtime(validTime);
        gd.setBasistime(validTime);

        gd.setThreatIndex(ThreatIndex.THREAT_MISSING_DATA); // Note: had been
        // set to 'R', literally
        gd.setLat(station.getLat());
        gd.setLon(station.getLon() * -1);
        // Note: had been set to "F", literally
        gd.setDispClass("F");

        reportList.add(gd);

        return reportList;
    }

    /**
     * Filters River Station Reports
     * 
     * @param reportListArg
     * @return
     */
    private List<GageData> filterRiverStationReports(
            List<GageData> reportListArg) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /* We only want to filter in Time Step mode with a River element type */
        if (pcOptions.getElementType() != HydroConstants.TimeStepDataElementType.RIVER_TIME_STEP_TYPE
                .getTimeStepDataElementType()) {
            return reportListArg;
        }

        int riverStationFilter = pcOptions.getRiverStationFilter();

        if (riverStationFilter == RiverStationFilter.ALL_STATIONS_RSF
                .getRiverStationFilter()) {
            // There is no filtering performed, so bail
            return reportListArg;
        }

        for (GageData gd : reportListArg) {
            boolean isReservoir = false;

            if (gd.getDispClass().toUpperCase().contains("D")) {
                isReservoir = true;
            }

            if (pcOptions.getRiverStationFilter() == RiverStationFilter.STREAM_STATIONS_RSF
                    .getRiverStationFilter()) {
                if (isReservoir) {
                    gd.setUse(false);
                }
            } else { // RESERVOIR_STATIONS_RSF
                if (!isReservoir) {
                    gd.setUse(false);
                }
            }

        }

        return reportListArg;
    }

    /**
     * Filters By PrecipPe
     * 
     * @param reportListArg
     * @return
     */
    private List<GageData> filterByPrecipPe(List<GageData> reportListArg) {
        /*
         * Note: this sort of filter routine- filters OUT, it never sets
         * rPtr->use to 1, since other filters may set it to 0, and hence filter
         * out the station
         */

        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * we only want to filter in time step mode with a RAIN_TIME_STEP_TYPE
         * that is not INSTANTANEOUS
         */
        if (pcOptions.getElementType() != HydroConstants.TimeStepDataElementType.RAIN_TIME_STEP_TYPE
                .getTimeStepDataElementType()) {
            return reportListArg;
        }

        if (pcOptions.getTsDataElement() == HydroConstants.TimeStepDataElement.INSTANTANEOUS_PRECIP_TSDE
                .getElementType()) {
            return reportListArg;
        }

        int precipPeFilter = pcOptions.getPrecipPeFilter();

        if (precipPeFilter == PrecipPeFilter.PC_AND_PP_PPF.getFilterType()) {
            /* There is no filtering performed, so bail */
            return reportListArg;
        }

        for (GageData gd : reportListArg) {
            if (precipPeFilter == PrecipPeFilter.PC_ONLY_PPF.getFilterType()) {
                if (gd.getPe().equalsIgnoreCase("PP")) {
                    gd.setUse(false);
                }
            } else if (precipPeFilter == PrecipPeFilter.PP_ONLY_PPF
                    .getFilterType()) {
                if (gd.getPe().equalsIgnoreCase("PC")) {
                    gd.setUse(false);
                }
            }
        }

        return reportListArg;
    }
}
