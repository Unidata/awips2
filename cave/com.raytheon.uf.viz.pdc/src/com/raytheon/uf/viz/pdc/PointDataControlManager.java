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
package com.raytheon.uf.viz.pdc;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.pdc.data.PDCLocationShift;
import com.raytheon.uf.viz.pdc.data.PointControlPeTs;
import com.raytheon.uf.viz.pdc.data.StationEntryDetails;
import com.raytheon.uf.viz.pdc.engine.PDCFindBasis;
import com.raytheon.uf.viz.pdc.engine.PointControlLocationShift;
import com.raytheon.uf.viz.pdc.engine.PointDataControlAddMissing;
import com.raytheon.uf.viz.pdc.engine.PointDataControlDerive;
import com.raytheon.uf.viz.pdc.engine.PointDataControlFilter;
import com.raytheon.uf.viz.pdc.engine.PointDataControlRiverStatus;
import com.raytheon.uf.viz.pdc.engine.PointDataControlValue;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.RainData;
import com.raytheon.viz.hydrocommon.data.RiverData;
import com.raytheon.viz.hydrocommon.datamanager.PDCDataManager;
import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;
import com.raytheon.viz.hydrocommon.listeners.StationDisplayListener;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants.TimeModeType;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.pdc.PDCRiverOptions;

/**
 * PDC Manager.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Oct 26, 2008           mpduff     Initial creation
 * Jan 25, 2011  7907     bkowal     Added private variables to cache
 *                                   information so that it would not need to be
 *                                   retrieved every time.
 * Jan 27, 2011  5274     bkowal     Using the swt job class to request data
 *                                   asynchronously and display a progress
 *                                   indicator on the main interface.
 * Feb 11, 2014  15829    lbousaidi  check for Missing before processing River
 *                                   Threat.
 * Oct 05, 2015  17978    lbousaidi  updated addStationEntry() to use
 *                                   StationEntryDetails.getPeDTsE()
 * May 23, 2016  5590     bkowal     {@link Observation} relocated to common.
 * Jan 09, 2017  19647    xwei       Fixed the "Ad Hoc Request Failed" error
 * Mar 08, 2017  19647    snaples    Fixed Class Cast exception when returning
 *                                   List to Arraylist.
 * Jun 27, 2017  5770     bkowal     Fix merge issues.
 * Apr 16, 2018  6630     randerso   Code cleanup.
 * Sep 21, 2018  7379     mduff      Moved and code cleanup for PDC Refactor.
 * Apr 30, 2019  7832     randerso   Fix update of display for "other" data types
 *
 * </pre>
 *
 * @author mpduff
 */

public class PointDataControlManager extends Job {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataControlManager.class);

    private static PointDataControlManager instance = null;

    private List<String> responsibleHsa = new ArrayList<>();

    private PDCDataManager dataManager = null;

    private HydroDisplayManager hvManager = HydroDisplayManager.getInstance();

    private int refreshMinutes = PDCConstants.REFRESH_MINUTES;

    private boolean firstGad = true;

    private Date previousRetrievalDate = SimulatedTime.getSystemTime()
            .getTime();

    private boolean redraw = false;

    private boolean dataRetrievalRequired = false;

    private List<Observation> obsHeightList = new ArrayList<>();

    private List<Observation> obsDischargeList = new ArrayList<>();

    private List<Observation> observationList = new ArrayList<>();

    private List<Riverstatus> riverStatusList = new ArrayList<>();

    private List<Curpc> pcList = null;

    private List<Curpp> ppList = null;

    private List<GageData> fcstReportList = null;

    private List<GageData> obs2ReportList = null;

    private List<GageData> fcst2ReportList = null;

    private boolean updateFlag = false;

    private List<StationDisplayListener> stationListenerList = new ArrayList<>();

    private List<PDCLocationShift> shiftList = null;

    private PointDataControlDlg pdcDialog = null;

    private Map<String, StationEntryDetails> stationEntryMap = new HashMap<>();

    private String hydroDataInfo = null;

    private int selectedTimeStepElement = 0;

    private List<GageData> repList = null;

    private PointDataControlTimeStep pdcTimeStep = null;

    /* For The Jobs That Will Be Running */
    public static enum REQUEST_TYPE {
        REQUEST_AD_HOC, REQUEST_TIME_STEP
    }

    private boolean updateData;

    private REQUEST_TYPE requestType;

    private boolean cancelJob = false;

    private PointDataControlManager() {
        super("");
    }

    public static synchronized PointDataControlManager getInstance() {
        if (instance == null) {
            instance = new PointDataControlManager();
            instance.dataManager = PDCDataManager.getInstance();
        }

        return instance;
    }

    public List<String> getServiceBackupInfo() {
        return responsibleHsa;
    }

    public void setServiceBackupInfo(List<String> hsa) {
        responsibleHsa = hsa;
    }

    public PointControlPeTs getPCPeTsData() {
        return PointControlPeTs.getInstance();
    }

    public boolean getRedraw() {
        return redraw;
    }

    public void setRedraw(boolean redrawMap) {
        redraw = redrawMap;
        if (redraw) {
            hvManager.fireMapDrawEvent();
        }
    }

    private void processAdhocRequest(boolean updateData) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * check if the time since the the last retrieval is greater than the
         * limit defined by a token. if so, then re-retrieve the data regardless
         * of the retrieval_required setting
         */
        Date now = SimulatedTime.getSystemTime().getTime();
        if (!updateData) {
            if (firstGad) {
                int refreshMinutes = AppsDefaults.getInstance().getInt(
                        PDCConstants.HV_REFRESH_MINUTES,
                        PDCConstants.REFRESH_MINUTES);
                String minuteString = String.valueOf(refreshMinutes);
                if ((minuteString != null) && (minuteString.length() > 0)) {
                    refreshMinutes = Integer.parseInt(minuteString);
                }
                firstGad = false;
            }
            long nowMillis = now.getTime();
            long prevMillis = previousRetrievalDate.getTime();
            if (nowMillis - prevMillis > refreshMinutes
                    * PDCConstants.MILLIS_PER_MINUTE) {
                updateData = true;
            }
        } else {
            previousRetrievalDate = now;
        }

        if (this.cancelJob) {
            return;
        }
        /*
         * Indicate to the outside world that the point data is being updated.
         */
        setUpdateFlag(true);

        if (updateData) {
            PointDataControlDerive derive = new PointDataControlDerive();
            this.repList = new ArrayList<>();
            this.obs2ReportList = null;
            this.fcstReportList = null;
            /*
             * get the RAW data, then derive the DERIVED data based on the user
             * instructions.
             */

            PointDataControlManager pdcManager = PointDataControlManager
                    .getInstance();
            pdcManager.setObsHeightList(null);
            pdcManager.setObsDischargeList(null);

            List<GageData> obsReportList;
            switch (pcOptions.getElementType()) {
            case 0: // RIVER_AD_HOC_TYPE
                /*
                 * get the data from the appropriate data set, whether it be
                 * from the height, discharge, or riverstatus table. then
                 * process the data to get a single value for each location; one
                 * list is for observed data and one is for forecast data
                 */
                RiverData riverData = dataManager.getRiverData(pcOptions);
                this.obsDischargeList = riverData.getObsDischargeList();
                this.observationList = riverData.getObservationList();
                this.obsHeightList = riverData.getObsHeightList();
                this.riverStatusList = riverData.getRiverStatusList();
                derive.deriveRiverReports(false, pcOptions);
                break;

            case 1: // RAIN_AD_HOC_TYPE
                /*
                 * Do not process rain for the value change option. Showing a
                 * change in precipitation amount over a period of time
                 * generally does not make sense.
                 */
                if (pcOptions.getTimeMode() != 4) { // VALUE_CHANGE
                    /*
                     * any "rain" data other than PC, PP, or both, is treated
                     * like generic data. then process the data to get a single
                     * value for each location
                     */
                    if ((pcOptions.getPcAndpp() == 0)
                            && !"PC".equalsIgnoreCase(
                                    pcOptions.getSelectedAdHocElementString())
                            && !"PP".equalsIgnoreCase(pcOptions
                                    .getSelectedAdHocElementString())) {
                        this.observationList = dataManager.getSnowTempOtherData();

                        obsReportList = derive.deriveOtherReports();
                        hvManager.setObsReportList(obsReportList);
                    } else {
                        /*
                         * get PC and/or PP data. then process the data to get a
                         * single value for each location
                         */
                        RainData rainData = dataManager.getRainData();
                        this.pcList = rainData.getPcList();
                        this.ppList = rainData.getPpList();
                        obsReportList = derive.deriveRainReports();
                        hvManager.setObsReportList(obsReportList);
                    }
                }
                break;

            case 2: // SNOW_ADHOC_TYPE
            case 3: // TEMP_ADHOC_TYPE
            default:
                /*
                 * get snow, temperature or other data, including processed
                 * data, in a general fashion. then process the data to get a
                 * single value for each location
                 */

                this.observationList = dataManager.getSnowTempOtherData();

                obsReportList = derive.deriveOtherReports();
                hvManager.setObsReportList(obsReportList);

                break;
            }

            if (this.cancelJob) {
                return;
            }
            /*
             * if getting the latest River data, then the stage basis selected
             * by the user must be applied. if getting the max of the obs or
             * forecast, then loop thru the observed and forecast lists and
             * assign the appropriate value. otherwise simply copy the list.
             */

            if ((pcOptions
                    .getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                            .getAdHocDataElementType())
                    && (pcOptions.getTimeMode() == TimeModeType.LATEST
                            .getTimeMode())) {
                if (pcOptions.getStageBasis() == 0) {
                    // BASIS_OBS
                    repList = PDCFindBasis
                            .copyReportList(hvManager.getObsReportList());
                } else if (pcOptions.getStageBasis() == 1) {
                    // Forecast
                    repList = PDCFindBasis.copyReportList(fcstReportList);
                } else {
                    // Both obs and fcst
                    repList = PDCFindBasis.determineRsMofo(getObsReportList(),
                            getFcstReportList());
                }
            } else {
                repList = PDCFindBasis
                        .copyReportList(hvManager.getObsReportList());
            }

            if (this.cancelJob) {
                return;
            }

            /*
             * Note that copy_replist must set the new elements for stage,
             * discharge, and departure to missing. These elements are being
             * included so that they don't have to be recalculated each time a
             * request is made for which data retrieval is not required. That
             * would be time consuming.
             */

            /*
             * if the user has requested that a missing report be shown for each
             * station being considered, then add them as necessary. the report
             * list is passed in AND returned to handle the case where the
             * report list may be empty, and therefore this function must assign
             * a new Head pointer.
             *
             * missing data are only added if requested, since it takes a finite
             * time to determine which stations are missing.
             */
            if (pcOptions.getSupressMissing() == 1) {
                repList = PointDataControlAddMissing.addMissingReports(repList);
            }

            if (this.cancelJob) {
                return;
            }

            /*
             * Check to determine if the user wants the displayed river icons to
             * be colored according to their river status value. This is done by
             * checking the toggle button corresponding to the display of
             * riverstatus information on the point data control gui.
             */
            if (pcOptions.getRiverStatus() == 1) {
                repList = PointDataControlRiverStatus.processRiverThreatIndex(
                        repList, dataRetrievalRequired);
            }

            if (this.cancelJob) {
                return;
            }

            /*
             * Check the "Values" pulldown option menu. Process the options
             * according to the selected value which may be one of the
             * following: Value, Value / Flood Level, Value / Derived Stage (or
             * Derived Flow), Flood Departure, Flood Departure / Flood Level
             *
             * Only do this for river PE's or the PRIMARY derived type.
             */
            repList = PointDataControlValue.processValueOption(repList,
                    dataRetrievalRequired);
        }

        if (this.cancelJob) {
            return;
        }
        /*
         * Filter the data as needed. This should be the last step in the
         * process of preparing the point data to be displayed. This routine
         * also adds key information to the reports such as the latitude and
         * longitude of the station, its display class, its name, and its stream
         * name.
         */
        repList = PointDataControlFilter.filterReportsAndAddInfo(repList);

        if (this.cancelJob) {
            return;
        }

        hvManager.setDrawStation(true);

        hvManager.setObsReportList(repList);
        setObsReportList(repList);

        if (hvManager.isDrawStation()) {
            PointDataControlManager.getInstance().fireUpdateEvent(null);
        }

    }

    public List<GageData> pcProcessOnetime(PDCRiverOptions pcOptionsOnetime) {
        final boolean oneTime = true;
        List<GageData> repList = null;
        obs2ReportList = null;
        fcst2ReportList = null;
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * get the RAW data, then derive the DERIVED data based on the user
         * instructions.
         */
        PointDataControlDerive derive = new PointDataControlDerive();
        if (pcOptionsOnetime
                .getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                        .getAdHocDataElementType()) {
            /*
             * get the data from the appropriate data set, whether it be from
             * the height, discharge, or riverstatus table. then process the
             * data to get a single value for each location; one list is for
             * observed data and one is for forecast data
             */
            RiverData riverData = dataManager.getRiverData(pcOptionsOnetime);
            this.obsHeightList = riverData.getObsHeightList();
            this.obsDischargeList = riverData.getObsDischargeList();
            this.observationList = riverData.getObservationList();
            this.riverStatusList = riverData.getRiverStatusList();

            derive.deriveRiverReports(oneTime, pcOptionsOnetime);
        } else if (pcOptionsOnetime
                .getElementType() == HydroConstants.AdHocDataElementType.RAIN_AD_HOC_TYPE
                        .getAdHocDataElementType()) {
            /*
             * any "rain" data other than PC, PP, or both, is treated like
             * generic data. then process the data to get a single value for
             * each location
             */

            if ((pcOptions.getPcAndpp() != 1)
                    && !"PC".equalsIgnoreCase(
                            pcOptions.getSelectedAdHocElementString())
                    && !"PP".equalsIgnoreCase(
                            pcOptions.getSelectedAdHocElementString())) {
                List<Observation> obsData = dataManager.getSnowTempOtherData();
                this.setObservationList(obsData);
                hvManager.setObsReportList(derive.deriveOtherReports());
            } else {
                /*
                 * get PC and/or PP data. then process the data to get a single
                 * value for each location
                 */
                RainData rainData = dataManager.getRainData();
                this.pcList = rainData.getPcList();
                this.ppList = rainData.getPpList();

                hvManager.setObsReportList(derive.deriveRainReports());
            }
        } else {
            /*
             * get snow, temperature or other data, including processed data, in
             * a general fashion. then process the data to get a single value
             * for each location
             */

            List<Observation> obsData = dataManager.getSnowTempOtherData();
            this.setObservationList(obsData);
            hvManager.setObsReportList(derive.deriveOtherReports());
        }

        /*
         * if getting the latest River data, then the stage basis selected by
         * the user must be applied. if getting the max of the obs or forecast,
         * then loop thru the observed and forecast lists and assign the
         * appropriate value. otherwise simply copy the list.
         */

        if ((pcOptionsOnetime
                .getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                        .getAdHocDataElementType())
                && (pcOptionsOnetime.getTimeMode() == TimeModeType.LATEST
                        .getTimeMode())) {
            if ((pcOptionsOnetime.getStageBasis() == 0)
                    && (hvManager.getObsReportList() != null)) { // BASIS_OBS
                repList = PDCFindBasis.copyReportList(obs2ReportList);
            } else if ((pcOptionsOnetime.getStageBasis() == 1)
                    && (fcstReportList != null)) { // BASIS_FCST
                repList = PDCFindBasis.copyReportList(fcst2ReportList);
            } else {
                repList = PDCFindBasis.determineRsMofo(obs2ReportList,
                        fcst2ReportList);
            }
        } else {
            repList = PDCFindBasis.copyReportList(obs2ReportList);
        }

        /*
         * if the user has requested that a missing report be shown for each
         * station being considered, then add them as necessary. the report list
         * is passed in AND returned to handle the case where the report list
         * may be empty, and therefore this function must assign a new Head
         * pointer.
         *
         * missing data are only added if requested, since it takes a finite
         * time to determine which stations are missing.
         */

        if ((repList != null) && (pcOptions.getSupressMissing() == 1)) {
            PointDataControlAddMissing.addMissingReports(repList);
        }

        /*
         * Don't bother filtering the data here. For riverstatus we want
         * everything.
         */

        return repList;
    }

    /**
     * Process the time step request.
     *
     * @param updateData
     */
    private void processTimeStepRequest(boolean updateData) {
        List<GageData> reportList = null;
        if (updateData) {

            reportList = null;

            this.pdcTimeStep = new PointDataControlTimeStep();
            if (this.cancelJob) {
                return;
            }
            reportList = pdcTimeStep.updateData();
        } else {
            reportList = this.pdcTimeStep.onlyFilterData();
        }

        if (this.cancelJob) {
            return;
        }
        setObsReportList(reportList);
    }

    /**
     * @return the obsReportList
     */
    public List<GageData> getObsReportList() {
        return hvManager.getObsReportList();
    }

    /**
     * @param obsReportList
     *            the obsReportList to set
     */
    public void setObsReportList(List<GageData> obsReportList) {
        hvManager.setObsReportList(obsReportList);
        if (this.cancelJob) {
            return;
        }
        if (hvManager.isDrawStation()) {
            hvManager.fireMapDrawEvent();
        }
    }

    /**
     * @return the fcstReportList
     */
    public List<GageData> getFcstReportList() {
        return fcstReportList;
    }

    /**
     * @param fcstReportList
     *            the fcstReportList to set
     */
    public void setFcstReportList(List<GageData> fcstReportList) {
        this.fcstReportList = fcstReportList;
    }

    /**
     * @return the updateFlag
     */
    public boolean isUpdateFlag() {
        return updateFlag;
    }

    /**
     * @param updateFlag
     *            the updateFlag to set
     */
    public void setUpdateFlag(boolean updateFlag) {
        this.updateFlag = updateFlag;
    }

    /**
     * @return the obsHeightList
     */
    public List<Observation> getObsHeightList() {
        return obsHeightList;
    }

    /**
     * @param obsHeightList
     *            the obsHeightList to set
     */
    public void setObsHeightList(List<Observation> obsHeightList) {
        this.obsHeightList = obsHeightList;
    }

    /**
     * @return the obsDischargeList
     */
    public List<Observation> getObsDischargeList() {
        return obsDischargeList;
    }

    /**
     * @param obsDischargeList
     *            the obsDischargeList to set
     */
    public void setObsDischargeList(List<Observation> obsDischargeList) {
        this.obsDischargeList = obsDischargeList;
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

    /**
     * Sets the PDCDialog instance.
     *
     * @param pdcDialog
     */
    public void setPointDataControlDialogInstance(
            PointDataControlDlg pdcDialog) {
        this.pdcDialog = pdcDialog;
    }

    /**
     * Apply the shift values to the actual values.
     */
    public void applyShiftValues() {
        // Read in the shift values if not already done
        shiftList = PointControlLocationShift.loadShiftData();

        // Apply the shift values
        List<GageData> obsReportList = hvManager.getObsReportList();
        if (obsReportList != null) {
            for (GageData gd : obsReportList) {
                PDCLocationShift shift = findLocationShift(gd);

                if (shift != null) {
                    gd.setX_shift(shift.getXShift());
                    gd.setY_shift(shift.getYShift());
                } else {
                    gd.setX_shift(0);
                    gd.setY_shift(0);
                }
            }
        }
    }

    /**
     * Get the location shift for the site if one exists.
     *
     * @param gd
     *            The GageData object to find the shift for
     * @return The PDCLocationShift object
     */
    private PDCLocationShift findLocationShift(GageData gd) {
        String paramCode = getParamCodeFromReport(gd);

        for (PDCLocationShift shift : shiftList) {
            if (shift.getParamCode().equals(paramCode)
                    && shift.getLid().equals(gd.getLid())) {
                return shift;
            }
        }

        return null;
    }

    /**
     * Get the Parameter code for that gage.
     *
     * @param gd
     *            The GageData object
     * @return The parameter code string
     */
    private String getParamCodeFromReport(GageData gd) {
        Character shefDurCode;
        if ("PC".equals(gd.getTs())) {
            // PC is always "I", but sometimes the duration might have been
            // screwed up
            shefDurCode = 'I';
        } else {
            shefDurCode = TimeSeriesUtil.convertDur2Code((int) gd.getDur());
            if (shefDurCode == null) {
                shefDurCode = '?';
            }
        }

        return String.format("%s%s%s%s", gd.getPe(), shefDurCode, gd.getTs(),
                gd.getExtremum());
    }

    /**
     * Step through time in HydroView.
     *
     * @param direction
     *            -1 is backwards in time, 1 is forward in time
     */
    public void step(int direction) {
        if (pdcDialog != null) {
            pdcDialog.incrementTime(direction);
        }
    }

    /**
     * Add a StationDisplayListener.
     *
     * @param sdl
     */
    public void addStationDisplayListener(StationDisplayListener sdl) {
        if (!stationListenerList.contains(sdl)) {
            stationListenerList.add(sdl);
        }
    }

    public void removeStationDisplayListener(StationDisplayListener sdl) {
        stationListenerList.remove(sdl);
    }

    /**
     * Fire a StationDisplayUpdateEvent.
     *
     * @param event
     */
    public void fireUpdateEvent(StationDisplayUpdateEvent event) {
        Iterator<StationDisplayListener> iter = stationListenerList.iterator();

        while (iter.hasNext()) {
            (iter.next()).notifyUpdate(event);
        }
    }

    /**
     * @return the isGage
     */
    public boolean isGage() {
        return hvManager.isGage();
    }

    /**
     * @return the isName
     */
    public boolean isName() {
        return hvManager.isName();
    }

    /**
     * @return the isTime
     */
    public boolean isTime() {
        return hvManager.isTime();
    }

    /**
     * @return the isID
     */
    public boolean isId() {
        return hvManager.isId();
    }

    /**
     * @return the isElevation
     */
    public boolean isElevation() {
        return hvManager.isElevation();
    }

    /**
     * @return the isPE
     */
    public boolean isPe() {
        return hvManager.isPe();
    }

    /**
     * @return the isValue
     */
    public boolean isValue() {
        return hvManager.isValue();
    }

    /**
     * @return the pcList
     */
    public List<Curpc> getPcList() {
        return pcList;
    }

    /**
     * @param pcList
     *            the pcList to set
     */
    public void setPcList(List<Curpc> pcList) {
        this.pcList = pcList;
    }

    /**
     * @return the ppList
     */
    public List<Curpp> getPpList() {
        return ppList;
    }

    /**
     * @param ppList
     *            the ppList to set
     */
    public void setPpList(List<Curpp> ppList) {
        this.ppList = ppList;
    }

    /**
     * @return the observationList
     */
    public List<Observation> getObservationList() {
        return observationList;
    }

    /**
     * @param observationList
     *            the observationList to set
     */
    public void setObservationList(List<Observation> observationList) {
        this.observationList = observationList;
    }

    /**
     * @return the obs2ReportList
     */
    public List<GageData> getObs2ReportList() {
        return obs2ReportList;
    }

    /**
     * @param obs2ReportList
     *            the obs2ReportList to set
     */
    public void setObs2ReportList(List<GageData> obs2ReportList) {
        this.obs2ReportList = obs2ReportList;
    }

    /**
     * @return the fcst2ReportList
     */
    public List<GageData> getFcst2ReportList() {
        return fcst2ReportList;
    }

    /**
     * @param fcst2ReportList
     *            the fcst2ReportList to set
     */
    public void setFcst2ReportList(List<GageData> fcst2ReportList) {
        this.fcst2ReportList = fcst2ReportList;
    }

    /**
     * Gets the StationEntryDetails Map.
     *
     * @return the stationEntryMap
     */
    public Map<String, StationEntryDetails> getStationEntryMap() {
        return stationEntryMap;
    }

    /**
     * Add an entry to the StationEntryMap.
     *
     * @param sed
     *            the StationEntryDetails object to add
     */
    public void addStationEntry(StationEntryDetails sed) {
        stationEntryMap.put(sed.getPeDTsE(), sed);
    }

    /**
     * Get a StationEntryDetails record.
     *
     * @param lid
     *            the lid to search on
     * @return the StationEntryDetails object that matches the lid
     */
    public StationEntryDetails getStationEntryDetails(String lid) {
        return stationEntryMap.get(lid);
    }

    /**
     * Reset the StationEntryMap.
     */
    public void resetStationEntryDetailsMap() {
        stationEntryMap = new HashMap<>();
    }

    /**
     * @return the hydroDataInfo
     */
    public String getHydroDataInfo() {
        return hydroDataInfo;
    }

    /**
     * @param hydroDataInfo
     *            the hydroDataInfo to set
     */
    public void setHydroDataInfo(String hydroDataInfo) {
        this.hydroDataInfo = hydroDataInfo;
    }

    /**
     * @return the selectedTimeStepElement
     */
    public int getSelectedTimeStepElement() {
        return selectedTimeStepElement;
    }

    /**
     * @param selectedTimeStepElement
     *            the selectedTimeStepElement to set
     */
    public void setSelectedTimeStepElement(int selectedTimeStepElement) {
        this.selectedTimeStepElement = selectedTimeStepElement;
    }

    @Override
    /**
     * Perform the data retrieval.
     */
    protected IStatus run(IProgressMonitor monitor) {
        String errorPrefix = "";

        try {
            switch (this.requestType) {
            case REQUEST_AD_HOC:
                super.setName("Processing Ad-Hoc Request ...");
                errorPrefix = "Ad Hoc Request ";
                this.processAdhocRequest(updateData);
                break;
            case REQUEST_TIME_STEP:
                super.setName("Processing Time Step Request ...");
                errorPrefix = "Time Step Request ";
                this.processTimeStepRequest(updateData);
                break;
            }
            this.applyShiftValues();
        } catch (Exception e) {
            e.printStackTrace();

            return new Status(Status.ERROR, "com.raytheon.uf.viz.pdc",
                    errorPrefix + "FAILED.", e);
        }

        if (this.cancelJob) {
            statusHandler.debug("INFO: Job Successfully Cancelled.");
            return Status.CANCEL_STATUS;
        }
        return Status.OK_STATUS;
    }

    /**
     * Schedule an ad-hoc or time step data request. Classes that use the
     * PointDataControlManager must use this method to schedule a data retrieval
     * now instead of directly invoking the ad hoc or time step request methods.
     *
     * @param updateData
     *            used to indicate if completely new data should be retrieved
     * @param requestType
     *            an enum indicating the type of request
     */
    public void scheduleRequest(boolean updateData, REQUEST_TYPE requestType) {
        this.updateData = updateData;
        this.requestType = requestType;
        this.cancelJob = false;
        this.schedule();
    }

    public void cancelRunningJobs() {
        if (this.getState() == Job.RUNNING) {
            this.cancelJob = true;
            this.cancel();
        }
    }
}
