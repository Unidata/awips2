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

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.Activator;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants.TimeModeType;
import com.raytheon.viz.hydro.pointdatacontrol.data.PDCLocationShift;
import com.raytheon.viz.hydro.pointdatacontrol.data.PointControlPeTs;
import com.raytheon.viz.hydro.pointdatacontrol.data.StationEntryDetails;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PDCFindBasis;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointControlLocationShift;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlAddMissing;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlDerive;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlFilter;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlRiverStatus;
import com.raytheon.viz.hydro.pointdatacontrol.engine.PointDataControlValue;
import com.raytheon.viz.hydro.pointdatacontrol.util.PDCUtils;
import com.raytheon.viz.hydro.resource.DamLocationResource;
import com.raytheon.viz.hydro.resource.MultiPointResource;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.Observation;
import com.raytheon.viz.hydrocommon.events.MapUpdateEvent;
import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;
import com.raytheon.viz.hydrocommon.listeners.MapUpdateListener;
import com.raytheon.viz.hydrocommon.listeners.StationDisplayListener;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.pdc.PDCRiverOptions;
import com.raytheon.viz.hydrocommon.util.HydroDialogStatus;

/**
 * PDC Manager.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2008            mpduff      Initial creation
 * Jan 25, 2011 #7907      bkowal      Added private variables to cache information
 *                                     so that it would not need to be retrieved
 *                                     every time.
 * Jan 27, 2011 #5274      bkowal      Using the swt job class to request data
 *                                     asynchronously and display a progress
 *                                     indicator on the main interface.
 * Feb 11, 2014 #15829     lbousaidi   check for Missing before processing River Threat.                                    
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointDataControlManager extends Job {
    private static PointDataControlManager instance = null;

    private List<String> responsibleHsa = new ArrayList<String>();

    private PDCDataManager dataManager = null;

    private HydroDisplayManager hvManager = HydroDisplayManager.getInstance();

    private int refreshMinutes = PDCConstants.REFRESH_MINUTES;

    private boolean firstGad = true;

    private Date previousRetrievalDate = SimulatedTime.getSystemTime()
            .getTime();

    private boolean redraw = false;

    private boolean dataRetrievalRequired = false;

    private List<Observation> obsHeightList = new ArrayList<Observation>();

    private List<Observation> obsDischargeList = new ArrayList<Observation>();

    private List<Observation> observationList = new ArrayList<Observation>();

    private List<Riverstatus> riverStatusList = new ArrayList<Riverstatus>();

    private List<Curpc> pcList = null;

    private List<Curpp> ppList = null;

    private List<GageData> obsReportList = null;

    private List<GageData> fcstReportList = null;

    private List<GageData> obs2ReportList = null;

    private List<GageData> fcst2ReportList = null;

    private boolean updateFlag = false;

    private ArrayList<MapUpdateListener> listenerList = new ArrayList<MapUpdateListener>();

    private ArrayList<StationDisplayListener> stationListenerList = new ArrayList<StationDisplayListener>();

    private ArrayList<PDCLocationShift> shiftList = null;

    private PointDataControlDlg pdcDialog = null;

    private boolean isGage = true;

    private boolean isName = true;

    private boolean isTime = true;

    private boolean isID = true;

    private boolean isElevation = false;

    private boolean isPE = false;

    private boolean isValue = false;

    private MultiPointResource multiPointResource = null;

    private Map<String, StationEntryDetails> stationEntryMap = new HashMap<String, StationEntryDetails>();

    private ColorMap colorMap = null;

    private ColorMapParameters colorMapParameters = null;

    private String hydroDataInfo = null;

    private int selectedTimeStepElement = 0;

    private DamLocationResource damLocationResource = null;

    private String colorUseName = null;

    /* For Ad-Hoc Requests */
    private PointDataControlDerive derive = null;

    private ArrayList<GageData> repList = null;

    /* For Time Step Requests */
    private ArrayList<GageData> reportList = null;

    private PointDataControlTimeStep pdcTimeStep = null;

    /* For The Jobs That Will Be Running */
    public static enum REQUEST_TYPE {
        REQUEST_AD_HOC, REQUEST_TIME_STEP
    }

    private boolean updateData;

    private REQUEST_TYPE requestType;

    boolean cancelJob = false;

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
            fireMapDrawEvent();
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
                String minuteString = String.valueOf(PDCUtils
                        .getRefreshMinutes());
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
            this.derive = new PointDataControlDerive();
            this.repList = new ArrayList<GageData>();
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

            switch (pcOptions.getElementType()) {
            case 0: // RIVER_AD_HOC_TYPE
                /*
                 * get the data from the appropriate data set, whether it be
                 * from the height, discharge, or riverstatus table. then
                 * process the data to get a single value for each location; one
                 * list is for observed data and one is for forecast data
                 */
                dataManager.getRiverData(pcOptions);
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
                            && !pcOptions.getSelectedAdHocElementString()
                                    .equalsIgnoreCase("PC")
                            && !pcOptions.getSelectedAdHocElementString()
                                    .equalsIgnoreCase("PP")) {
                        dataManager.getSnowTempOtherData();

                        obsReportList = derive.deriveOtherReports();
                    } else {
                        /*
                         * get PC and/or PP data. then process the data to get a
                         * single value for each location
                         */
                        dataManager.getRainData();
                        obsReportList = derive.deriveRainReports();
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

                dataManager.getSnowTempOtherData();

                obsReportList = derive.deriveOtherReports();

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

            if ((pcOptions.getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                    .getAdHocDataElementType())
                    && (pcOptions.getTimeMode() == TimeModeType.LATEST
                            .getTimeMode())) {
                if (pcOptions.getStageBasis() == 0) { // BASIS_OBS
                    repList = PDCFindBasis
                            .copyReportList((ArrayList<GageData>) obsReportList);
                    // .copyReportList((ArrayList<GageData>) obsReportList);
                } else if (pcOptions.getStageBasis() == 1) { // Forecast
                    repList = PDCFindBasis
                            .copyReportList((ArrayList<GageData>) fcstReportList);
                } else { // Both obs and fcst
                    repList = PDCFindBasis.determineRsMofo(
                            (ArrayList<GageData>) getObsReportList(),
                            (ArrayList<GageData>) getFcstReportList());
                }
            } else {
                repList = PDCFindBasis
                        .copyReportList((ArrayList<GageData>) obsReportList);
            }

            if (repList != null) {
                repList.trimToSize();
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
        } /* end of if (retrieval_required) block */

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
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        manager.setDrawStation(true);

        setObsReportList(repList);
    }

    public ArrayList<GageData> pcProcessOnetime(PDCRiverOptions pcOptionsOnetime) {
        final boolean oneTime = true;
        ArrayList<GageData> repList = null;
        obs2ReportList = null;
        fcst2ReportList = null;
        PDCOptionData pcOptions = PDCOptionData.getInstance();

        /*
         * get the RAW data, then derive the DERIVED data based on the user
         * instructions.
         */
        PointDataControlDerive derive = new PointDataControlDerive();
        if (pcOptionsOnetime.getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                .getAdHocDataElementType()) {
            /*
             * get the data from the appropriate data set, whether it be from
             * the height, discharge, or riverstatus table. then process the
             * data to get a single value for each location; one list is for
             * observed data and one is for forecast data
             */
            dataManager.getRiverData(pcOptionsOnetime);
            derive.deriveRiverReports(oneTime, pcOptionsOnetime);
        } else if (pcOptionsOnetime.getElementType() == HydroConstants.AdHocDataElementType.RAIN_AD_HOC_TYPE
                .getAdHocDataElementType()) {
            /*
             * any "rain" data other than PC, PP, or both, is treated like
             * generic data. then process the data to get a single value for
             * each location
             */

            if ((pcOptions.getPcAndpp() != 1)
                    && !pcOptions.getSelectedAdHocElementString()
                            .equalsIgnoreCase("PC")
                    && !pcOptions.getSelectedAdHocElementString()
                            .equalsIgnoreCase("PP")) {
                dataManager.getSnowTempOtherData();

                obsReportList = derive.deriveOtherReports();
            } else {
                /*
                 * get PC and/or PP data. then process the data to get a single
                 * value for each location
                 */
                dataManager.getRainData();

                obsReportList = derive.deriveRainReports();
            }
        } else {
            /*
             * get snow, temperature or other data, including processed data, in
             * a general fashion. then process the data to get a single value
             * for each location
             */

            dataManager.getSnowTempOtherData();

            obsReportList = derive.deriveOtherReports();
        }

        /*
         * if getting the latest River data, then the stage basis selected by
         * the user must be applied. if getting the max of the obs or forecast,
         * then loop thru the observed and forecast lists and assign the
         * appropriate value. otherwise simply copy the list.
         */

        if ((pcOptionsOnetime.getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                .getAdHocDataElementType())
                && (pcOptionsOnetime.getTimeMode() == TimeModeType.LATEST
                        .getTimeMode())) {
            if ((pcOptionsOnetime.getStageBasis() == 0)
                    && (obsReportList != null)) { // BASIS_OBS
                repList = PDCFindBasis
                        .copyReportList((ArrayList<GageData>) obs2ReportList);
            } else if ((pcOptionsOnetime.getStageBasis() == 1)
                    && (fcstReportList != null)) { // BASIS_FCST
                repList = PDCFindBasis
                        .copyReportList((ArrayList<GageData>) fcst2ReportList);
            } else {
                repList = PDCFindBasis.determineRsMofo(
                        (ArrayList<GageData>) obs2ReportList,
                        (ArrayList<GageData>) fcst2ReportList);
            }
        } else {
            repList = PDCFindBasis
                    .copyReportList((ArrayList<GageData>) obs2ReportList);
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
        if (updateData) {
            this.reportList = null;

            this.pdcTimeStep = new PointDataControlTimeStep();
            if (this.cancelJob) {
                return;
            }
            reportList = pdcTimeStep.updateData();
        } else {
            this.reportList = this.pdcTimeStep.onlyFilterData();
        }

        if (this.cancelJob) {
            return;
        }
        setObsReportList(this.reportList);
    }

    /**
     * @return the obsReportList
     */
    public List<GageData> getObsReportList() {
        return obsReportList;
    }

    /**
     * @param obsReportList
     *            the obsReportList to set
     */
    public void setObsReportList(List<GageData> obsReportList) {
        this.obsReportList = obsReportList;
        if (this.cancelJob) {
            return;
        }
        if (hvManager.isDrawStation()) {
            fireMapDrawEvent();
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
    public void setObsHeightList(List<Observation> anObsHeightList) {
        obsHeightList = anObsHeightList;
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
    public void setObsDischargeList(List<Observation> anObsDischargeList) {
        obsDischargeList = anObsDischargeList;
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
    public void setRiverStatusList(List<Riverstatus> aRiverStatusList) {
        riverStatusList = aRiverStatusList;
    }

    /**
     * Sets the PDCDialog instance.
     * 
     * @param pdcDialog
     */
    public void setPointDataControlDialogInstance(PointDataControlDlg pdcDialog) {
        this.pdcDialog = pdcDialog;
    }

    public void addMapUpdateListener(MapUpdateListener mul) {
        listenerList.add(mul);
    }

    public void removeMapUpdateListener(MapUpdateListener mul) {
        if (listenerList.contains(mul)) {
            listenerList.remove(mul);
        }
    }

    private void fireMapDrawEvent() {
        if (this.cancelJob) {
            return;
        }
        MapUpdateEvent event = new MapUpdateEvent(this);

        Iterator<MapUpdateListener> iter = listenerList.iterator();
        if (this.cancelJob) {
            return;
        }
        while (iter.hasNext()) {
            MapUpdateListener listener = iter.next();
            if (listener.getClass().toString().contains("StationListDlg")) {
                if (HydroDialogStatus.stationListDlgOpen == true) {
                    listener.notifyUpdate(event);
                }
            } else {
                listener.notifyUpdate(event);
            }
        }
    }

    /**
     * Apply the shift values to the actual values.
     */
    public void applyShiftValues() {
        // Read in the shift values if not already done
        shiftList = PointControlLocationShift.loadShiftData();

        // Apply the shift values
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
        String shefDurCode = "?";
        if (gd.getTs().equals("PC")) {
            // PC is always "I", but sometimes the duration might have been
            // screwed up
            shefDurCode = "I";
        } else {
            shefDurCode = getShefDurCodeFromIhfsDurCode((int) gd.getDur());
        }

        return String.format("%s%s%s%s", gd.getPe(), shefDurCode, gd.getTs(),
                gd.getExtremum());
    }

    /**
     * Get the SHEF duration code.
     * 
     * @param intDuration
     *            The duration value
     * @return The duration code
     */
    private String getShefDurCodeFromIhfsDurCode(int intDuration) {
        /*
         * 0 | I | Instantaneous 1 | U | 1 Minute 15 | C | 15 Minutes 30 | J |
         * 30 Minutes 1001 | H | 1 Hour 1002 | B | 2 Hour 1003 | T | 3 Hour 1004
         * | F | 4 Hour 1006 | Q | 6 Hour 1008 | A | 8 Hour 1012 | K | 12 Hour
         * 1018 | L | 18 Hour 2001 | D | 1 Day 2007 | W | 1 Week 3001 | M | 1
         * Month 4001 | Y | 1 Year 5000 | Z | Unspecified 5001 | S | Seasonal
         * 5002 | R | Period of Record 5004 | P | Total Since 7 AM 5005 | X |
         * Unknown
         */

        char code = '?';

        // 21 characters that I care about
        String codes = "IUCJHBTFQAKLDWMYZSRPX";
        char[] charCodeArray = codes.toCharArray();

        int[] intCodeArray = { 0, 1, 15, 30, 1001, 1002, 1003, 1004, 1006,
                1008, 1012, 1018, 2001, 2007, 3001, 4001, 5000, 5001, 5002,
                5004, 5005 };

        int i = 0;

        for (i = 0; i < intCodeArray.length; i++) {
            if (intDuration == intCodeArray[i]) {
                code = charCodeArray[i];
                break;
            }
        }

        if (code == '?') {
            // System.err.println("getShefDurCodeFromIhfsDurCode(): unidentified dur code = "
            // + intDuration);
        }

        return Character.toString(code);

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
        stationListenerList.add(sdl);
    }

    /**
     * Fire a StationDisplayUpdateEvent.
     */
    public void fireUpdateEvent(StationDisplayUpdateEvent event) {
        Iterator<StationDisplayListener> iter = stationListenerList.iterator();

        while (iter.hasNext()) {
            (iter.next()).notifyUpdate(event);
        }
        setGage(event.isGage());
        setID(event.isID());
        setPE(event.isPE());
        setTime(event.isTime());
        setElevation(event.isElevation());
        setValue(event.isValue());
        setName(event.isName());
    }

    /**
     * @return the isGage
     */
    public boolean isGage() {
        return isGage;
    }

    /**
     * @param isGage
     *            the isGage to set
     */
    public void setGage(boolean isGage) {
        this.isGage = isGage;
    }

    /**
     * @return the isName
     */
    public boolean isName() {
        return isName;
    }

    /**
     * @param isName
     *            the isName to set
     */
    public void setName(boolean isName) {
        this.isName = isName;
    }

    /**
     * @return the isTime
     */
    public boolean isTime() {
        return isTime;
    }

    /**
     * @param isTime
     *            the isTime to set
     */
    public void setTime(boolean isTime) {
        this.isTime = isTime;
    }

    /**
     * @return the isID
     */
    public boolean isID() {
        return isID;
    }

    /**
     * @param isID
     *            the isID to set
     */
    public void setID(boolean isID) {
        this.isID = isID;
    }

    /**
     * @return the isElevation
     */
    public boolean isElevation() {
        return isElevation;
    }

    /**
     * @param isElevation
     *            the isElevation to set
     */
    public void setElevation(boolean isElevation) {
        this.isElevation = isElevation;
    }

    /**
     * @return the isPE
     */
    public boolean isPE() {
        return isPE;
    }

    /**
     * @param isPE
     *            the isPE to set
     */
    public void setPE(boolean isPE) {
        this.isPE = isPE;
    }

    /**
     * @return the isValue
     */
    public boolean isValue() {
        return isValue;
    }

    /**
     * @param isValue
     *            the isValue to set
     */
    public void setValue(boolean isValue) {
        this.isValue = isValue;
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
     * @return the mpr
     */
    public MultiPointResource getMultiPointResource() {
        return multiPointResource;
    }

    /**
     * @param mpr
     *            the mpr to set
     */
    public void setMultiPointResource(MultiPointResource mpr) {
        multiPointResource = mpr;
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
        stationEntryMap.put(sed.getLid(), sed);
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
        stationEntryMap = new HashMap<String, StationEntryDetails>();
    }

    /**
     * Get the colorMap.
     * 
     * @return the colorMap
     */
    public IColorMap getColorMap() {
        return colorMap;
    }

    public void setColorMap(IColorMap colorMap) {
        this.colorMap = (ColorMap) colorMap;
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

    /**
     * @return the colorMapParameters
     */
    public ColorMapParameters getColorMapParameters() {
        return colorMapParameters;
    }

    /**
     * @param colorMapParameters
     *            the colorMapParameters to set
     */
    public void setColorMapParameters(ColorMapParameters colorMapParameters) {
        this.colorMapParameters = colorMapParameters;
    }

    /**
     * @return the damLocationResource
     */
    public DamLocationResource getDamLocationResource() {
        return damLocationResource;
    }

    /**
     * @param damLocationResource
     *            the damLocationResource to set
     */
    public void setDamLocationResource(DamLocationResource damLocationResource) {
        this.damLocationResource = damLocationResource;
    }

    /**
     * @return the colorUseName
     */
    public String getColorUseName() {
        return colorUseName;
    }

    /**
     * @param colorUseName
     *            the colorUseName to set
     */
    public void setColorUseName(String colorUseName) {
        this.colorUseName = colorUseName;
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

            return new Status(Status.ERROR, Activator.PLUGIN_ID, errorPrefix
                    + "FAILED.", e);
        }

        if (this.cancelJob) {
            System.out.println("INFO: Job Successfully Cancelled.");
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
