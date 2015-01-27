/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.util;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.ChangeStationStateRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveGeoMagDataRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveGeoMagDataRequest.RetrieveGeoMagDataRequestType;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveK1minRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveK1minRequest.RetrieveK1minRequestType;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveStationStateRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStationList;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.GeoMagStationLookup;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.rtkp.Activator;
import gov.noaa.nws.ncep.viz.rtkp.controls.GeoMagAnalysisPlotDlg;
import gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpDataBlockWindow;
import gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpRecentKpWindow;
import gov.noaa.nws.ncep.viz.rtkp.rsc.GeoMagWorldActivityResource;

import java.awt.Color;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.VizWorkbenchManager;
//import gov.noaa.nws.ncep.viz.resources.manager.NcBundleLoader;
//import gov.noaa.nws.ncep.viz.resources.manager.ResourceBndlLoader2;

/**
 * 
 * Utility class for RTKP.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class RTKpUtil {

    private static final String GEOMAGDATA_ERROR = "GeoMag Time Series Data Util Exception";

    /*
     * Geomag Data Block view ID is also defined in plugin.xml.
     */
    public static final String DATABLOCK_VIEW_ID = "gov.noaa.nws.ncep.viz.rtkp.GEOMAGRTKPDATABLOCK";

    /*
     * Geomag Recent Kp view ID is also defined in plugin.xml.
     */
    public static final String RECENTKP_VIEW_ID = "gov.noaa.nws.ncep.viz.rtkp.GEOMAGRTKPRECENTKP";

    /** Path to geoMagStations.xml file. */
    public static final String GEOMAGSTATIONS_FILE_NAME = "ncep"
            + File.separator + "geomag" + File.separator + "geoMagStations.xml";

    /*
     * Lookup table to convert floating point to easy read format
     */
    public static String[] KpLu = { "0o", "0+", "1-", "1o", "1+", "2-", "2o",
            "2+", "3-", "3o", "3+", "4-", "4o", "4+", "5-", "5o", "5+", "6-",
            "6o", "6+", "7-", "7o", "7+", "8-", "8o", "8+", "9-", "9o" };

    public static enum GeoMagStationType {
        ALL, K, NON_K, KP, NON_KP
    }

    public static final String KS_PLOT = "Ks";

    public static final String KP_PLOT = "Kp";

    /**
     * Returns color for a given station count index from the list of colors
     * provided <br>
     * 
     * @param index
     * @param colors
     * @return RGB object
     */
    public static RGB getStationCountColor(int index, RGB[] colors) {

        int numStations = RTKpUtil.getGeoMagStationCodes(GeoMagStationType.KP)
                .size();

        // color table - keyed to the number of stations
        RGB colorTable[] = new RGB[numStations + 1];
        colorTable[numStations] = colors[8];
        if (numStations > 1) {
            colorTable[numStations - 1] = colors[1];
        }
        if (numStations > 2) {
            colorTable[numStations - 2] = colors[2];
        }
        if (numStations > 3) {
            colorTable[numStations - 3] = colors[3];
        }
        if (numStations > 4) {
            colorTable[numStations - 4] = colors[4];
        }
        if (numStations > 5) {
            colorTable[numStations - 5] = colors[5];
        }
        if (numStations > 6) {
            colorTable[numStations - 6] = colors[6];
        }
        if (numStations > 7) {
            for (int i = numStations - 7; i > 0; i--) {
                colorTable[i] = colors[7];
            }
        }

        colorTable[0] = colors[0];

        return colorTable[index];
    }

    /**
     * Retrieves a GeoMagK1min record for a given station code and reftime. <br>
     * 
     * @param stationCode
     *            Station Code
     * @param refTime
     *            Reference time
     * @return GeoMagK1min object
     * @throws GeoMagTimeSeriesDataException
     */
    public static GeoMagK1min retrieveK1minRecord(String stationCode,
            Date refTime) throws GeoMagTimeSeriesDataException {

        RetrieveK1minRequest req = new RetrieveK1minRequest(stationCode,
                refTime, null);
        GeoMagK1min record = null;
        try {
            Object rslts = ThriftClient.sendRequest(req);

            if (rslts != null && rslts instanceof List<?>) {
                ArrayList<GeoMagK1min> rsltsList = (ArrayList<GeoMagK1min>) rslts;
                if (rsltsList != null && rsltsList.size() > 0)
                    record = rsltsList.get(0);
            }
        } catch (Exception e) {
            throw new GeoMagTimeSeriesDataException(
                    "Error during retrieval request.", e);
        }

        return record;
    }

    /**
     * Retrieves list of GeoMagK1min records for a given stationcode, start date
     * and end date. <br>
     * 
     * @param stationCode
     *            A station code
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return List of GeoMagK1min records
     * @throws GeoMagTimeSeriesDataException
     */
    public static List<GeoMagK1min> retrieveStationK1minRecords(
            String stationCode, Date startTime, Date endTime)
            throws GeoMagTimeSeriesDataException {

        RetrieveK1minRequest req = new RetrieveK1minRequest(stationCode,
                startTime, endTime);
        List<GeoMagK1min> rsltsList = new ArrayList<GeoMagK1min>();
        try {
            Object rslts = ThriftClient.sendRequest(req);

            if (rslts != null && rslts instanceof List<?>) {
                rsltsList = (ArrayList<GeoMagK1min>) rslts;
            }
        } catch (Exception e) {
            throw new GeoMagTimeSeriesDataException(
                    "Error during retrieval request.", e);
        }

        return rsltsList;
    }

    /**
     * Retrieves list of GeoMagK1min records for a list of station codes, start
     * date and end date. <br>
     * 
     * @param stationCodes
     *            A list of station codes
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return List of GeoMagK1min records
     * @throws GeoMagTimeSeriesDataException
     */
    public static List<GeoMagK1min> getEstKIndex1min(List<String> stationCodes,
            Date startTime, Date endTime) throws GeoMagTimeSeriesDataException {

        RetrieveK1minRequest req = new RetrieveK1minRequest(stationCodes,
                startTime, endTime, RetrieveK1minRequestType.K);
        List<GeoMagK1min> rsltsList = new ArrayList<GeoMagK1min>();
        try {
            Object rslts = ThriftClient.sendRequest(req);

            if (rslts != null && rslts instanceof List<?>) {
                rsltsList = (ArrayList<GeoMagK1min>) rslts;
            }
        } catch (Exception e) {
            throw new GeoMagTimeSeriesDataException(
                    "Error during retrieval request.", e);
        }

        return rsltsList;
    }

    /**
     * Retrieves list of maps containing kp estimates data for a a given start
     * date and end date. <br>
     * 
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return List of maps containing data
     * @throws GeoMagTimeSeriesDataException
     */
    public static List<Map<String, Object>> getEstKpIndex1min(Date startTime,
            Date endTime) throws GeoMagTimeSeriesDataException {

        SimpleDateFormat f = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss");
        f.setTimeZone(TimeZone.getTimeZone("GMT"));

        RetrieveK1minRequest request = new RetrieveK1minRequest(null,
                startTime, endTime, RetrieveK1minRequestType.KP);
        List<Map<String, Object>> rsltsList = new ArrayList<Map<String, Object>>();

        DbQueryResponse response = null;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            throw new GeoMagTimeSeriesDataException(
                    "Error during geomag data retrieval request."
                            + e.getLocalizedMessage(), e);
        }

        if (response != null) {
            rsltsList = response.getResults();
            int rsltsListSize = (rsltsList != null) ? rsltsList.size() : 0;

            List<Map<String, Object>> newRsltsList = new ArrayList<Map<String, Object>>();

            for (int i = 0; i < rsltsListSize; i++) {
                Map<String, Object> map = rsltsList.get(i);
                Double ks_avg = (Double) map.get("ks_avg");
                Double Kp_est = (double) Math.round(3 * ks_avg) / 3.0;

                map.put("Kp_est", Kp_est);
                newRsltsList.add(map);
            }

            rsltsList = newRsltsList;
        }

        return rsltsList;
    }

    /**
     * Retrieves the last data date among data from kp stations <br>
     * 
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return lastDataDate
     * @throws GeoMagTimeSeriesDataException
     */
    public static Date getKpStationsLastDataDate(Date startTime, Date endTime)
            throws VizException {

        RetrieveK1minRequest request = new RetrieveK1minRequest(
                getGeoMagStationCodes(GeoMagStationType.KP), startTime,
                endTime, RetrieveK1minRequestType.LAST_DATA_DATE);
        Date lastDataDate = new Date();

        try {
            Object rslt = ThriftClient.sendRequest(request);

            if (rslt != null && rslt instanceof Date) {
                lastDataDate = (Date) rslt;
            }
        } catch (Exception e) {
            throw new VizException("Error while retrieving last data date."
                    + e.getLocalizedMessage(), e);
        }

        return lastDataDate;
    }

    /**
     * Save GeoMagStationList to xml file. <br>
     * 
     * @param sfstnlist
     *            GeoMagStationList containing list of stations
     */
    public static void saveGeoMagStationList(GeoMagStationList sfstnlist)
            throws VizException {

        try {
            GeoMagStationLookup.getInstance().saveGeoMagStationList(sfstnlist);
        } catch (Exception e) {
            throw new VizException(
                    " Exception while saving geoMagStations.xml file: "
                            + e.getMessage());
        }

    }

    /**
     * Save a list of GeoMagStations to xml file. <br>
     * 
     * @param stns
     *            List of GeoMagStations
     */
    public static void saveGeoMagStations(List<GeoMagStation> stns)
            throws VizException {

        List<GeoMagStation> allStations = RTKpUtil
                .getGeoMagStations(GeoMagStationType.ALL);
        List<GeoMagStation> stnsToSave = new ArrayList<GeoMagStation>();

        HashMap<String, GeoMagStation> stnMap = new HashMap<String, GeoMagStation>();

        if (stns != null) {
            for (int i = 0; i < stns.size(); i++) {
                stnMap.put(stns.get(i).getStationCode(), stns.get(i));
            }
        }
        if (allStations != null && !allStations.isEmpty()) {
            for (int i = 0; i < allStations.size(); i++) {
                GeoMagStation stn = (GeoMagStation) allStations.get(i);
                if (stnMap.containsKey(stn.getStationCode())) {
                    // stn.setKpState(stnMap.get(stn.getStationCode())
                    // .getKpState());
                    stnsToSave.add(stn);
                } else {
                    stnsToSave.add(stn);
                }
            }
        } else {
            stnsToSave = stns;
        }

        GeoMagStationList stnlist = new GeoMagStationList(
                (ArrayList) stnsToSave);

        try {
            saveGeoMagStationList(stnlist);
        } catch (Exception e) {
            throw new VizException(
                    " Exception while saving geoMagStations.xml file: "
                            + e.getMessage());
        }

    }

    /**
     * Sets up an eclipse Command and ExecuteEvent for a registered commandId,
     * and then executes it.
     * 
     * @param commandId
     *            Command id
     */
    public static void exeCommand(String commandId) {

        IEditorPart part = VizWorkbenchManager.getInstance().getActiveEditor();

        if (part != null) {
            ICommandService service = (ICommandService) part.getSite()
                    .getService(ICommandService.class);
            Command cmd = service.getCommand(commandId);

            if (cmd != null) {

                try {

                    ExecutionEvent exec = new ExecutionEvent(cmd,
                            new HashMap<String, Object>(), null, null);

                    // Execute the handler
                    cmd.executeWithChecks(exec);

                } catch (Exception e) {
                    // Error executing Handler

                    e.printStackTrace();
                    String msg = "Could not draw the window";
                    ErrorDialog.openError(
                            Display.getCurrent().getActiveShell(),
                            "Error Activating one of RTKp's Window" + " Tool",
                            msg, new Status(Status.ERROR, Activator.PLUGIN_ID,
                                    msg, e));
                }
            }
        }
    }

    /**
     * Displays the data block for RTKP.
     * 
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     */
    public static void showDataBlock(Date startTime, Date endTime) {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        IViewPart vpart = wpage.findView(RTKpUtil.DATABLOCK_VIEW_ID);
        if (vpart == null) {
            exeCommand("gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpDataBlockAction");
        }

        GeoMagRTKpDataBlockWindow rtkpDataBlockWindow = GeoMagRTKpDataBlockWindow
                .getAccess();
        if (rtkpDataBlockWindow != null) {
            rtkpDataBlockWindow.setEndTime(endTime);
            rtkpDataBlockWindow.setStartTime(startTime);
            rtkpDataBlockWindow.displayDataBlock();
            // showRecentKpEstBlock(rtkpDataBlockWindow);
        }
    }

    /**
     * Displays the recent Kp estimates block for RTKP.
     * 
     * @param rtkpDataBlockWindow
     *            GeoMagRTKpDataBlockWindow
     */
    public static void showRecentKpEstBlock(
            GeoMagRTKpDataBlockWindow rtkpDataBlockWindow) {

        String perspective = getPerspective();

        // display the recent kp estimates view only in NCP
        if (perspective != null && perspective.contains("NCP")) {

            IWorkbenchPage wpage = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            IViewPart vpart = wpage.findView(RTKpUtil.RECENTKP_VIEW_ID);
            if (vpart == null) {
                exeCommand("gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpRecentKpAction");
            }

            GeoMagRTKpRecentKpWindow rtkpRecentKpWindow = GeoMagRTKpRecentKpWindow
                    .getAccess();
            if (rtkpRecentKpWindow != null && rtkpDataBlockWindow != null) {
                rtkpRecentKpWindow.displayRecentKpEstimates(
                        rtkpDataBlockWindow.getKp_last10_text(),
                        rtkpDataBlockWindow.getKp_last10_color(),
                        rtkpDataBlockWindow.getRecentKpEstTitleColor());
            }
        }
    }

    public static String getPerspective() {
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        if (window != null) {
            IWorkbenchPage page = window.getActivePage();
            if (page != null) {
                IPerspectiveDescriptor desc = page.getPerspective();
                if (desc != null) {
                    return desc.getId();
                }
            }
        }
        return null;
    }

    /**
     * Displays the recent Kp estimates block for RTKP.
     * 
     * @param kp_last10_text
     *            kp_last10_text
     * @param kp_last10_color
     *            kp_last10_color
     * @param titleColor
     *            titleColor
     */
    public static void showRecentKpEstBlock(List<String> kp_last10_text,
            List<RGB> kp_last10_color, Color titleColor) {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        IViewPart vpart = wpage.findView(RTKpUtil.RECENTKP_VIEW_ID);
        if (vpart == null) {
            exeCommand("gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpRecentKpAction");
        }

        GeoMagRTKpRecentKpWindow rtkpRecentKpWindow = GeoMagRTKpRecentKpWindow
                .getAccess();
        if (rtkpRecentKpWindow != null) {
            rtkpRecentKpWindow.displayRecentKpEstimates(kp_last10_text,
                    kp_last10_color, titleColor);
        }
    }

    /**
     * Displays the Geomag World-wide Mag Activity Map RTKP.
     * 
     * @param dataBlock
     *            GeoMagRTKpDataBlockWindow
     */
    public static void showGeoMagWorldActivity(
            GeoMagRTKpDataBlockWindow dataBlock) {
        GeoMagWorldActivityResource geomagMapResource = GeoMagWorldActivityResource
                .getGeoMagWorldActivityResource();
        geomagMapResource.init(dataBlock);
        geomagMapResource.issueRefresh();
    }

    /**
     * Get a list of maps containing the latest estimate kindex and ks values
     * for a given list of station codes, start and end dates.
     * 
     * @param stationCodes
     *            A list of station codes
     * @param startTime
     *            start date
     * @param endTime
     *            end date
     * @return List<Map>
     */
    public static List<Map<String, Object>> getLatestEstKIndex(
            final List<String> stationCodes, final Date startTime,
            final Date endTime) throws VizException {

        RetrieveK1minRequest request = new RetrieveK1minRequest(stationCodes,
                startTime, endTime, RetrieveK1minRequestType.LATEST_K);
        List<Map<String, Object>> rsltsList = new ArrayList<Map<String, Object>>();

        DbQueryResponse response = null;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            throw new VizException(
                    "Error during latest k-index retrieval request."
                            + e.getLocalizedMessage(), e);
        }

        if (response != null) {
            rsltsList = response.getResults();
            int rsltsListSize = (rsltsList != null) ? rsltsList.size() : 0;

            List<Map<String, Object>> newRsltsList = new ArrayList<Map<String, Object>>();

            for (int i = 0; i < rsltsListSize; i++) {
                Map<String, Object> map = rsltsList.get(i);
                Integer k = (Integer) map.get("kestindex");
                Float ks = (Float) map.get("ks");

                if (k > 9) {
                    k = 0;
                }

                if (ks > 9.0) {
                    ks = 0.0f;
                }
                map.put("kestindex", k);
                map.put("ks", ks);
                newRsltsList.add(map);

            }

            rsltsList = newRsltsList;
        }

        return rsltsList;
    }

    /**
     * Get a list of all magnetometer stations and their current states.
     * 
     * @param prevPeriod
     *            Obtain values for previous synoptic period? 1='yes' and 0='no'
     *            (default)
     * @return List<Map>
     */
    public static List<Map<String, Object>> getStationsStates(
            final GeoMagStationType type, final Integer prevPeriod)
            throws VizException {

        Date refTime = null;
        if (prevPeriod == 1) {
            refTime = calcPrevSynPerStartTime();
        } else {
            refTime = calcCurSynPerStartTime();
        }

        if (refTime == null) {
            return new ArrayList<Map<String, Object>>();
        }

        List<String> stnCodes = RTKpUtil.getGeoMagStationCodes(type);

        RetrieveStationStateRequest request = new RetrieveStationStateRequest(
                stnCodes, refTime);

        DbQueryResponse response = null;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            throw new VizException("Error stations' states retrieval request."
                    + e.getLocalizedMessage(), e);
        }

        List<Map<String, Object>> rsltsList = new ArrayList<Map<String, Object>>();
        if (response != null) {
            rsltsList = response.getResults();
            int rsltsListSize = (rsltsList != null) ? rsltsList.size() : 0;

            List<Map<String, Object>> newRsltsList = new ArrayList<Map<String, Object>>();
            Map<String, Object> stnMap = new HashMap<String, Object>();
            int processk = 0, kpstation = 0, active = 0;
            String prevStation = null;

            for (int i = 0; i < rsltsListSize; i++) {
                Map<String, Object> map = rsltsList.get(i);
                String stationCode = (String) map.get("stationcode");
                String state = (String) map.get("processingstate");

                if (!stationCode.equals(prevStation)) {
                    processk = 0;
                    kpstation = 0;
                    active = 0;
                    stnMap = new HashMap<String, Object>();
                    stnMap.put("stationcode", stationCode);
                }
                if ("Process".equals(state)) {
                    processk = 1;
                }
                if ("Algorithm".equals(state)) {
                    kpstation = 1;
                }
                if ("Active".equals(state)) {
                    active = 1;
                }

                stnMap.put("k_station", processk);
                stnMap.put("kp_station", kpstation);
                stnMap.put("kp_active", active);
                if (newRsltsList.contains(stnMap)) {
                    newRsltsList.remove(stnMap);
                }
                newRsltsList.add(stnMap);

                prevStation = stationCode;
            }

            rsltsList = newRsltsList;
        }

        return rsltsList;

    }

    /**
     * Get a list of all magnetometer stations and their current states.
     * 
     * @param type
     *            GeoMagStationType (ALL, K, NON_K, KP, NON_KP, KP_ACTIVE)
     * @param prevPeriod
     *            Obtain values for previous synoptic period? 1='yes' and 0='no'
     *            (default)
     * @return List<Map>
     */
    public static HashMap<String, Map<String, Object>> getStationsStatesMap(
            final GeoMagStationType type, final Integer prevPeriod)
            throws VizException {

        List<Map<String, Object>> curKpStationsStates = RTKpUtil
                .getStationsStates(type, prevPeriod);
        int curKpStationsStatesSize = (curKpStationsStates != null) ? curKpStationsStates
                .size() : 0;

        HashMap<String, Map<String, Object>> statesMap = new HashMap<String, Map<String, Object>>();

        for (int i = 0; i < curKpStationsStatesSize; i++) {
            Map<String, Object> stnMap = curKpStationsStates.get(i);
            String stnCode = (String) stnMap.get("stationcode");
            statesMap.put(stnCode, stnMap);
        }

        return statesMap;
    }

    /**
     * Get a list of all active kp magnetometer station codes
     * 
     * @return List<String>
     */
    public static List<String> getCurActiveKpStationCodes() throws VizException {

        List<Map<String, Object>> curKpStationsStates = RTKpUtil
                .getStationsStates(GeoMagStationType.KP, 0);
        int curKpStationsStatesSize = (curKpStationsStates != null) ? curKpStationsStates
                .size() : 0;

        List<String> activeKpStations = new ArrayList<String>();
        for (int i = 0; i < curKpStationsStatesSize; i++) {
            Map<String, Object> stnMap = curKpStationsStates.get(i);
            String stnCode = (String) stnMap.get("stationcode");
            activeKpStations.add(stnCode);
        }

        // List<Map<String, Object>> prevKpStationsStates = RTKpUtil
        // .getStationsStates(GeoMagStationType.KP, 1);
        // int prevKpStationsStatesSize = (prevKpStationsStates != null) ?
        // prevKpStationsStates
        // .size() : 0;
        // for (int i = 0; i < prevKpStationsStatesSize; i++) {
        // Map<String, Object> stnMap = prevKpStationsStates.get(i);
        // String stnCode = (String) stnMap.get("stationcode");
        // if (!activeKpStations.contains(stnCode)) {
        // activeKpStations.add(stnCode);
        // }
        // }

        return activeKpStations;
    }

    /**
     * Toggles whether or not a station is used in the Kp algorithm for the
     * current or prior synoptic period.
     * 
     * @param stationCode
     *            Station code
     * @param prevPeriod
     *            Obtain values for previous synoptic period? 1='yes' and 0='no'
     *            (default)
     * @return List<Map>
     */
    public static boolean changeStationState(final String stationCode,
            final Integer prevPeriod) throws VizException {

        Boolean rval = Boolean.FALSE;
        Date synopticTime = null;
        if (prevPeriod == 1) {
            synopticTime = calcPrevSynPerStartTime();
        } else {
            synopticTime = calcCurSynPerStartTime();
        }

        if (synopticTime != null) {

            ChangeStationStateRequest request = new ChangeStationStateRequest(
                    stationCode, prevPeriod, synopticTime);
            try {
                rval = (Boolean) RequestRouter.route(request);
            } catch (Exception e) {
                throw new VizException(
                        "Error stations' states retrieval request."
                                + e.getLocalizedMessage(), e);
            }
        }
        return rval;
    }

    /**
     * Get a list of GeoMagStation Codes given a GeoMagStationType (ALL, K,
     * NON_K, KP, NON_KP, KP_ACTIVE).
     * 
     * @param type
     *            - GeoMagStationType (ALL, K, NON_K, KP, NON_KP, KP_ACTIVE)
     * @return List<String>
     */
    public static List<String> getGeoMagStationCodes(GeoMagStationType type) {

        List<String> stations = new ArrayList<String>();

        Map<String, ArrayList<GeoMagStation>> allStations = GeoMagStationLookup
                .getInstance().getStationsByCodeMap();

        if (allStations != null) {
            Object[] stnCodes = allStations.keySet().toArray();
            int stnCodesSize = (stnCodes != null) ? stnCodes.length : 0;

            if (stnCodes != null && stnCodesSize > 0) {

                for (Object stnCode : stnCodes) {
                    ArrayList<GeoMagStation> station = allStations.get(stnCode);
                    if (station != null && station.size() > 0) {

                        boolean kStation = (station.get(0).getkStation() == 1 ? true
                                : false);
                        boolean kpStation = (station.get(0).getKpStation() == 1 ? true
                                : false);
                        switch (type) {
                        case K:
                            if (kStation) {
                                stations.add(station.get(0).getStationCode());
                            }
                            break;
                        case NON_K:
                            if (!kStation) {
                                stations.add(station.get(0).getStationCode());
                            }
                            break;
                        case KP:
                            if (kStation && kpStation) {
                                stations.add(station.get(0).getStationCode());
                            }
                            break;
                        case NON_KP:
                            if (kStation && !kpStation) {
                                stations.add(station.get(0).getStationCode());
                            }
                            break;
                        case ALL:
                        default:
                            stations.add(station.get(0).getStationCode());
                            break;
                        }
                    }
                }
            }
        }
        java.util.Collections.sort(stations);
        return stations;
    }

    /**
     * Get a list of GeoMagStations given a GeoMagStationType (ALL, K, NON_K,
     * KP, NON_KP, KP_ACTIVE).
     * 
     * @param type
     *            - GeoMagStationType (ALL, K, NON_K, KP, NON_KP)
     * @return List<GeoMagStation>
     */
    public static List<GeoMagStation> getGeoMagStations(GeoMagStationType type) {

        List<GeoMagStation> stations = new ArrayList<GeoMagStation>();

        Map<String, ArrayList<GeoMagStation>> allStations = GeoMagStationLookup
                .getInstance().getStationsByCodeMap();

        if (allStations != null) {
            Object[] stnCodes = allStations.keySet().toArray();
            int stnCodesSize = (stnCodes != null) ? stnCodes.length : 0;

            if (stnCodes != null && stnCodesSize > 0) {

                for (Object stnCode : stnCodes) {
                    ArrayList<GeoMagStation> station = allStations.get(stnCode);
                    if (station != null && station.size() > 0) {

                        station.get(0)
                                .getLocation()
                                .setLongitude(
                                        MapUtil.correctLon(station.get(0)
                                                .getLocation().getLongitude()));
                        station.get(0)
                                .getLocation()
                                .setLatitude(
                                        MapUtil.correctLat(station.get(0)
                                                .getLocation().getLatitude()));

                        boolean kStation = (station.get(0).getkStation() == 1 ? true
                                : false);
                        boolean kpStation = (station.get(0).getKpStation() == 1 ? true
                                : false);

                        switch (type) {
                        case K:
                            if (kStation) {
                                stations.add(station.get(0));
                            }
                            break;
                        case NON_K:
                            if (!kStation) {
                                stations.add(station.get(0));
                            }
                            break;
                        case KP:
                            if (kStation && kpStation) {
                                stations.add(station.get(0));
                            }
                            break;
                        case NON_KP:
                            if (kStation && !kpStation) {
                                stations.add(station.get(0));
                            }
                            break;
                        case ALL:
                        default:
                            stations.add(station.get(0));
                            break;
                        }

                    }
                }
            }
        }
        java.util.Collections.sort(stations);
        return stations;
    }

    /**
     * Returns start time of previous synoptic period
     * 
     * @return startTime of previous synoptic period
     */
    public static Date calcPrevSynPerStartTime() {
        Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        ttime.set(Calendar.MINUTE, 0);
        ttime.set(Calendar.SECOND, 0);
        ttime.set(Calendar.MILLISECOND, 0);
        int currentHour = ttime.get(Calendar.HOUR_OF_DAY);
        // find current synoptic period end time
        int hoursToAdd = 3 - (currentHour % 3);
        ttime.add(Calendar.HOUR_OF_DAY, hoursToAdd);
        // subtract -6 to get start time of previous synoptic period
        ttime.add(Calendar.HOUR_OF_DAY, -6);

        return ttime.getTime();
    }

    /**
     * Returns start time of current synoptic period
     * 
     * @return startTime of current synoptic period
     */
    public static Date calcCurSynPerStartTime() {
        Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        ttime.set(Calendar.MINUTE, 0);
        ttime.set(Calendar.SECOND, 0);
        ttime.set(Calendar.MILLISECOND, 0);
        int currentHour = ttime.get(Calendar.HOUR_OF_DAY);
        // find current synoptic period end time
        int hoursToAdd = 3 - (currentHour % 3);
        ttime.add(Calendar.HOUR_OF_DAY, hoursToAdd);
        // subtract -3 to get start time of current synoptic period
        ttime.add(Calendar.HOUR_OF_DAY, -3);

        return ttime.getTime();
    }

    /**
     * Returns end time of current synoptic period
     * 
     * @return endTime of current synoptic period
     */
    public static Date calcCurSynPerEndTime() {
        Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        ttime.set(Calendar.MINUTE, 0);
        ttime.set(Calendar.SECOND, 0);
        ttime.set(Calendar.MILLISECOND, 0);
        int currentHour = ttime.get(Calendar.HOUR_OF_DAY);
        // find current synoptic period end time
        int hoursToAdd = 3 - (currentHour % 3);
        ttime.add(Calendar.HOUR_OF_DAY, hoursToAdd);

        return ttime.getTime();
    }

    /**
     * Returns the current date with second and millisecond set to 0
     * 
     * @return currentTime
     */
    public static Date calcCurTime() {
        Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        ttime.set(Calendar.SECOND, 0);
        ttime.set(Calendar.MILLISECOND, 0);
        return ttime.getTime();
    }

    /**
     * Returns the current time
     * 
     * @return currentTime
     */
    public static Date getCurrentTime() {
        Calendar ttime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        return ttime.getTime();
    }

    /**
     * Returns the start time for Data Block given an end time
     * 
     * @param endTime
     * @return startTime
     */
    public static Date calcDataBlockStartTime(Date endTime) {
        Calendar time = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        time.setTime(endTime);
        time.add(Calendar.MINUTE, -20);
        return time.getTime();
    }

    public static void loadHDWithQdcPlots(String stationCode, String sourceId)
            throws VizException {

        File rbdFile = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/GeoMagHDQdcPlots.xml"); // GeoMagHDQdcPlots
        // WNG_hdqdc_plot1_mp_test
        AbstractRBD<?> rbdBndl = AbstractRBD.getRbd(rbdFile);

        Shell parent = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        GeoMagAnalysisPlotDlg dlg = new GeoMagAnalysisPlotDlg(parent, 850, 850,
                rbdBndl, stationCode, sourceId);
        dlg.open();
    }

    public static String getMostAvailableDataBasedOnSourceId(
            String stationCode, Date startTime, Date endTime)
            throws VizException {

        String sourceId = "101";
        try {
            Integer count101 = getGeoMagRecordsCountForSource(stationCode,
                    startTime, endTime, 101);
            Integer count102 = getGeoMagRecordsCountForSource(stationCode,
                    startTime, endTime, 102);
            Integer count103 = getGeoMagRecordsCountForSource(stationCode,
                    startTime, endTime, 103);

            Integer max = count101;
            if (count102 > max) {
                max = count102;
                sourceId = "102";
            }
            if (count103 > max) {
                max = count103;
                sourceId = "103";
            }

        } catch (GeoMagTimeSeriesDataException e) {
            throw new VizException(
                    "Error at getMostAvailableDataBasedOnSourceId().", e);
        }
        return sourceId;

    }

    public static Integer getGeoMagRecordsCountForSource(String stationCode,
            Date startTime, Date endTime, int sourceId)
            throws GeoMagTimeSeriesDataException {

        RetrieveGeoMagDataRequest request = new RetrieveGeoMagDataRequest(
                stationCode, startTime, endTime, sourceId,
                RetrieveGeoMagDataRequestType.DATA_LIST);

        List<GeoMagRecord> rsltsList = new ArrayList<GeoMagRecord>();
        try {
            Object rslts = ThriftClient.sendRequest(request);

            if (rslts != null && rslts instanceof List<?>) {
                rsltsList = (ArrayList<GeoMagRecord>) rslts;
            }
        } catch (Exception e) {
            throw new GeoMagTimeSeriesDataException(
                    "Error during retrieval request.", e);
        }

        return (rsltsList != null) ? rsltsList.size() : 0;
    }

    /**
     * Displays an Exception in a Message Dialog
     * 
     * @param e
     */
    public static void showError(Exception e) {

        StringBuilder sb = new StringBuilder(e.getMessage());

        Throwable temp = e;
        while ((temp = temp.getCause()) != null) {
            sb.append("\n");
            sb.append(temp.getMessage());
        }

        MessageDialog errorDlg = new MessageDialog(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(), GEOMAGDATA_ERROR, null,
                sb.toString(), MessageDialog.ERROR, new String[] { "OK" }, 0);

        errorDlg.open();
    }

}
