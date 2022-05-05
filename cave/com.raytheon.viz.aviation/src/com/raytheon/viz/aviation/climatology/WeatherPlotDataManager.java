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
package com.raytheon.viz.aviation.climatology;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.aviation.cachedata.CacheGuidanceRequest;
import com.raytheon.viz.aviation.cachedata.MetarCacheGuidanceRequest;
import com.raytheon.viz.aviation.cachedata.MosCacheGuidanceRequest;
import com.raytheon.viz.aviation.cachedata.PythonCacheGuidanceJob;
import com.raytheon.viz.aviation.guidance.GuidanceRequest.GuidanceType;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;
import com.raytheon.viz.aviation.monitor.TafUtil;
import com.raytheon.viz.aviation.xml.PlotViewerCfg;
import com.raytheon.viz.aviation.xml.WxPlotCfg;

import jep.JepConfig;
import jep.JepException;

/**
 * A singleton class for retrieving weather data for plotting.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2009            avarani     Initial creation
 * Nov 18, 2010 6701       rferrel     Use PlotViewerCfg class name and
 *                                     only retrieves selected data.
 * 28 FEB 2011  8188       rferrel     Fixed getNam
 * Apr 28, 2011 8065       rferrel     Use cache data
 * Nov 26, 2012 1298       rferrel     Non-blocking dialog cleanup now use
 *                                      IUFStatusHandler for error messages.
 * May 15, 2014 3002       bgonzale    Moved common taf code to com.raytheon.uf.common.dataplugin.taf.
 * Nov 02, 2016 5979       njensen     Cast to Number where applicable
 * Jan 13, 2017 5959       njensen     Cleaned up warnings
 * Mar 22, 2017  6183      tgurney     Move python files to common_static
 * Jul 31, 2019  7878      tgurney     Handle pickled Python objects as byte[]
 * Feb 26, 2020  7878      tgurney     Fix casting of Python int to Java int
 * Mar  9, 2020  8048      tgurney     Fix NPE in formatMosData
 *
 * </pre>
 *
 * @author avarani
 */

public class WeatherPlotDataManager {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherPlotDataManager.class);

    private static final Map<String, String> modelTypes = new HashMap<>();
    static {
        modelTypes.put("etamos", "ETA");
        modelTypes.put("etabuf", "ETA");
        modelTypes.put("gfsmos", "GFS");
        modelTypes.put("gfslamp", "LAMP");
    }

    private static WeatherPlotDataManager instance;

    private WxPlotCfg wxPlotCfg;

    private long tStart;

    private long tEnd;

    private List<String> tafLabels;

    private Map<String, List<Map<String, Object>>> dcdTafs;

    private List<Map<String, Object>> dcdMtrs;

    private Map<String, Map<String, Object>> namWrfData;

    private Map<String, Map<String, Object>> gfsMosData;

    private Map<String, Map<String, Object>> gfsLampData;

    private Map<String, Map<String, Object>> namMosData;

    private String selectedTafKey;

    private String selectedGfsMosKey;

    private String selectedGfsLampKey;

    private String selectedNamMosKey;

    private String selectedNamKey;

    private boolean dataLoaded;

    private String includePath;

    private String filePath;

    private List<CacheGuidanceRequest> cacheRequests = new ArrayList<>();

    private WeatherPlotDataManager() {
        dcdTafs = new HashMap<>();
        tafLabels = new ArrayList<>();
        dcdMtrs = new ArrayList<>();
        namWrfData = null;
        gfsMosData = null;
        gfsLampData = null;
        namMosData = null;
        dataLoaded = false;
        selectedTafKey = "";
        selectedGfsMosKey = "";
        selectedGfsLampKey = "";
        selectedNamMosKey = "";
        selectedNamKey = "";

        getNewConfig();
    }

    public static synchronized WeatherPlotDataManager getInstance() {
        if (instance == null) {
            instance = new WeatherPlotDataManager();
        }

        return instance;
    }

    public void getNewConfig() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile("aviation/config/gui/WxPlotCfg.xml");

            wxPlotCfg = JAXB.unmarshal(path, WxPlotCfg.class);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to read WxPlotCfg.xml", e);
        }
    }

    public void loadCacheData(String siteId) {
        List<PlotViewerCfg> plotViewerList = wxPlotCfg.getPlotViewers();
        List<CacheGuidanceRequest> cacheRequests = new ArrayList<>();
        for (PlotViewerCfg viewerCfg : plotViewerList) {
            String viewerData = viewerCfg.getClassName();
            CacheGuidanceRequest req = null;

            if (viewerData.equals(PlotViewerCfg.ClassNames.TAFS.getName())) {
                continue;
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.METARS.getName())) {
                req = checkCacheMetars(siteId);
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.NAM_MOS.getName())) {
                req = checkCacheMos(siteId, "etamos", null);
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.GFS_MOS.getName())) {
                req = checkCacheMos(siteId, "gfsmos", null);
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.GFSLAMP.getName())) {
                req = checkCacheMos(siteId, "gfslamp", null);
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.NAM_WRF.getName())) {
                req = checkCacheMos(siteId, "etabuf", null);
            } else {
                statusHandler.warn("Unknown class name: " + viewerData);
            }
            if (req != null) {
                cacheRequests.add(req);
            }
        }
        if (!cacheRequests.isEmpty()) {
            PythonCacheGuidanceJob.getInstance()
                    .waitForCacheRequests(cacheRequests);
        }
    }

    private CacheGuidanceRequest checkCacheMetars(String siteId) {
        String metarSize = String.valueOf(
                wxPlotCfg.getHoursBack() + 1 + wxPlotCfg.getHoursForeward());
        String tag = MetarCacheGuidanceRequest.getTag(siteId, metarSize);
        byte[] siteObj = PythonCacheGuidanceJob.getInstance().getSiteObj(siteId,
                tag);
        MetarCacheGuidanceRequest req = null;
        if (siteObj == null) {
            req = new MetarCacheGuidanceRequest();
            req.setTag(tag);
            req.setGuidanceType(GuidanceType.METAR);
            req.setSiteID(siteId);
            req.setSize(metarSize);
        }
        return req;
    }

    /**
     * This determines if the data for the mapping has been cached. When the
     * data is cached it is formatted for plotting and placed in the map.
     *
     * @param cacheMap
     *            - The mapping
     * @return req - The request needed to obtain the data else null if already
     *         cached
     */
    private CacheGuidanceRequest checkCacheMos(Map<String, Object> cacheMap) {
        if (cacheMap == null) {
            return null;
        }
        String siteId = cacheMap.get("siteId").toString();
        String model = cacheMap.get("model").toString();
        String refTimeStr = cacheMap.get("refTimeStr").toString();
        CacheGuidanceRequest req = checkCacheMos(siteId, model, refTimeStr);

        if (req != null) {
            // Remove any old data
            cacheMap.put("data", null);
        } else if (cacheMap.get("data") == null) {
            String tag = MosCacheGuidanceRequest.getTag(siteId, model,
                    refTimeStr);
            byte[] siteObj = PythonCacheGuidanceJob.getInstance()
                    .getSiteObj(siteId, tag);
            if (!"etabuf".equals(model)) {
                cacheMap.put("data", formatMosData(siteObj, model));
            } else {
                cacheMap.put("data", formatNamData(siteObj));
            }
        }

        return req;
    }

    /**
     * This determines if the desired data is in cache. When non-null request is
     * returned it is up to the user to queue the request.
     *
     * @param siteId
     *
     * @param model
     *            -
     * @param refTimeStr
     *            - Reference time as a string or "current" for latest time
     * @return req - null when data is cached otherwise the request need to
     *         cache the data
     */
    private CacheGuidanceRequest checkCacheMos(String siteId, String model,
            String refTimeStr) {
        String format = "short";
        String tag = null;
        if (refTimeStr == null) {
            tag = MosCacheGuidanceRequest.getTag(siteId, model);
        } else {
            tag = MosCacheGuidanceRequest.getTag(siteId, model, refTimeStr);
        }
        byte[] siteObj = PythonCacheGuidanceJob.getInstance().getSiteObj(siteId,
                tag);
        MosCacheGuidanceRequest req = null;
        if (siteObj == null) {
            req = new MosCacheGuidanceRequest();
            req.setTag(tag);
            req.setGuidanceType(GuidanceType.MOS);
            req.setSiteID(siteId);
            req.setModel(model);
            req.setFormat(format);
            if (refTimeStr != null && !"current".equals(refTimeStr)) {
                req.setRefTime(refTimeStr);
            }
        }
        return req;
    }

    public boolean loadData(String siteId, long currentTime) {
        long now = currentTime;
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTimeInMillis(now);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MILLISECOND, 0);
        now = c.getTimeInMillis();
        c.add(Calendar.HOUR_OF_DAY, -1 * wxPlotCfg.getHoursBack());
        tStart = c.getTimeInMillis();
        c.setTimeInMillis(now);
        c.add(Calendar.HOUR_OF_DAY, wxPlotCfg.getHoursForeward());
        tEnd = c.getTimeInMillis();
        List<PlotViewerCfg> plotViewerList = wxPlotCfg.getPlotViewers();

        dcdTafs.clear();
        tafLabels.clear();
        selectedTafKey = "";
        dcdMtrs.clear();
        selectedNamMosKey = "";
        selectedGfsMosKey = "";
        selectedGfsLampKey = "";
        selectedNamKey = "";

        boolean state = true;
        for (PlotViewerCfg viewerCfg : plotViewerList) {
            String viewerData = viewerCfg.getClassName();

            if (viewerData.equals(PlotViewerCfg.ClassNames.TAFS.getName())) {
                state = loadTafs(siteId);
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.METARS.getName())) {
                state = loadMetars(siteId);
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.NAM_MOS.getName())) {
                state = loadMos(siteId, "etamos");
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.GFS_MOS.getName())) {
                state = loadMos(siteId, "gfsmos");
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.GFSLAMP.getName())) {
                state = loadMos(siteId, "gfslamp");
            } else if (viewerData
                    .equals(PlotViewerCfg.ClassNames.NAM_WRF.getName())) {
                state = loadMos(siteId, "etabuf");
            } else {
                statusHandler.warn("Unknown class name: " + viewerData);
            }
            if (!state) {
                break;
            }
        }

        dataLoaded = state;
        return state;
    }

    /**
     * Prohibit cloning
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    private PythonScript initializePython() throws JepException {
        if ((includePath == null || includePath.isEmpty())
                && (filePath == null || filePath.isEmpty())) {
            IPathManager pm = PathManagerFactory.getPathManager();
            File runner = pm.getStaticFile("aviation/python/PlotEntry.py");
            LocalizationContext baseCtx = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            // need getFile to pull whole python dir from localization server
            pm.getLocalizationFile(baseCtx, "aviation/python").getFile();
            filePath = runner.getPath();
            includePath = PyUtil.buildJepIncludePath(
                    runner.getParentFile().getPath(),
                    AvnPyUtil.getLoggingHandlerDir(),
                    AvnPyUtil.getPointDataDir(),
                    AvnPyUtil.getCommonPythonDir());
        }
        JepConfig config = new JepConfig().setIncludePath(includePath)
                .setClassLoader(WeatherPlotDataManager.class.getClassLoader());
        return new PythonScript(config, filePath);
    }

    @SuppressWarnings("unchecked")
    private boolean loadTafs(String siteId) {
        try (PythonScript python = initializePython()) {
            TafRecord[] tafs = TafUtil.getLatestTafs(siteId, 99);
            long keyTime = 0;

            if (tafs != null) {
                List<TafRecord> tlist = Arrays.asList(tafs);
                Collections.reverse(tlist);
                tlist.toArray(tafs);
                for (TafRecord taf : tafs) {
                    Map<String, Object> args = new HashMap<>();
                    args.put("taf", TafUtil.safeFormatTaf(taf, false));
                    args.put("wmoHeader", taf.getWmoHeader());
                    Object obj = python.execute("decodeTaf", args);
                    if (obj != null) {
                        Map<String, Object> dcdTaf = (Map<String, Object>) obj;
                        Map<String, Object> dcd = (Map<String, Object>) dcdTaf
                                .get("dcd");
                        Map<String, Object> itimeObj = (Map<String, Object>) dcd
                                .get("itime");
                        long itime = ((Number) itimeObj.get("value"))
                                .longValue();
                        List<Map<String, Object>> groups = (List<Map<String, Object>>) dcd
                                .get("group");
                        List<Map<String, Object>> list = new ArrayList<>();

                        for (Map<String, Object> group : groups) {
                            Map<String, Object> prev = (Map<String, Object>) group
                                    .get("prev");
                            Map<String, Object> sky = (Map<String, Object>) prev
                                    .get("sky");
                            Object cig = sky.get("cig");
                            Map<String, Object> vsby = (Map<String, Object>) prev
                                    .get("vsby");
                            Object vis = vsby.get("value");
                            Map<String, Object> time = (Map<String, Object>) prev
                                    .get("time");
                            Long from = ((Number) time.get("from")).longValue()
                                    * 1000;
                            Long to = ((Number) time.get("to")).longValue()
                                    * 1000;
                            Map<String, Object> wind = (Map<String, Object>) prev
                                    .get("wind");
                            Object ff = wind.get("ff");

                            Map<String, Object> tmp = new HashMap<>();
                            tmp.put("to", to);
                            tmp.put("from", from);
                            tmp.put("cig", cig);
                            tmp.put("vis", vis);
                            tmp.put("ff", ff);

                            Object ddObj = wind.get("dd");
                            tmp.put("dd", ddObj);

                            list.add(tmp);
                        }

                        dcdTafs.put(taf.getWmoHeader(), list);
                        tafLabels.add(taf.getWmoHeader());

                        if (selectedTafKey.isEmpty()) {
                            selectedTafKey = taf.getWmoHeader();
                            keyTime = itime;
                        } else if (itime > keyTime) {
                            selectedTafKey = taf.getWmoHeader();
                            keyTime = itime;
                        }
                    }
                }
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private boolean loadMetars(String siteId) {
        String metarSize = String.valueOf(
                wxPlotCfg.getHoursBack() + 1 + wxPlotCfg.getHoursForeward());
        String tag = MetarCacheGuidanceRequest.getTag(siteId, metarSize);
        byte[] siteObj = PythonCacheGuidanceJob.getInstance().getSiteObj(siteId,
                tag);
        if (siteObj == null) {
            return false;
        }
        try (PythonScript python = initializePython()) {
            Map<String, Object> args = new HashMap<>();
            args.put("siteObj", siteObj);
            args.put("size", 99);
            Object obj = python.execute("getMetars", args);

            if (obj != null) {
                List<Map<String, Object>> mtrs = (List<Map<String, Object>>) obj;

                for (Map<String, Object> mtr : mtrs) {
                    Map<String, Object> dcd = (Map<String, Object>) mtr
                            .get("dcd");
                    Map<String, Object> itime = (Map<String, Object>) dcd
                            .get("itime");
                    Map<String, Object> vsby = (Map<String, Object>) dcd
                            .get("vsby");
                    Map<String, Object> sky = (Map<String, Object>) dcd
                            .get("sky");
                    Map<String, Object> wind = (Map<String, Object>) dcd
                            .get("wind");
                    Object vis = vsby.get("value");
                    Object cig;

                    if (sky != null) {
                        cig = sky.get("cig");
                    } else {
                        cig = 99_999;
                    }

                    Long from = ((Number) itime.get("value")).longValue()
                            * 1000;
                    Object ff;

                    if (wind != null) {
                        ff = wind.get("ff");
                    } else {
                        ff = null;
                    }

                    if (from < tStart) {
                        continue;
                    }

                    Map<String, Object> tmp = new HashMap<>();
                    tmp.put("from", from);
                    tmp.put("cig", cig);
                    tmp.put("vis", vis);
                    tmp.put("ff", ff);

                    if (wind != null) {
                        Object ddObj = wind.get("dd");
                        tmp.put("dd", ddObj);
                    }

                    dcdMtrs.add(tmp);
                }
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return true;
    }

    private boolean loadMos(String siteId, String model) {
        String tag = MosCacheGuidanceRequest.getTag(siteId, model, "current");
        byte[] siteObj = PythonCacheGuidanceJob.getInstance().getSiteObj(siteId,
                tag);
        if (siteObj == null) {
            return false;
        }

        String type = modelTypes.get(model);
        String[] refTimes = getRefTimes(siteId, type);

        Map<String, Map<String, Object>> dataMap = new HashMap<>();
        String selectedTime = null;
        for (String rt : refTimes) {
            Map<String, Object> cacheMap = new HashMap<>();
            String[] dateTime = rt.split(" ");
            String[] dateSplit = dateTime[0].split("-");
            String[] timeSplit = dateTime[1].split(":");
            String time = dateSplit[0] + dateSplit[1] + dateSplit[2]
                    + timeSplit[0] + timeSplit[1];
            if (selectedTime == null) {
                cacheMap.put("refTimeStr", "current");
                selectedTime = time;
            } else {
                cacheMap.put("refTimeStr", rt);
            }

            cacheMap.put("siteId", siteId);
            cacheMap.put("model", model);
            dataMap.put(time, cacheMap);

            CacheGuidanceRequest req = checkCacheMos(cacheMap);
            if (req != null) {
                PythonCacheGuidanceJob.getInstance().enqueue(req);
            }
        }

        if ("etamos".equals(model)) {
            namMosData = dataMap;
            selectedNamMosKey = selectedTime;
        } else if ("gfsmos".equals(model)) {
            gfsMosData = dataMap;
            selectedGfsMosKey = selectedTime;
        } else if ("gfslamp".equals(model)) {
            gfsLampData = dataMap;
            selectedGfsLampKey = selectedTime;
        } else if ("etabuf".equals(model)) {
            namWrfData = dataMap;
            selectedNamKey = selectedTime;
        } else {
            statusHandler.warn("Unknown handler model: " + model);
        }
        return true;
    }

    /**
     * Format Mos Data into the form needed to generate the plots.
     *
     * @param siteObj
     *            - Python pickle string with the data to be formatted
     * @param model
     * @return data
     */
    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> formatMosData(byte[] siteObj,
            String model) {
        List<Map<String, Object>> mosData = new ArrayList<>();
        try (PythonScript python = initializePython()) {
            Map<String, Object> args = new HashMap<>();
            args.put("siteObj", siteObj);
            args.put("model", model);
            Object retObj = python.execute("getMos", args);
            if (retObj != null) {
                List<Object> resultList = (List<Object>) retObj;
                for (Object obj : resultList) {
                    Map<String, Object> results = (Map<String, Object>) obj;
                    Object dataObj = results.get("data");

                    if (dataObj instanceof Map) {
                        Map<String, Object> data = (Map<String, Object>) dataObj;
                        List<Map<String, Object>> group = (List<Map<String, Object>>) data
                                .get("group");
                        Map<String, Object> itimeMap = (Map<String, Object>) data
                                .get("itime");
                        Long itime = ((Number) itimeMap.get("value"))
                                .longValue() * 1000;
                        Calendar c = Calendar
                                .getInstance(TimeZone.getTimeZone("GMT"));
                        c.setTimeInMillis(itime);

                        for (Map<String, Object> record : group) {
                            Map<String, Object> time = (Map<String, Object>) record
                                    .get("time");
                            Map<String, Object> sky = (Map<String, Object>) record
                                    .get("sky");
                            Map<String, Object> vsby = (Map<String, Object>) record
                                    .get("vsby");
                            Map<String, Object> wind = (Map<String, Object>) record
                                    .get("wind");
                            Object vis = vsby.get("value");
                            Object cig = sky.get("cig");

                            Long from = ((Number) time.get("from")).longValue()
                                    * 1000;
                            Long to = ((Number) time.get("to")).longValue()
                                    * 1000;
                            Object ff = wind.get("ff");

                            if (to > tEnd) {
                                continue;
                            }

                            if (from < tStart) {
                                continue;
                            }

                            Map<String, Object> tmp = new HashMap<>();
                            tmp.put("from", from);
                            tmp.put("to", to);
                            tmp.put("cig", cig);
                            tmp.put("vis", vis);
                            tmp.put("ff", ff);

                            Object ddObj = wind.get("dd");
                            String ddStr;
                            Integer ddInt;

                            if (ddObj instanceof String) {
                                ddStr = (String) ddObj;
                                tmp.put("dd", ddStr);
                            } else if (ddObj instanceof Number) {
                                ddInt = ((Number) ddObj).intValue();
                                tmp.put("dd", ddInt);
                            } else if (ddObj != null) {
                                statusHandler.error("Unknown type of dd: "
                                        + ddObj.getClass().getName());
                            }

                            mosData.add(tmp);
                        }
                    }
                }
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return mosData;
    }

    /**
     * Format Nam Data into the form needed to generate the plots.
     *
     * @param siteObj
     *            - Python pickle string with the data to be formatted
     * @return data
     */
    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> formatNamData(byte[] siteObj) {
        List<Map<String, Object>> namDataList = new ArrayList<>();
        try (PythonScript python = initializePython()) {
            Map<String, Object> args = new HashMap<>();
            args.put("siteObj", siteObj);
            Object retObj = python.execute("getNam", args);
            if (retObj != null) {
                List<Object> resultList = (List<Object>) retObj;

                for (Object obj : resultList) {
                    if (obj instanceof Map) {
                        Map<String, Object> eta = (Map<String, Object>) obj;
                        Object dataObj = eta.get("data");
                        if (!(dataObj instanceof Map<?, ?>)) {
                            break;
                        }
                        Map<String, Object> data = (Map<String, Object>) dataObj;
                        List<Map<String, Object>> group = (List<Map<String, Object>>) data
                                .get("group");
                        if (group == null) {
                            break;
                        }

                        for (Map<String, Object> record : group) {
                            Map<String, Object> time = (Map<String, Object>) record
                                    .get("time");
                            Map<String, Object> sky = (Map<String, Object>) record
                                    .get("sky");
                            Map<String, Object> vsby = (Map<String, Object>) record
                                    .get("vsby");
                            Map<String, Object> wind = (Map<String, Object>) record
                                    .get("wind");
                            Object vis = vsby.get("value");
                            Object cig = sky.get("cig");

                            Long from = ((Number) time.get("from")).longValue()
                                    * 1000;
                            Long to = ((Number) time.get("to")).longValue()
                                    * 1000;
                            Object ff = wind.get("ff");

                            if (to > tEnd) {
                                continue;
                            }

                            if (from < tStart) {
                                continue;
                            }

                            Map<String, Object> tmp = new HashMap<>();
                            tmp.put("from", from);
                            tmp.put("to", to);
                            tmp.put("cig", cig);
                            tmp.put("vis", vis);
                            tmp.put("ff", ff);

                            Object ddObj = wind.get("dd");
                            tmp.put("dd", ddObj);

                            namDataList.add(tmp);
                        }
                    }
                }
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return namDataList;
    }

    public String[] getTafHeaders() {
        // tafLabels already in the desired order.
        String[] headers = new String[tafLabels.size()];
        tafLabels.toArray(headers);
        return headers;
    }

    public List<Map<String, Object>> getTafs() {
        return dcdTafs.get(selectedTafKey);
    }

    public List<Map<String, Object>> getMetars() {
        return dcdMtrs;
    }

    public String getSelectedTaf() {
        return selectedTafKey;
    }

    public boolean havePendingCache() {
        return !cacheRequests.isEmpty();
    }

    /**
     * Update the selections and generate list of requests needed for the plots.
     *
     * @param taf
     * @param namMos
     * @param gfsMos
     * @param gfsLamp
     * @param namWrf
     */
    public void updateSelections(String taf, String namMos, String gfsMos,
            String gfsLamp, String namWrf) {
        List<CacheGuidanceRequest> cacheRequests = new ArrayList<>();
        CacheGuidanceRequest req = null;
        selectedTafKey = taf;

        selectedNamMosKey = namMos;
        if (namMos != null) {
            req = checkCacheMos(namMosData.get(namMos));
            if (req != null) {
                cacheRequests.add(req);
            }
        }

        selectedGfsMosKey = gfsMos;
        if (gfsMos != null) {
            req = checkCacheMos(gfsMosData.get(gfsMos));
            if (req != null) {
                cacheRequests.add(req);
            }
        }

        selectedGfsLampKey = gfsLamp;
        if (gfsLamp != null) {
            req = checkCacheMos(gfsLampData.get(gfsLamp));
            if (req != null) {
                cacheRequests.add(req);
            }
        }

        selectedNamKey = namWrf;
        if (namWrf != null) {
            req = checkCacheMos(namWrfData.get(namWrf));
            if (req != null) {
                cacheRequests.add(req);
            }
        }
        this.cacheRequests = cacheRequests;
    }

    /**
     * This waits for plot data to be cached and formats if for display.
     */
    public void waitForCacheRequests() {
        PythonCacheGuidanceJob.getInstance()
                .waitForCacheRequests(cacheRequests);
        checkCacheMos(namMosData.get(selectedNamMosKey));
        checkCacheMos(gfsMosData.get(selectedGfsMosKey));
        checkCacheMos(gfsLampData.get(selectedGfsLampKey));
        checkCacheMos(namWrfData.get(selectedNamKey));
        cacheRequests.clear();
    }

    public String getSelectedNamMos() {
        return selectedNamMosKey;
    }

    public String getSelectedGfsMos() {
        return selectedGfsMosKey;
    }

    public String getSelectedGfsLamp() {
        return selectedGfsLampKey;
    }

    public List<Map<String, Object>> getWrfNam() {
        return getSelectedMosData(selectedNamKey, namWrfData);
    }

    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> getSelectedMosData(String time,
            Map<String, Map<String, Object>> dataMaps) {

        if (dataMaps != null) {
            Map<String, Object> cacheMap = dataMaps.get(time);

            if (cacheMap != null) {
                Object obj = cacheMap.get("data");
                if (obj instanceof List<?>) {
                    return (List<Map<String, Object>>) obj;
                }
            }
        }
        return null;
    }

    public List<Map<String, Object>> getNamMos() {
        return getSelectedMosData(selectedNamMosKey, namMosData);
    }

    public List<Map<String, Object>> getGfsMos() {
        return getSelectedMosData(selectedGfsMosKey, gfsMosData);
    }

    public List<Map<String, Object>> getGfsLamp() {
        return getSelectedMosData(selectedGfsLampKey, gfsLampData);
    }

    public String[] getNamMosTimes() {
        Set<String> keySet = namMosData.keySet();
        String keys[] = new String[keySet.size()];
        keySet.toArray(keys);
        Arrays.sort(keys, Collections.reverseOrder());
        return keys;
    }

    public String[] getGfsMosTimes() {
        Set<String> keySet = gfsMosData.keySet();
        String keys[] = new String[keySet.size()];
        keySet.toArray(keys);
        Arrays.sort(keys, Collections.reverseOrder());
        return keys;
    }

    public String[] getGfsLampTimes() {
        Set<String> keySet = gfsLampData.keySet();
        String keys[] = new String[keySet.size()];
        keySet.toArray(keys);
        Arrays.sort(keys, Collections.reverseOrder());
        return keys;
    }

    public String getselectedNamWrf() {
        return selectedNamKey;
    }

    public String[] getNamWrfTimes() {
        Set<String> keySet = namWrfData.keySet();
        String keys[] = new String[keySet.size()];
        keySet.toArray(keys);
        Arrays.sort(keys, Collections.reverseOrder());
        return keys;
    }

    public WxPlotCfg getWxPlotCfg() {
        return wxPlotCfg;
    }

    public boolean dataLoaded() {
        return dataLoaded;
    }

    private String[] getRefTimes(String siteId, String type) {
        String fieldName = "dataTime.refTime";
        Map<String, RequestConstraint> queryTerms = new HashMap<>();
        queryTerms.put("location.stationId", new RequestConstraint(siteId));

        if (!"ETA".equals(type)) {
            queryTerms.put("pluginName",
                    new RequestConstraint("bufrmos" + type));
        } else {
            queryTerms.put("pluginName",
                    new RequestConstraint("modelsounding"));
            queryTerms.put("reportType", new RequestConstraint(type));
        }
        String[] result = new String[] {};

        try {
            result = CatalogQuery.performQuery(fieldName, queryTerms);
            Arrays.sort(result, Collections.reverseOrder());
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return result;
    }
}
