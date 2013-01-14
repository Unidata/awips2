package com.raytheon.uf.viz.monitor.ffmp;

import java.io.File;
import java.io.FileNotFoundException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentNavigableMap;
import java.util.concurrent.ConcurrentSkipListMap;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPCacheRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates.MODE;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.ResourceMonitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPSplash;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpBasinTableDlg;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData;
import com.raytheon.uf.viz.monitor.ffmp.ui.listeners.IFFMPResourceListener;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPDataLoader;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPDataLoader.LOADER_TYPE;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResource;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResourceData;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPTimeWindow;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;

/**
 * Monitor implementation for FFMP
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/03/10     4494        D. Hladky   Initial release
 * 12/07/12     1353        rferrel     Changes for non-blocking FFMPSplash.
 * 01/10/13     1475        D. Hladky   Cleaned up some logging.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPMonitor extends ResourceMonitor {
    private static long SECONDS_PER_HOUR = 60 * 60;

    /** boolean for initialization **/
    public static boolean isInitialized = false;

    /** Singleton instance of this class */
    private static FFMPMonitor monitor = null;

    /** Array of scan listeners **/
    private ArrayList<IFFMPResourceListener> resourceListeners = new ArrayList<IFFMPResourceListener>();

    public FFMPSplash ffmpSplash;

    private String wfo = null;

    /** Pattern for dates in radar */
    public static String datePattern = "yyyy-MM-dd HH:mm:ss";

    /** FFMP Records indexed by site, times and field **/
    public ConcurrentHashMap<String, ConcurrentHashMap<String, FFMPCacheRecord>> ffmpData = null;

    // list of earliest available date queried by site, sourceName
    public ConcurrentHashMap<String, ConcurrentHashMap<String, Date>> ffmpAvailableUriQueryDates = null;

    // map by site, sourceName, Date
    public ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentSkipListMap<Date, List<String>>>> ffmpAvailableUris = null;

    // map by field, huc, and the URIs that have been loaded for
    public ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>> ffmpLoadedUris = null;

    // Interpolation Guidance Sources
    public FFMPGuidanceInterpolation interpolation = null;

    /** config manager **/
    public FFMPSourceConfigurationManager fscm = null;

    /** config manager **/
    public FFMPConfig ffmpConfig = null;

    public ArrayList<Date> dataTimes = null;

    private FFMPTimeWindow qpfWindow = null;

    private FFMPTimeWindow qpeWindow = null;

    /** The infamous templates **/
    private FFMPTemplates templates = null;

    private FFMPRunConfigurationManager frcm = null;

    private FFFGDataMgr fffg = null;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPMonitor.class);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ResourceMonitor#nullifyMonitor()
     */
    @Override
    public void nullifyMonitor() {

        if (resourceListeners != null) {
            for (IFFMPResourceListener listener : resourceListeners) {
                if (listener instanceof FFMPResource) {
                    FFMPResource res = (FFMPResource) listener;
                    if (res.basinTableDlg != null) {
                        closeDialog(res);
                    }

                    // will kill any loaders running
                    if (res.getResourceData().floader != null) {
                        res.getResourceData().floader.kill();
                        res.getResourceData().floader = null;
                    }
                }
            }

            // clear the resource list
            resourceListeners.clear();
        }

        // clear all remaining data
        if (ffmpData != null) {
            for (String key : ffmpData.keySet()) {
                for (Entry<String, FFMPCacheRecord> entry : ffmpData.get(key)
                        .entrySet()) {
                    entry.getValue().closeCache();
                }
            }
        }

        ffmpData = null;
        ffmpAvailableUriQueryDates = null;
        ffmpAvailableUris = null;
        ffmpLoadedUris = null;

        // kill this monitor
        monitor = null;
        System.gc();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ResourceMonitor#thresholdUpdate(com.raytheon
     * .uf.viz.monitor.events.IMonitorThresholdEvent)
     */
    @Override
    public void thresholdUpdate(IMonitorThresholdEvent me) {
        // TODO Auto-generated method stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ResourceMonitor#configUpdate(com.raytheon
     * .uf.viz.monitor.events.IMonitorConfigurationEvent)
     */
    @Override
    public void configUpdate(IMonitorConfigurationEvent me) {
        // updates the config
        getSourceConfig().readConfigXml();
        boolean isLinkToFrame = getConfig().getFFMPConfigData()
                .getLinkToFrame();

        for (IFFMPResourceListener listener : resourceListeners) {

            listener.setLinkToFrame(isLinkToFrame);

            if (listener.isAutoRefresh()) {
                listener.setQuery(true);
                fireRefresh(listener);
            }

            listener.updateDialog();
        }
    }

    /**
     * Actual initialization if necessary
     * 
     * @return
     */
    public static synchronized FFMPMonitor getInstance() {
        if (monitor == null) {
            monitor = new FFMPMonitor();
            monitor.createDataStructures();
            isInitialized = true;
        }

        return monitor;
    }

    /**
     * check for life
     * 
     * @return
     */
    public static boolean isRunning() {

        if (monitor == null) {
            return false;
        }

        return true;
    }

    /**
     * get a ref to the config
     * 
     * @return
     */
    public FFMPSourceConfigurationManager getSourceConfig() {
        if (fscm == null) {
            fscm = FFMPSourceConfigurationManager.getInstance();
            fscm.readConfigXml();
        }

        return fscm;
    }

    /**
     * Gets the FFFG manager
     * 
     * @return
     */
    public FFFGDataMgr getFFFGConfig() {
        if (fffg == null) {
            fffg = FFFGDataMgr.getInstance();
        }

        return fffg;
    }

    /**
     * Gets the dialog config
     * 
     * @return
     */
    public FFMPConfig getConfig() {
        if (ffmpConfig == null) {
            ffmpConfig = FFMPConfig.getInstance();
        }
        return ffmpConfig;
    }

    /**
     * Creates the linked maps
     */
    private void createDataStructures() {
        ffmpData = new ConcurrentHashMap<String, ConcurrentHashMap<String, FFMPCacheRecord>>();
        ffmpAvailableUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentSkipListMap<Date, List<String>>>>();
        ffmpAvailableUriQueryDates = new ConcurrentHashMap<String, ConcurrentHashMap<String, Date>>();
        ffmpLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>>();
    }

    /**
     * gets the URI's by field type and site
     * 
     * @param siteKey
     * @param pfield
     * @param phuc
     * @return
     */
    private ConcurrentHashMap<String, String> getUriMap(String siteKey,
            String source, String phuc) {

        ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>> siteLoadedUris = null;

        if (ffmpLoadedUris != null) {
            siteLoadedUris = ffmpLoadedUris.get(siteKey);
        }

        if (siteLoadedUris == null) {
            siteLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>();
            ffmpLoadedUris.put(siteKey, siteLoadedUris);
        }

        ConcurrentHashMap<String, ConcurrentHashMap<String, String>> sourceLoadedUris = null;

        if (ffmpLoadedUris != null) {
            sourceLoadedUris = ffmpLoadedUris.get(siteKey).get(source);
        }

        if (sourceLoadedUris == null) {

            sourceLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>();
            ffmpLoadedUris.get(siteKey).put(source, sourceLoadedUris);
        }

        ConcurrentHashMap<String, String> hucLoadedUris = null;

        if (ffmpLoadedUris != null) {
            hucLoadedUris = ffmpLoadedUris.get(siteKey).get(source).get(phuc);
        }

        if (hucLoadedUris == null) {

            hucLoadedUris = new ConcurrentHashMap<String, String>(200, 0.75f, 4);
            if (ffmpLoadedUris != null) {
                ffmpLoadedUris.get(siteKey).get(source)
                        .put(phuc, hucLoadedUris);
            }
        }

        return hucLoadedUris;
    }

    /**
     * Static to make it fast, at least that's the idea.
     * 
     * @param uri
     * @return
     * @throws VizException
     */
    private static FFMPRecord loadRecordFromDatabase(String uri)
            throws VizException {
        FFMPRecord ffmpRec = null;

        if (uri != null) {
            Map<String, Object> vals = new HashMap<String, Object>();
            vals.put("pluginName", "ffmp");
            vals.put("dataURI", uri);
            ffmpRec = (FFMPRecord) Loader.loadData(vals);
        }

        return ffmpRec;
    }

    /**
     * populate a new FFMPRecord
     * 
     * @param sourceName
     * @param icao
     * @param refTime
     * @return
     * @throws VizException
     */
    public void populateFFMPRecord(ProductXML product, String siteKey,
            String dataKey, String source, Date ptime, String phuc,
            boolean retrieveNew) {

        boolean isProductLoad = false;
        if (product != null) {
            isProductLoad = true;
        }

        if (source != null) {

            boolean dupOverride = false;
            if (getSourceConfig().getSource(source).getSourceType()
                    .equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
                dupOverride = true;
            }

            SortedMap<Date, List<String>> urisByDate = getAvailableUris(
                    siteKey, dataKey, source, ptime, retrieveNew);

            if (urisByDate != null) {
                for (List<String> uris : urisByDate.values()) {
                    for (String uri : uris) {
                        if (uri != null) {
                            if (dupOverride
                                    || !getUriMap(siteKey, source, phuc)
                                            .containsKey(uri)) {
                                try {
                                    populateFFMPRecord(isProductLoad, siteKey,
                                            loadRecordFromDatabase(uri),
                                            source, phuc);
                                } catch (Exception e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            "FFMP Can't retrieve FFMP URI, "
                                                    + uri, e);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * populate a new FFMPRecord
     * 
     * @param sourceName
     * @param icao
     * @param refTime
     * @return
     * @throws VizException
     */
    public FFMPCacheRecord populateFFMPRecord(boolean isProductLoad,
            String uri, String siteKey, String source, String phuc)
            throws Exception {

        try {
            populateFFMPRecord(isProductLoad, siteKey,
                    loadRecordFromDatabase(uri), source, phuc);
        } catch (VizException e) {
            statusHandler.handle(Priority.INFO,
                    "FFMP Can't retrieve FFMP URI, " + uri, e);
        }

        return ffmpData.get(siteKey).get(source);
    }

    /**
     * Inserts the loader records directly into the cache
     * 
     * @param data
     * @param siteKey
     * @param dataKey
     * @param source
     * @param huc
     */
    public void insertFFMPData(FFMPBasinData data, String siteKey,
            String source, String huc) {

        final String fsiteKey = siteKey;
        final FFMPBasinData fdata = data;
        final String fsource = source;
        final String fhuc = huc;

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {

                if (ffmpData.containsKey(fsiteKey)) {
                    if (ffmpData.get(fsiteKey).containsKey(fsource)) {
                        ffmpData.get(fsiteKey).get(fsource)
                                .setBasinBuddyData(fdata, fhuc);
                    }
                }
            }
        });
    }

    /**
     * Try and get the loading off of the GUI thread
     * 
     * @param isProductLoad
     * @param siteKey
     * @param ffmpRec
     * @param source
     * @param phuc
     * @throws Exception
     */
    public void populateFFMPRecord(boolean isProductLoad, String siteKey,
            FFMPRecord ffmpRec, String source, String phuc) throws Exception {

        FFMPLoadRecord flr = new FFMPLoadRecord(isProductLoad, siteKey,
                ffmpRec, source, phuc);
        flr.run();
    }

    /**
     * Get load of basins off the main thread
     * 
     * @param dataUri
     * @param siteKey
     * @param source
     * @param phuc
     * @param basin
     * @throws VizException
     */
    public void populateFFMPBasin(String dataUri, String siteKey,
            String source, String phuc, FFMPBasin basin) throws VizException {

        final String fdataUri = dataUri;
        final String fsiteKey = siteKey;
        final String fhuc = phuc;
        final String fsource = source;
        final FFMPBasin fbasin = basin;

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {

                if (fdataUri != null) {
                    ConcurrentMap<String, String> uris = getUriMap(fsiteKey,
                            fsource, fhuc);
                    if (!uris.containsKey(fdataUri)) {
                        try {
                            SourceXML sourceXML = fscm.getSource(fsource);
                            FFMPCacheRecord ffmpRec = populateFFMPRecord(true,
                                    fdataUri, fsiteKey, fsource, fhuc);
                            // FFMPRecord ffmpRec =
                            // loadRecordFromDatabase(fdataUri);
                            File loc = HDF5Util.findHDF5Location(ffmpRec);
                            IDataStore dataStore = DataStoreFactory
                                    .getDataStore(loc);

                            if (sourceXML.getSourceType().equals(
                                    SOURCE_TYPE.GAGE.getSourceType())
                                    && fhuc.equals("ALL")) {
                                ffmpRec.retrieveVirtualBasinFromDataStore(
                                        dataStore, fdataUri,
                                        getTemplates(fsiteKey), ffmpRec
                                                .getDataTime().getRefTime(),
                                        fbasin);
                            } else {
                                ffmpRec.retrieveBasinFromDataStore(dataStore,
                                        fdataUri, getTemplates(fsiteKey), fhuc,
                                        ffmpRec.getDataTime().getRefTime(),
                                        ffmpRec.getSourceName(), fbasin);
                            }
                        } catch (Throwable e) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "FFMP Can't retrieve FFMP URI, "
                                                    + fdataUri, e);
                        }
                    }
                }
            }
        });
    }

    /**
     * Getting a specific URI
     * 
     * @param date
     * @return
     */
    public String getAvailableUri(String siteKey, String dataKey,
            String sourceName, Date ptime) {

        String uri = null;
        SimpleDateFormat datef = new SimpleDateFormat(datePattern);
        datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
        String sql = "select datauri from ffmp where wfo = '" + getWfo()
                + "' and reftime = '" + datef.format(ptime)
                + "' and sourcename = '" + sourceName + "' and sitekey = '"
                + siteKey + "' and datakey = '" + dataKey
                + "' order by reftime";
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    "metadata", QueryLanguage.SQL);
            if (results.size() > 0) {
                uri = (String) results.get(0)[0];
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        return uri;
    }

    /**
     * Get me the available URIS for this source
     * 
     * @param siteKey
     * @param dataKey
     * @param sourceName
     * @param time
     * @return
     */
    public ConcurrentNavigableMap<Date, List<String>> getAvailableUris(
            String siteKey, String dataKey, String sourceName, Date time) {

        ConcurrentNavigableMap<Date, List<String>> uris = null;

        try {
            uris = getAvailableUris(siteKey, dataKey, sourceName, time, false);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "FFMP Can't find availble URI list for, " + sourceName, e);
        }

        return uris;
    }

    /**
     * Gets the available uris back to a given time
     * 
     * @param date
     * @return
     */
    public ConcurrentNavigableMap<Date, List<String>> getAvailableUris(
            String siteKey, String dataKey, String sourceName, Date time,
            boolean retrieveNew) {

        if (!ffmpAvailableUris.containsKey(siteKey)) {
            ConcurrentHashMap<String, ConcurrentSkipListMap<Date, List<String>>> tempUriList = new ConcurrentHashMap<String, ConcurrentSkipListMap<Date, List<String>>>(
                    16, 0.75f, 4);
            ffmpAvailableUris.put(siteKey, tempUriList);
        }

        ConcurrentSkipListMap<Date, List<String>> sortedUris = ffmpAvailableUris
                .get(siteKey).get(sourceName);
        if (sortedUris == null) {
            synchronized (ffmpAvailableUris) {
                // double check in case another thread created in the mean time
                sortedUris = ffmpAvailableUris.get(siteKey).get(sourceName);
                if (sortedUris == null) {
                    sortedUris = new ConcurrentSkipListMap<Date, List<String>>();
                    ffmpAvailableUris.get(siteKey).put(sourceName, sortedUris);
                }
            }
        }

        if (!ffmpAvailableUriQueryDates.containsKey(siteKey)) {
            ConcurrentHashMap<String, Date> tempUriQueryDates = new ConcurrentHashMap<String, Date>(
                    16, 0.75f, 4);
            ffmpAvailableUriQueryDates.put(siteKey, tempUriQueryDates);
        }

        Date previousQueryTime = ffmpAvailableUriQueryDates.get(siteKey).get(
                sourceName);
        SourceXML source = getSourceConfig().getSource(sourceName);

        if (source.getSourceType().equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
            // Always look back for guidance types because of long expiration
            // times,
            // prevents mosaic brittleness from occurring.
            retrieveNew = true;
        }

        if (retrieveNew
                || ((time != null) && ((previousQueryTime == null) || (time
                        .getTime() < previousQueryTime.getTime())))) {
            SimpleDateFormat datef = new SimpleDateFormat(datePattern);
            datef.setTimeZone(TimeZone.getTimeZone("Zulu"));
            Date earliestTime = time;
            StringBuilder query = new StringBuilder(200);
            query.append("select datauri from ffmp where wfo = '");
            query.append(getWfo());
            query.append("' and sourcename = '");
            query.append(sourceName);

            // GUIDANCE we save by displayName, *type*
            if (source.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {

                long timeOffset = source.getExpirationMinutes(siteKey) * TimeUtil.MILLIS_PER_MINUTE;
                earliestTime = new Date(time.getTime() - timeOffset);
            }

            query.append("' and sitekey = '");
            query.append(siteKey);
            if (!source.isMosaic()) {
                query.append("' and datakey = '");
                query.append(dataKey);
            }

            query.append("' and reftime >= '");
            query.append(datef.format(earliestTime));

            if (!retrieveNew && (previousQueryTime != null)) {
                query.append("' and reftime < '");
                query.append(datef.format(previousQueryTime));
            }

            query.append("' order by reftime desc");

            try {
                List<Object[]> results = DirectDbQuery.executeQuery(
                        query.toString(), "metadata", QueryLanguage.SQL);
                List<String> list = new LinkedList<String>();
                Date prevRefTime = null;

                for (int j = 0; j < results.size(); j++) {
                    if (results.size() > 0) {
                        Object[] results2 = results.get(j);
                        // System.out.println("Querrying for URIs: "
                        // + query.toString() + " # " + results2.length);

                        for (int i = 0; i < results2.length; i++) {
                            String uri = (String) results2[0];
                            FFMPRecord rec = new FFMPRecord(uri);
                            Date curRefTime = rec.getDataTime().getRefTime();
                            if ((prevRefTime != null)
                                    && !prevRefTime.equals(curRefTime)) {
                                sortedUris.put(prevRefTime, list);
                                list = new LinkedList<String>();
                            }

                            prevRefTime = curRefTime;
                            list.add(uri);
                        }
                    }
                }

                if (list != null) {
                    if ((prevRefTime == null) || (list == null)) {
                        statusHandler.handle(Priority.WARN,
                                "Source prevTime or list = null: " + sourceName
                                        + " Date: " + time);
                    } else {
                        sortedUris.put(prevRefTime, list);
                    }
                }

                ffmpAvailableUriQueryDates.get(siteKey).put(sourceName, time);

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "FFMP Can't find availble URI list for, " + sourceName,
                        e);
            }
        }

        if (time != null) {
            if (source.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                return sortedUris;
            } else {
                return sortedUris.tailMap(time, true);
            }
        }

        return null;
    }

    /**
     * Request a record
     * 
     * @param retrieveNew
     * 
     * @param refTime
     * @param field
     * @param huc
     * @return
     * 
     */
    public FFMPCacheRecord getFFMPData(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date ptime, String phuc,
            boolean retrieveNew) {

        FFMPCacheRecord record = ffmpData.get(siteKey).get(sourceName);
        String guidSrc = FFMPConfig.getInstance().getFFMPConfigData()
                .getGuidSrc();

        if ((record != null)
                && (record.getBasinData(phuc).getBasins().size() > 0)) {

            SourceXML sourceXML = getSourceConfig().getSource(sourceName);

            if (sourceXML == null) {
                sourceXML = getSourceConfig()
                        .getSourceByDisplayName(sourceName);
            }

            if (sourceXML.getSourceType().equals(
                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                // FFG in table special case where display is the sourceName
                if (product != null) {

                    ProductRunXML productRun = getRunConfig().getProduct(
                            siteKey);

                    for (SourceXML source : productRun.getGuidanceSources(
                            product, guidSrc)) {
                        if (ffmpLoadedUris.get(siteKey).containsKey(
                                source.getSourceName())) {
                            continue;
                        } else {
                            populateFFMPRecord(product, siteKey, dataKey,
                                    source.getSourceName(), ptime, phuc,
                                    retrieveNew);
                        }
                    }
                } else {
                    // FFG is the primary if
                    if (!ffmpLoadedUris.get(siteKey).containsKey(sourceName)) {
                        populateFFMPRecord(product, siteKey, dataKey,
                                sourceName, ptime, phuc, retrieveNew);
                    }
                }

                record = ffmpData.get(siteKey).get(sourceName);

            } else {
                populateFFMPRecord(product, siteKey, dataKey, sourceName,
                        ptime, phuc, retrieveNew);
            }

        } else { // must populate for a different huc for all possible times
            if (product != null) {

                SourceXML sourceXML = getSourceConfig().getSourceByDisplayName(
                        sourceName);

                if (sourceXML != null) {

                    if (sourceXML.getDisplayName().equals(sourceName)) {
                        // FFG table display special case updates
                        ProductRunXML productRun = getRunConfig().getProduct(
                                siteKey);

                        if (sourceXML
                                .getSourceType()
                                .equals(FFMPSourceConfigurationManager.SOURCE_TYPE.GUIDANCE
                                        .getSourceType())) {
                            for (SourceXML ffgSource : productRun
                                    .getGuidanceSources(product, guidSrc)) {

                                populateFFMPRecord(product, siteKey, dataKey,
                                        ffgSource.getSourceName(), ptime, phuc,
                                        retrieveNew);
                            }
                        } else {
                            populateFFMPRecord(product, siteKey, dataKey,
                                    sourceName, ptime, phuc, retrieveNew);
                        }
                    }
                } else {
                    populateFFMPRecord(product, siteKey, dataKey, sourceName,
                            ptime, phuc, retrieveNew);
                }
            } else {
                // special case where FFG is the primary source
                // check for special case with dual stand alone and table
                // display loaded

                SourceXML sourcexml = getSourceConfig().getSource(sourceName);

                if (sourcexml.getSourceType().equals(
                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                    sourceName = sourcexml.getDisplayName();
                } else {
                    populateFFMPRecord(product, siteKey, dataKey, sourceName,
                            ptime, phuc, retrieveNew);
                }

            }

            record = ffmpData.get(siteKey).get(sourceName);
        }

        return record;

    }

    /**
     * Sort by DataTime
     * 
     * @author dhladky
     * 
     */
    public class SortByDataTime implements Comparator<DataTime> {

        @Override
        public int compare(DataTime o1, DataTime o2) {

            return o1.compareTo(o2);
        }
    }

    /**
     * gets the wfo
     * 
     * @return
     */
    public String getWfo() {
        return wfo;
    }

    /**
     * sets the wfo
     * 
     * @param wfo
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * gets the reverse of the later
     * 
     * @param sourceName
     * @return
     */
    public FIELDS getField(String sourceName) {

        String sfield = getSourceConfig().getSourceType(sourceName)
                .getSourceType().toLowerCase();
        FIELDS myField = null;
        if (sfield.equals(FFMPRecord.FIELDS.QPE.getFieldName())) {
            myField = FFMPRecord.FIELDS.QPE;
        } else if (sfield.equals(FFMPRecord.FIELDS.RATE.getFieldName())) {
            myField = FFMPRecord.FIELDS.RATE;
        } else if (sfield.equals(FFMPRecord.FIELDS.QPF.getFieldName())) {
            myField = FFMPRecord.FIELDS.QPF;
        } else if (sfield.equals(FFMPRecord.FIELDS.GUIDANCE.getFieldName())) {
            myField = FFMPRecord.FIELDS.GUIDANCE;
        } else if (sfield.equals("gage")) {
            myField = FFMPRecord.FIELDS.VIRTUAL;
        }
        return myField;
    }

    /**
     * Start secondary and tertiary data loads
     * 
     * @param startTime
     * @param loadType
     * @throws VizException
     */
    public void startLoad(FFMPResource resource, Date startTime,
            LOADER_TYPE loadType) throws VizException {
        Date timeBack = null;
        FFMPTemplateConfigurationManager ftcm = FFMPTemplateConfigurationManager
                .getInstance();
        ArrayList<String> hucsToLoad = ftcm.getHucLevels();
        FFMPResourceData frd = resource.getResourceData();

        if (loadType == LOADER_TYPE.SECONDARY) {
            // hucsToLoad.remove("ALL");
            // hucsToLoad.remove(getConfig().getFFMPConfigData().getLayer());
            timeBack = new Date(resource.getMostRecentTime().getTime()
                    - (6 * 1000 * 24));
            frd.timeBack = timeBack;
        } else if (loadType == LOADER_TYPE.TERTIARY) {
            hucsToLoad.clear();
            hucsToLoad.add("ALL");
            timeBack = new Date(resource.getMostRecentTime().getTime()
                    - (TimeUtil.MILLIS_PER_HOUR * 24));
        }

        frd.floader = new FFMPDataLoader(frd, timeBack, startTime, loadType,
                hucsToLoad);
        frd.floader.addListener(resource);
        frd.floader.start();

    }

    public void launchSplash(String siteKey) {

        final String fsiteKey = siteKey;

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                Shell fshell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();

                if (ffmpSplash == null) {
                    ffmpSplash = new FFMPSplash(fshell);
                    ffmpSplash.open();
                    // latch
                    int count = 0;
                    while (!getTemplates(fsiteKey).done) {

                        try {
                            count++;
                            if (count == 50) {
                                ffmpSplash.close();
                                break;
                            }
                            Thread.sleep(1000);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                            if (ffmpSplash != null) {
                                ffmpSplash.close();
                            }
                        }
                    }
                }
            }
        });
    }

    /**
     * Launch a basin trend from a screen click
     * 
     * @param pfaf
     */
    public void basinTrend(Long pfaf) {

        final String pfafs = pfaf.toString();

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                for (IMonitorListener listener : getMonitorListeners()) {
                    if (listener instanceof FfmpBasinTableDlg) {
                        ((FfmpBasinTableDlg) listener).displayBasinTrend(pfafs);
                    }
                }
            }
        });
    }

    /**
     * launch the dialog
     */
    public void launchFFMPDialog(FFMPResource resource) {

        final FFMPResource fresource = resource;

        if (resource.basinTableDlg == null) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    Shell fshell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();

                    FFMPTableData tData = new FFMPTableData();
                    fresource.basinTableDlg = new FfmpBasinTableDlg(fshell,
                            tData, fresource);
                    addMonitorListener(fresource.basinTableDlg);
                    fresource.basinTableDlg.addListener(fresource);
                    fresource.basinTableDlg.open();
                }
            });
        } else {
            resource.basinTableDlg.open();
        }
    }

    /**
     * update the data in the dialog
     */
    public void updateDialog(FFMPResource resource) {

        if (resource.basinTableDlg != null) {

            resource.isFirst = false;
            fireMonitorEvent(resource.basinTableDlg.getClass().getName());
        }
    }

    public synchronized void splashDisposeAndDataLoad(FFMPResource resource) {
        if (ffmpSplash != null) {
            ffmpSplash.close();
            ffmpSplash = null;

            if (resource.isFirst) {
                updateDialog(resource);
            }

            // start secondary data load
            try {
                startLoad(resource, resource.getResourceData().timeBack,
                        LOADER_TYPE.SECONDARY);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Secondary Data Load failure", e);
            }
        }
    }

    public void forceKillFFMPSplash() {
        if (ffmpSplash != null) {
            ffmpSplash.close();
            ffmpSplash = null;
        }
    }

    /**
     * Add the FFMPResource
     * 
     * @param listener
     */
    public void addResourceListener(IFFMPResourceListener listener) {
        resourceListeners.add(listener);
    }

    /**
     * Remove the FFMPResource
     * 
     * @param listener
     */
    public void removeResourceListener(IFFMPResourceListener listener) {

        ConcurrentHashMap<String, Integer> siteCount = new ConcurrentHashMap<String, Integer>();

        for (IFFMPResourceListener clistener : resourceListeners) {
            if (listener instanceof FFMPResource) {
                FFMPResource res = (FFMPResource) clistener;
                if (siteCount.containsKey(res.getSiteKey())) {
                    int val = siteCount.get(res.getSiteKey());
                    siteCount.replace(res.getSiteKey(), val + 1);
                } else {
                    siteCount.put(res.getSiteKey(), 1);
                }
            }
        }

        if (listener instanceof FFMPResource) {
            FFMPResource res = (FFMPResource) listener;
            if (res.getResourceData().floader != null) {
                res.getResourceData().floader.kill();
            }
            res.getResourceData().floader = null;
            int val = siteCount.get(res.getSiteKey());

            // never opened a cache
            if (ffmpData.get(res.getSiteKey()) != null) {
                // clear out the cache
                for (Entry<String, FFMPCacheRecord> entry : ffmpData.get(
						res.getSiteKey()).entrySet()) {
                    entry.getValue().closeCache();
                }
            }

            if ((val == 1) && (siteCount.size() > 1)) {

                ffmpData.remove(res.getSiteKey());
                ffmpAvailableUriQueryDates.remove(res.getSiteKey());
                ffmpAvailableUris.remove(res.getSiteKey());
                ffmpLoadedUris.remove(res.getSiteKey());
            }
        }

        resourceListeners.remove(listener);
        // clean up if we can
        System.gc();
    }

    public ArrayList<IFFMPResourceListener> getResourceListenerList() {
        return resourceListeners;
    }

    /**
     * close dialog and cleanup
     */
    public void closeDialog(FFMPResource res) {
        if (res.basinTableDlg != null) {
            res.basinTableDlg.removeListener(res);
            res.basinTableDlg.disposeDialog();
            res.basinTableDlg = null;
        }
    }

    /**
     * Thread the updates to the drawing
     * 
     * @param listener
     */
    private void fireRefresh(IFFMPResourceListener listener) {

        final IFFMPResourceListener flistener = listener;

        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                flistener.refresh();
            }
        });
    }

    /**
     * Sees if this source is a primary source for a product
     * 
     * @param sourceName
     * @return
     */
    public ProductXML getProductXML(String sourceName) {
        return getSourceConfig().getProduct(sourceName);
    }

    /**
     * Get the pertinent QPE source Record.
     * 
     * @param date
     * @param phuc
     * @param retrieveNew
     * @return
     */
    public FFMPCacheRecord getQPERecord(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc,
            boolean retrieveNew) {

        // comparisons done with table display
        if (product != null) {
            sourceName = product.getQpe();
        }

        return getFFMPData(product, siteKey, dataKey, sourceName, date, phuc,
                retrieveNew);
    }

    /**
     * Get the pertinent QPE source basin.
     * 
     * @param date
     * @param phuc
     * @param pfaf
     * @return
     */
    public FFMPBasin getGraphQPEBasin(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc, Long pfaf)
            throws VizException {
        // comparisons done with table display
        if ((product != null) && (sourceName == null)) {
            sourceName = product.getQpe();
        }

        return getFFMPBasinData(product, siteKey, dataKey, sourceName, date,
                phuc, pfaf);
    }

    /**
     * Get the rate record.
     * 
     * @param date
     * @param phuc
     * @param retrieveNew
     * @return
     */
    public FFMPCacheRecord getRateRecord(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc,
            boolean retrieveNew) {

        // comparisons done with table display
        if (product != null) {
            sourceName = product.getRate();
        }

        return getFFMPData(product, siteKey, dataKey, sourceName, date, phuc,
                retrieveNew);
    }

    /**
     * Get the rate basin.
     * 
     * @param ptime
     * @param phuc
     * @param pfaf
     * @return
     */
    public FFMPBasin getGraphRateBasin(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date ptime, String phuc,
            Long pfaf) throws VizException {

        // comparisons done with table display
        if ((product != null) && (sourceName == null)) {
            sourceName = product.getRate();
        }

        return getFFMPBasinData(product, siteKey, dataKey, sourceName, ptime,
                phuc, pfaf);
    }

    /**
     * Gets the QPF record
     * 
     * @param date
     * @param phuc
     * @param retrieveNew
     * @return
     */
    public FFMPCacheRecord getQPFRecord(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc,
            boolean retrieveNew) {

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(siteKey);
        String qpfType = ffmpTableCfgData.getQpfType();
        ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
                .getProduct(siteKey);

        // comparisons done with table display
        if (product != null) {
            sourceName = productRun.getQpfSources(product, qpfType).get(0)
                    .getSourceName();
        }

        return getFFMPData(product, siteKey, dataKey, sourceName, date, phuc,
                retrieveNew);
    }

    /**
     * Get the QPF Basin.
     * 
     * @param date
     * @param phuc
     * @param pfaf
     * @return
     */
    public FFMPBasin getGraphQPFBasin(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc, Long pfaf)
            throws VizException {

        // comparisons done with table display
        if (product != null) {
            ProductRunXML productRun = FFMPRunConfigurationManager
                    .getInstance().getProduct(siteKey);
            FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig
                    .getInstance().getTableConfigData(siteKey);
            String qpfType = ffmpTableCfgData.getQpfGraphType();
            sourceName = productRun.getQpfSources(product, qpfType).get(0)
                    .getSourceName();
        }

        return getFFMPBasinData(product, siteKey, dataKey, sourceName, date,
                phuc, pfaf);
    }

    /**
     * Gets the guidance source types
     * 
     * @param date
     * @param phuc
     * @return
     */
    public FFMPCacheRecord getGuidanceRecord(ProductXML product,
            String siteKey, String sourceName, Date date, String phuc,
            boolean isStandAlone) {
        String guidSrc = FFMPConfig.getInstance().getFFMPConfigData()
                .getGuidSrc();
        if (!isStandAlone && guidSrc.startsWith("xxx")) {
            return null;
        }
        if (product != null) {
            ProductRunXML productRun = FFMPRunConfigurationManager
                    .getInstance().getProduct(siteKey);
            SourceXML source = productRun.getGuidanceSources(product, guidSrc)
                    .get(0);
            sourceName = source.getDisplayName();
        }

        return getFFMPData(product, siteKey, null, sourceName, date, phuc,
                false);
    }

    /**
     * Gets the Guidance records NOTE: This is only used in the
     * FFMPDataGenerator
     * 
     * @param product
     * @param siteKey
     * @param date
     * @param phuc
     * @param retrieveNew
     * @return
     */
    public HashMap<String, FFMPCacheRecord> getGuidanceRecords(
            ProductXML product, String siteKey, Date date, String phuc,
            boolean retrieveNew) {

        HashMap<String, FFMPCacheRecord> guidRecs = new HashMap<String, FFMPCacheRecord>();
        ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
                .getProduct(siteKey);
        ArrayList<String> guidTypes = productRun.getGuidanceTypes(product);

        for (String type : guidTypes) {

            FFMPCacheRecord guidRec = getFFMPData(product, siteKey, null, type,
                    date, phuc, retrieveNew);
            guidRecs.put(type, guidRec);
        }

        return guidRecs;
    }

    /**
     * Gets the guidance source types.
     * 
     * @param date
     * @param phuc
     * @param pfaf
     * @return
     */
    public FFMPBasin getGraphGuidanceBasin(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc, Long pfaf)
            throws VizException {

        if (product != null) {
            String guidSrc = FFMPConfig.getInstance().getFFMPConfigData()
                    .getGuidSrc();
            SourceXML source = product.getGuidanceSourcesByType(guidSrc).get(0);
            sourceName = source.getDisplayName();
        }

        return getFFMPBasinData(product, siteKey, dataKey, sourceName, date,
                phuc, pfaf);
    }

    /**
     * Gets the virtual gage basin record
     * 
     * @param date
     * @param phuc
     * @param retrieveNew
     * @return
     */
    public FFMPCacheRecord getVirtualRecord(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date date, String phuc,
            boolean retrieveNew) {
        // comparisons done with table display
        // field doesn't matter here

        if (product != null) {
            sourceName = product.getVirtual();
        }

        return getFFMPData(product, siteKey, dataKey, sourceName, date, phuc,
                retrieveNew);
    }

    /**
     * Grabs data for a particular basin. Used by the graph's.
     * 
     * @param sourceName
     * @param date
     * @param phuc
     * @param pfaf
     * @return
     */
    public FFMPBasin getFFMPBasinData(ProductXML product, String siteKey,
            String dataKey, String sourceName, Date ptime, String phuc,
            Long pfaf) throws VizException {
        FFMPBasin basin = null;
        // check for already loaded records
        FFMPCacheRecord record = ffmpData.get(siteKey).get(sourceName);
        if (record != null) {
            FFMPBasinData basinData = record.getBasinData(phuc);
            if (basinData != null) {
                basin = basinData.get(pfaf);
            }
        }

        SourceXML psource = getSourceConfig().getSource(sourceName);
        if (psource == null) {
            psource = getSourceConfig().getSourceByDisplayName(sourceName);
        }
        // basin should have loaded something
        if (psource.getDisplayName().equals(sourceName)
                && psource.getSourceType().equals(
                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
            FFMPGuidanceBasin fgb = null;
            if (basin != null) {
                fgb = (FFMPGuidanceBasin) basin;
            } else {
                fgb = new FFMPGuidanceBasin(pfaf, !phuc.equals("ALL"));
                basin = fgb;
            }

            if (product != null) {
                ProductRunXML productRun = getRunConfig().getProduct(siteKey);
                FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig
                        .getInstance().getTableConfigData(siteKey);

                for (SourceXML source : productRun.getGuidanceSources(product,
                        ffmpTableCfgData.getFfgGraphType())) {
                    if (fgb.containsKey(source.getSourceName())) {
                        continue;
                    } else {
                        // populate if it isn't there
                        SortedMap<Date, List<String>> availableUris = getAvailableUris(
                                siteKey, dataKey, source.getSourceName(), ptime);

                        if ((availableUris != null)
                                && (availableUris.size() > 0)) {
                            POINT_RETRIVAL: for (List<String> uris : availableUris
                                    .values()) {
                                for (String uri : uris) {

                                    if (!ffmpLoadedUris.containsKey(siteKey)) {

                                        ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>> tempLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>();
                                        tempLoadedUris
                                                .put(source.getSourceName(),
                                                        new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>(
                                                                16, 0.75f, 4));

                                        ffmpLoadedUris.put(siteKey,
                                                tempLoadedUris);
                                    }

                                    if (!ffmpLoadedUris.get(siteKey)
                                            .get(source).get(phuc)
                                            .containsKey(uri)) {
                                        // populate point only
                                        populateFFMPBasin(uri, siteKey,
                                                source.getSourceName(), phuc,
                                                fgb);
                                    } else {
                                        break POINT_RETRIVAL;
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                // FFG is the primary source
                if (!fgb.containsKey(sourceName)) {
                    // populate if it isn't there
                    SortedMap<Date, List<String>> availableUris = getAvailableUris(
                            siteKey, dataKey, sourceName, ptime);

                    if ((availableUris != null) && (availableUris.size() > 0)) {
                        POINT_RETRIVAL: for (List<String> uris : availableUris
                                .values()) {
                            for (String uri : uris) {

                                if (!ffmpLoadedUris.containsKey(siteKey)) {

                                    ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>> tempLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>();

                                    tempLoadedUris
                                            .put(sourceName,
                                                    new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>(
                                                            16, 0.75f, 4));

                                    ffmpLoadedUris.put(siteKey, tempLoadedUris);
                                }

                                if (!ffmpLoadedUris.get(siteKey)
                                        .get(sourceName).get(phuc)
                                        .containsKey(uri)) {
                                    // populate point only
                                    populateFFMPBasin(uri, siteKey, sourceName,
                                            phuc, fgb);
                                } else {
                                    break POINT_RETRIVAL;
                                }
                            }
                        }
                    }
                }
            }
        } else {
            // populate if it isn't there
            SortedMap<Date, List<String>> availableUris = getAvailableUris(
                    siteKey, dataKey, sourceName, ptime);

            if ((availableUris != null) && (availableUris.size() > 0)) {
                POINT_RETRIVAL: for (List<String> uris : availableUris.values()) {
                    for (String uri : uris) {

                        if (!ffmpLoadedUris.containsKey(siteKey)) {

                            ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>> tempLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>();

                            if (product != null) {
                                for (String source : product.getSources()) {

                                    tempLoadedUris
                                            .put(source,
                                                    new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>(
                                                            16, 0.75f, 4));
                                }

                                ffmpLoadedUris.put(siteKey, tempLoadedUris);

                            } else {
                                tempLoadedUris
                                        .put(sourceName,
                                                new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>(
                                                        16, 0.75f, 4));
                                ffmpLoadedUris.put(siteKey, tempLoadedUris);
                            }
                        }

                        if (!ffmpLoadedUris.get(siteKey)
                                .containsKey(sourceName)) {
                            ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>> tempLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>();
                            tempLoadedUris
                                    .put(sourceName,
                                            new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>(
                                                    16, 0.75f, 4));
                            ffmpLoadedUris.put(siteKey, tempLoadedUris);
                        }

                        if (!ffmpLoadedUris.get(siteKey).get(sourceName)
                                .containsKey(phuc)) {
                            ffmpLoadedUris
                                    .get(siteKey)
                                    .get(sourceName)
                                    .put(phuc,
                                            new ConcurrentHashMap<String, String>());
                        }

                        if (!ffmpLoadedUris.get(siteKey).get(sourceName)
                                .get(phuc).containsKey(uri)) {
                            // populate point only
                            populateFFMPBasin(uri, siteKey, sourceName, phuc,
                                    basin);
                        } else {
                            break POINT_RETRIVAL;
                        }
                    }
                }
            }
        }

        return basin;
    }

    /**
     * Gets the valid time window for the source as a long
     * 
     * @param sourceName
     * @return
     */
    public long getSourceTimeWindow(String sourceName, String siteKey) {
        return getSourceConfig().getSource(sourceName).getExpirationMinutes(
                siteKey) * 60 * 1000;
    }

    /**
     * gets the time window object
     * 
     * @param sourceName
     * @return
     */
    public FFMPTimeWindow getTimeWindow(String sourceName, Date date,
            String siteKey) {
        FFMPTimeWindow window = new FFMPTimeWindow();
        long lwindow = getSourceTimeWindow(sourceName, siteKey);
        window.setAfterTime(new Date(date.getTime() - lwindow));
        window.setBeforeTime(new Date(date.getTime() + lwindow));

        return window;
    }

    public FFMPTimeWindow getQpfWindow() {
        return qpfWindow;
    }

    public void setQpfWindow(FFMPTimeWindow qpfWindow) {
        this.qpfWindow = qpfWindow;
    }

    public FFMPTimeWindow getQpeWindow() {
        return qpeWindow;
    }

    public void setQpeWindow(FFMPTimeWindow qpeWindow) {
        this.qpeWindow = qpeWindow;
    }

    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * fire off a cleaner
     * 
     * @param product
     * @param source
     * @param siteKey
     * @param date
     */
    public void purgeFFMPData(ProductXML product, String source,
            String siteKey, Date date) {
        PurgeFFMPData pfd = new PurgeFFMPData(product, source, siteKey, date);
        pfd.purge();
    }

    /**
     * Purge old data from the cache
     * 
     * @author dhladky
     * 
     */
    private class PurgeFFMPData implements Runnable {

        final ProductXML fproduct;

        final String fsource;

        final String fsiteKey;

        final Date fdate;

        public PurgeFFMPData(ProductXML product, String source, String siteKey,
                Date date) {

            this.fproduct = product;
            this.fsource = source;
            this.fsiteKey = siteKey;
            this.fdate = date;
        }

        @Override
        public void run() {
            try {
                purge();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to purge Data from FFMP cache, ", e);
            }
        }

        public void purge() {

            ArrayList<String> purgeSources = new ArrayList<String>();

            if (fproduct != null) {

                ProductRunXML productRun = getRunConfig().getProduct(fsiteKey);

                // guidance
                for (String type : productRun.getGuidanceTypes(fproduct)) {
                    for (SourceXML guidSource : productRun.getGuidanceSources(
                            fproduct, type)) {
                        if (guidSource != null) {
                            purgeSources.add(guidSource.getSourceName());
                        }
                    }
                }

                // qpf
                for (String type : productRun.getQpfTypes(fproduct)) {
                    for (SourceXML qpfSource : productRun.getQpfSources(
                            fproduct, type)) {
                        if (qpfSource != null) {
                            purgeSources.add(qpfSource.getSourceName());
                        }
                    }
                }
                // qpe, etc
                for (String sourceName : fproduct.getSources()) {
                    if (!purgeSources.contains(sourceName)) {
                        purgeSources.add(sourceName);
                    }
                }

                for (String sourceName : purgeSources) {

                    SourceXML source = fscm.getSource(sourceName);

                    if (source != null) {
                        if (source.getSourceType().equals(
                                SOURCE_TYPE.GUIDANCE.getSourceType())) {
                            sourceName = SOURCE_TYPE.GUIDANCE.getSourceType();
                        }

                        if (ffmpData != null) {
                            if (ffmpData.containsKey(fsiteKey)) {
                                FFMPCacheRecord record = ffmpData.get(fsiteKey)
                                        .get(sourceName);
                                if (record != null) {
                                    record.purgeData(fdate);
                                }
                            }
                        }
                    }
                }

            } else {
                if (ffmpData != null) {
                    if (ffmpData.containsKey(fsiteKey)) {
                        FFMPCacheRecord record = ffmpData.get(fsiteKey).get(
                                fsource);
                        if (record != null) {
                            record.purgeData(fdate);
                        }
                    }
                }
            }

            if (ffmpAvailableUris != null) {
                for (Entry<String, ConcurrentSkipListMap<Date, List<String>>> entry : ffmpAvailableUris
                        .get(fsiteKey).entrySet()) {
                    ConcurrentNavigableMap<Date, List<String>> oldUris = entry
                            .getValue().headMap(fdate);
                    for (List<String> uris : oldUris.headMap(fdate).values()) {
                        for (String uri : uris) {

                            if (fproduct != null) {

                                for (String sourceName : purgeSources) {

                                    if (ffmpLoadedUris.get(fsiteKey)
                                            .containsKey(sourceName)) {
                                        for (ConcurrentHashMap<String, String> loadedUris : ffmpLoadedUris
                                                .get(fsiteKey).get(sourceName)
                                                .values()) {
                                            loadedUris.remove(uri);
                                        }
                                    }
                                }
                            } else {
                                for (ConcurrentHashMap<String, String> loadedUris : ffmpLoadedUris
                                        .get(fsiteKey).get(fsource).values()) {
                                    loadedUris.remove(uri);
                                }
                            }
                        }
                    }

                    oldUris.clear();
                }
            }
        }
    }

    /**
     * Process an individual URI
     * 
     * @param isProductLoad
     * @param uri
     * @param siteKey
     * @param sourceName
     * @param barrierTime
     * @param phuc
     */
    public void processUri(boolean isProductLoad, String uri, String siteKey,
            String sourceName, Date barrierTime, String phuc) {

        SourceXML source = getSourceConfig().getSource(sourceName);

        if (uri != null) {
            FFMPRecord record = new FFMPRecord(uri);

            try {
                record = populateFFMPRecord(isProductLoad, uri, siteKey,
                        sourceName, phuc);
                if ((record != null) && (source != null)) {
                    record.setExpiration(source.getExpirationMinutes(siteKey));
                    record.setRate(source.isRate());
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "FFMP Can't retrieve FFMP URI, " + uri, e);
            }
        }
    }

    /**
     * Adds this source to the URI hash in the monitor
     * 
     * @param uris
     * @param sourceName
     */
    public void processUris(NavigableMap<Date, List<String>> uriMap,
            boolean isProductLoad, String siteKey, String sourceName,
            Date barrierTime, String phuc) {

        FFMPProcessUris processor = new FFMPProcessUris(uriMap, isProductLoad,
                siteKey, sourceName, barrierTime, phuc);
        processor.run();
    }

    @Override
    protected void nullifyMonitor(String icao) {
        // TODO Auto-generated method stub

    }

    /**
     * Gets the HUC templates
     * 
     * @return
     */
    public FFMPTemplates getTemplates(String siteKey) {
        if (templates == null) {
            FFMPRunXML runner = getRunConfig().getRunner(wfo);
            this.templates = FFMPTemplates.getInstance(
                    runner.getPrimaryDomain(), siteKey, MODE.CAVE);
            // backup domains
            if (runner.getBackupDomains() != null) {
                for (DomainXML backup : runner.getBackupDomains()) {
                    templates.addDomain(siteKey, backup);
                }
            } else {
                templates.done = true;
            }
        }

        if (!templates.isSiteLoaded(siteKey)) {
            FFMPRunXML runner = getRunConfig().getRunner(wfo);

            for (DomainXML domain : runner.getDomains()) {
                templates.addDomain(siteKey, domain);
            }
        }

        return templates;
    }

    /**
     * Get the Run configuration
     * 
     * @return
     */
    public FFMPRunConfigurationManager getRunConfig() {
        if (frcm == null) {
            frcm = FFMPRunConfigurationManager.getInstance();
        }
        return frcm;
    }

    /**
     * Get the PP and PC values from curpp and curpc tables
     * 
     * @param pfaf
     * @param lid
     * @param refTime
     */
    public FFMPVirtualGageBasin getVirtualGageBasinData(long pfaf, String lid,
            Date refTime) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        TimeZone gmt = TimeZone.getTimeZone("GMT");
        final String ihfs = "ihfs";
        FFMPVirtualGageBasin basin = new FFMPVirtualGageBasin(lid, pfaf, false);

        sdf.setTimeZone(gmt);

        Calendar cal = Calendar.getInstance(gmt);
        cal.setTime(refTime);
        cal.add(Calendar.HOUR_OF_DAY, -24);
        Date startTime = cal.getTime();

        String starttime = sdf.format(startTime);
        String endtime = sdf.format(refTime);

        /*
         * First we will check "PC" precip data from the database; The big issue
         * of the PC data is that the gage amount sometimes is re-set to "zero"
         * which will be ignored. If the "PC" data is not available, then we
         * will check for its "PP" data which is less frequently updated most of
         * the times and has an issue of "duration" (1 min, 15 min, 30 min, 1
         * hour, 2 hours, 3 hours and 6 hours) we have to deal with.
         */
        String pe = "PC";
        String tablename = "curpc";

        String format = "SELECT lid, pe, dur, ts, obstime, value, product_id "
                + "FROM %s WHERE lid = '%s' AND obstime >= '%s' AND obstime < '%s' "
                + "AND pe = '%s' ORDER BY obstime DESC, ts ASC";

        String sql = String.format(format, tablename, lid, starttime, endtime,
                pe);

        List<Object[]> precipData = null;
        try {
            precipData = DirectDbQuery.executeQuery(sql, ihfs,
                    QueryLanguage.SQL);
        } catch (VizException e) {
            // problem querying PC data
            e.printStackTrace();
        }

        // if PC data NOT available
        if ((precipData == null) || (precipData.size() == 0)) {

            pe = "PP";
            tablename = "curpp";
            String desiredDur = "1001";
            format = "SELECT lid, pe, dur, ts, obstime, value, product_id "
                    + "FROM %s WHERE lid = '%s' AND obstime >= '%s' AND obstime < '%s' "
                    + "AND pe = '%s' AND dur <= %s AND value >=0 ORDER BY dur ASC, "
                    + "obstime DESC, ts ASC";

            sql = String.format(format, tablename, lid, starttime, endtime, pe,
                    desiredDur);

            try {
                precipData = DirectDbQuery.executeQuery(sql, ihfs,
                        QueryLanguage.SQL);
            } catch (VizException e) {
                // problem querying PP data
                e.printStackTrace();
            }

            if ((precipData == null) || (precipData.size() == 0)) {
                return null;
            }
        }

        /*
         * need to check whether the data list contains records from different
         * type source which should not be plotted in one graph. Use the
         * "ingestfilter" table to pick up the type source with the highest type
         * ranking order.
         */
        String typeSrc = null;
        List<String> uniqueSrcType = new ArrayList<String>();
        List<Object[]> trendData = new ArrayList<Object[]>();

        // no type source need to be checked if only one record is found.
        if (precipData.size() > 1) {
            for (Object[] oa : precipData) {
                String ts = (String) oa[3];
                if (uniqueSrcType.contains(ts) == false) {
                    uniqueSrcType.add(ts);
                }
            }

            if (uniqueSrcType.size() > 1) {
                /*
                 * more than one ts found. Get the type source rankings by
                 * accessing the "ingestfilter" table in the database. The
                 * smaller the type source rank value is, the higher priority
                 * the type source should be taken into account.
                 */
                format = "SELECT dur, ts, ts_rank FROM ingestFilter WHERE "
                        + "lid = '%s' AND pe = '%s' ORDER BY ts_rank ASC";

                sql = String.format(format, lid, pe);
                List<Object[]> rs = null;

                try {
                    rs = DirectDbQuery.executeQuery(sql, ihfs,
                            QueryLanguage.SQL);
                } catch (VizException e) {
                    // problem querying IngestFilter table
                    e.printStackTrace();
                }

                if ((rs != null) && (rs.size() > 0)) {
                    typeSrc = (String) rs.get(0)[1];
                }

                // Get the records of the desired type source.
                for (Object[] oa : precipData) {
                    if (((String) oa[3]).equals(typeSrc)) {
                        trendData.add(oa);
                    }
                }
            } else {
                for (Object[] oa : precipData) {
                    trendData.add(oa);
                }
            }
        }

        // processing gage data
        float gageAccu = 0;

        if (pe.equalsIgnoreCase("PP")) {
            // to select the data with the shortest duration from
            // durations of 1 min, 15 min, 30 min, 1 hr and 2 hrs.
            TreeSet<Integer> durOrderSet = new TreeSet<Integer>();

            for (Object[] oa : trendData) {
                int dur = Integer.parseInt((String) oa[2]);
                durOrderSet.add(dur);
            }

            // need to get the max duration for each hour
            List<ArrayList<Integer>> idxSameHour = getMaxDurationPerHour(trendData);

            Iterator<Integer> iter = durOrderSet.iterator();
            int maxDur = 0;
            boolean first = true;
            List<Object[]> data1Dur = null;

            while (iter.hasNext()) {
                if (first) {
                    maxDur = iter.next();
                    first = false;
                }

                // Notes (02/07/2006): For the AWOS and ASOS sites, the metar
                // data are converted by SHEFDECODER and then saved into the PP
                // precip database. But the metar data taken at 0005Z
                // (20 min duration), 0025Z (40 min duration) and
                // 0045Z (60 min duration) might be stored as records with
                // 1-hour duration (as dur=1001). To avoid to count the
                // 1-hour precip for multipe times for one same period,
                // only ONE of the multiple records will be taken for VGB
                // gage plot. Using the maximum duration -- 1-hr dur will
                // be the most representative one. Need to make sure only
                // one data point should be used within each hour if those
                // multiple data points have the same duration for this hour.
                data1Dur = selectMaxDurData(trendData, maxDur, idxSameHour);

                if (data1Dur.size() > 0) {
                    break;
                }
            }

            long secondsL = 0; // duration left bound
            long secondsR = 0; // duration right bound
            Date t1 = refTime;
            // Date t0 = refTime - purgeTime;
            long dur = 0;

            if (data1Dur != null) {
                // should differentiate the "duration" of the gage data.
                for (int i = 0; i < data1Dur.size(); i++) {
                    secondsL = (t1.getTime() - ((Date) data1Dur.get(i)[4])
                            .getTime()) / 1000;
                    if (((Integer) data1Dur.get(i)[2]) > 1000) {
                        dur = ((Integer) data1Dur.get(i)[2] - 1000)
                                * SECONDS_PER_HOUR;
                    } else {
                        dur = ((Integer) data1Dur.get(i)[2]) * 1000;
                    }

                    secondsR = secondsL + dur; // time drawn in reverse way
                    // we can not include the future gage data into the plot
                    // compared
                    // to the latest inv time of FFMP accum
                    // Note: for PP data, each precip value relates to two time
                    // instants
                    // based on "duration". A horizontal line (composed of two
                    // points) will be drawn to represent the precip during
                    // each "duration" period.
                    if ((secondsL < 0) || (secondsR > t1.getTime() / 1000)) {
                        // gage data ahead of or beyond radar time range
                        continue;
                    } else {
                        gageAccu += (Float) data1Dur.get(i)[5];
                        basin.setValue(refTime, gageAccu);
                    }
                }
            }
        } else { // PC Data
            long seconds = 0; // initialized
            Date t1 = refTime;
            if ((trendData != null) && (trendData.size() > 0)) {
                double firstValue = (Double) trendData.get(0)[5];
                for (int i = 0; i < trendData.size(); i++) {
                    Date dataDate = (Date) trendData.get(i)[4];
                    Double value = (Double) trendData.get(i)[5];
                    double prevValue = -99;
                    if (i > 1) {
                        prevValue = (Double) trendData.get(i - 1)[5];
                    }

                    seconds = (t1.getTime() - ((Date) trendData.get(i)[4])
                            .getTime()) / 1000;

                    if ((seconds < 0) || (seconds > t1.getTime() / 1000)) { // gage
                        // range
                        continue;
                    } else if (i == 0) {
                        // accum is set to zero at the first gage data point.
                        basin.setValue(dataDate, gageAccu);
                    } else {
                        if (value < 0) { // if cur point unavailable
                            continue;
                        } else if (prevValue < 0) { // previous point N/A
                            if (firstValue > 0) {
                                gageAccu = (float) (firstValue - value);
                            } else {
                                continue;
                            }
                        } else {
                            double dAccu = prevValue - value;
                            if (dAccu < 0) { // gage reading error
                                continue;
                            } else {
                                gageAccu += dAccu;
                                // Note: PC data are the rain amount that fallen
                                // since some time when the "counter" was set to
                                // 0.
                                // So we can not count the accum if gage_accu is
                                // a negative value which means the accumulation
                                // will be re-started from zero.
                            }

                            basin.setValue(dataDate, gageAccu);
                        }
                    }
                }
            }
        }

        return basin;
    }

    private List<Object[]> selectMaxDurData(List<Object[]> org, int dur,
            List<ArrayList<Integer>> idxSameHour) {
        List<Object[]> filtered = new ArrayList<Object[]>();

        // go through the indices for each hour period
        for (int i = 0; i < idxSameHour.size(); i++) {
            // select the max obsTime at desired duration as the data point
            // to be used as the representative within each hour. Since the
            // obstime was sorted in the descending order, the first one will be
            // the max Obstime for the specified hour.
            for (int j = 0; j < idxSameHour.size(); j++) {
                if ((Integer) org.get(idxSameHour.get(i).get(j))[4] == dur) {
                    // the obsTime has already been in DESCENDING order, so the
                    // largest one for the hour is what we are looking for.
                    filtered.add(org.get(idxSameHour.get(i).get(j)));
                }
            }
        }

        return filtered;
    }

    private List<ArrayList<Integer>> getMaxDurationPerHour(List<Object[]> data) {
        List<ArrayList<Integer>> idx = new ArrayList<ArrayList<Integer>>();
        ArrayList<Integer> dummyIdx = new ArrayList<Integer>();

        for (int i = 0; i < data.size(); i++) {
            Object[] oa = data.get(i);
            if (i < data.size() - 1) {
                dummyIdx.add(i);
                if (!withinSameHour((Date) oa[4], (Date) data.get(i + 1)[4])) {
                    idx.add(dummyIdx);
                    dummyIdx.clear();
                }
            } else { // for the LAST time element
                if (withinSameHour((Date) data.get(i - 1)[4], (Date) oa[4])) {
                    // Note: the previously grouped times have not been inserted
                    // into the list, so add it with the last time element.
                    dummyIdx.add(i);
                    idx.add(dummyIdx);
                    dummyIdx.clear();
                } else {
                    // Note: the last time element does NOT belong to the
                    // previous group,
                    dummyIdx.add(i);
                    idx.add(dummyIdx);
                    dummyIdx.clear();
                }
            }
        }

        return idx;
    }

    /**
     * To check whether the passed-in hours belong to the same hour period.
     * 
     * @param a
     *            Date
     * @param b
     *            Date
     * @return true if times belong to the same hour
     */
    private boolean withinSameHour(Date a, Date b) {
        TimeZone gmt = TimeZone.getTimeZone("GMT");
        Calendar calA = Calendar.getInstance(gmt);
        calA.setTime(a);
        Calendar calB = Calendar.getInstance(gmt);
        calB.setTime(b);
        if (calB.get(Calendar.MINUTE) == 0) {
            return false;
        } else if ((calA.get(Calendar.YEAR) == calB.get(Calendar.YEAR))
                && (calA.get(Calendar.MONTH) == calB.get(Calendar.MONTH))
                && (calA.get(Calendar.DAY_OF_MONTH) == calB
                        .get(Calendar.DAY_OF_MONTH))) {
            if (calA.get(Calendar.HOUR_OF_DAY) == calB
                    .get(Calendar.HOUR_OF_DAY)) {
                return true;
            } else if ((calA.get(Calendar.MINUTE) == 0)
                    && (Math.abs(calA.getTimeInMillis()
                            - calB.getTimeInMillis()) < (60 * 60))) {
                return true;
            }
        }

        return false;
    }

    /**
     * Update FFFG
     */
    public void updateFFFG() {

        fffg = FFFGDataMgr.getUpdatedInstance();

        for (IFFMPResourceListener listener : getResourceListenerList()) {
            if (listener instanceof FFMPResource) {

                FFMPResource resource = (FFMPResource) listener;
                resource.dirty();
                resource.refresh();
                resource.clearTables();

                if (resource.getResourceData().tableLoad) {
                    updateDialog(resource);
                }
            }
        }
    }

    /**
     * Gets the previous URI available
     * 
     * @param siteKey
     * @param sourceName
     * @return
     */
    public Date getPreviousQueryTime(String siteKey, String sourceName) {
        return ffmpAvailableUriQueryDates.get(siteKey).get(sourceName);
    }

    /**
     * class used to thread data loads
     * 
     * @author dhladky
     * 
     */
    private class FFMPLoadRecord implements Runnable {

        final FFMPRecord fffmpRec;

        final String fsource;

        final String fhuc;

        final String fsiteKey;

        public FFMPLoadRecord(boolean isProductLoad, String siteKey,
                FFMPRecord ffmpRec, String source, String huc) throws Exception {
            this.fffmpRec = ffmpRec;
            this.fsource = source;
            this.fsiteKey = siteKey;
            this.fhuc = huc;
        }

        public void run() {
            try {
                load();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "FFMP load FFMPData, "
                        + fsource + " " + fhuc, e);
            }
        }

        public void load() {

            if (fffmpRec != null) {

                ConcurrentMap<String, String> uris = getUriMap(fsiteKey,
                        fsource, fhuc);
                String dataUri = fffmpRec.getDataURI();
                if (!uris.containsKey(dataUri)) {
                    File loc = HDF5Util.findHDF5Location(fffmpRec);
                    IDataStore dataStore = DataStoreFactory.getDataStore(loc);

                    if (!ffmpData.containsKey(fsiteKey)) {
                        ConcurrentHashMap<String, FFMPCacheRecord> fieldRecs = new ConcurrentHashMap<String, FFMPCacheRecord>();
                        ffmpData.put(fsiteKey, fieldRecs);
                    }

                    String mySource = fsource;
                    SourceXML source = fscm.getSource(fsource);

                    if (source.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {
                        mySource = source.getDisplayName();
                    }

                    FFMPCacheRecord curRecord = ffmpData.get(fsiteKey).get(
                            mySource);
                    if (curRecord == null) {
                        // ensure the record can only be set once
                        synchronized (ffmpData) {
                            curRecord = ffmpData.get(fsiteKey).get(mySource);
                            if (curRecord == null) {
                                curRecord = new FFMPCacheRecord(fffmpRec,
                                        mySource, getRunConfig().getRunner(wfo)
                                                .getCacheDir());
                                ffmpData.get(fsiteKey).put(mySource, curRecord);
                            }
                        }
                    }

                    SourceXML sourceXML = fscm.getSource(mySource);

                    if ((sourceXML != null)
                            && sourceXML.getSourceType().equals(
                                    SOURCE_TYPE.GAGE.getSourceType())
                            && fhuc.equals("ALL")) {
                        try {
                            curRecord.retrieveVirtualMapFromDataStore(
                                    dataStore, dataUri, getTemplates(fsiteKey),
                                    fffmpRec.getDataTime().getRefTime(),
                                    fffmpRec.getSourceName());
                        } catch (FileNotFoundException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "FFMP Can't find FFMP URI, " + dataUri, e);
                        } catch (StorageException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "FFMP Can't retrieve (Storage problem) FFMP URI, "
                                            + dataUri, e);
                        }
                    } else {
                        try {
                            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                                statusHandler.handle(Priority.DEBUG,
                                    "Retrieving and Populating URI: , "
                                            + dataUri);
                            }
                            curRecord.retrieveMapFromDataStore(dataStore,
                                    dataUri,
                                    getTemplates(fffmpRec.getSiteKey()), fhuc,
                                    fffmpRec.getDataTime().getRefTime(),
                                    fffmpRec.getSourceName());
                        } catch (Exception e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "FFMP Can't retrieve FFMP URI, " + dataUri,
                                    e);
                        }
                    }

                    ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>> siteLoadedUris = ffmpLoadedUris
                            .get(fsiteKey);

                    if (siteLoadedUris == null) {

                        siteLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, ConcurrentHashMap<String, String>>>();
                        ffmpLoadedUris.put(fsiteKey, siteLoadedUris);
                    }

                    ConcurrentHashMap<String, ConcurrentHashMap<String, String>> sourceLoadedUris = ffmpLoadedUris
                            .get(fsiteKey).get(fsource);

                    if (sourceLoadedUris == null) {

                        sourceLoadedUris = new ConcurrentHashMap<String, ConcurrentHashMap<String, String>>();
                        ffmpLoadedUris.get(fsiteKey).put(fsource,
                                sourceLoadedUris);
                    }

                    ConcurrentHashMap<String, String> hucLoadedUris = ffmpLoadedUris
                            .get(fsiteKey).get(fsource).get(fhuc);

                    if (hucLoadedUris == null) {

                        hucLoadedUris = new ConcurrentHashMap<String, String>();
                        ffmpLoadedUris.get(fsiteKey).get(fsource)
                                .put(fhuc, hucLoadedUris);
                    }

                    synchronized (hucLoadedUris) {
                        // ensure not created by another thread
                        ffmpLoadedUris.get(fsiteKey).get(fsource).get(fhuc)
                                .put(dataUri, dataUri);
                    }
                }
            }
        }
    }

    /**
     * The uri processing
     * 
     * @author dhladky
     * 
     */
    private class FFMPProcessUris implements Runnable {

        final NavigableMap<Date, List<String>> furiMap;

        final boolean fisProductLoad;

        final String fsiteKey;

        final String fsourceName;

        final Date fbarrierTime;

        final String fhuc;

        public FFMPProcessUris(NavigableMap<Date, List<String>> uriMap,
                boolean isProductLoad, String siteKey, String sourceName,
                Date barrierTime, String phuc) {

            this.furiMap = uriMap;
            this.fsiteKey = siteKey;
            this.fbarrierTime = barrierTime;
            this.fisProductLoad = isProductLoad;
            this.fsourceName = sourceName;
            this.fhuc = phuc;

        }

        public void run() {

            SourceXML source = getSourceConfig().getSource(fsourceName);

            if (furiMap != null) {
                for (List<String> uris : furiMap.descendingMap().values()) {
                    for (String uri : uris) {
                        if (uri != null) {
                            FFMPRecord record = new FFMPRecord(uri);
                            if (record.getDataTime().getRefTime()
                                    .after(fbarrierTime)
                                    || source
                                            .getSourceType()
                                            .equals(FFMPSourceConfigurationManager.SOURCE_TYPE.GUIDANCE
                                                    .getSourceType())) {
                                try {

                                    if (!getUriMap(fsiteKey, fsourceName, fhuc)
                                            .containsKey(uri)) {

                                        record = populateFFMPRecord(
                                                fisProductLoad, uri, fsiteKey,
                                                fsourceName, fhuc);

                                        if ((record != null)
                                                && (source != null)) {
                                            record.setExpiration(source
                                                    .getExpirationMinutes(fsiteKey));
                                            record.setRate(source.isRate());
                                        }
                                    }

                                } catch (Exception e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            "FFMP Can't retrieve FFMP URI, "
                                                    + uri, e);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

}
