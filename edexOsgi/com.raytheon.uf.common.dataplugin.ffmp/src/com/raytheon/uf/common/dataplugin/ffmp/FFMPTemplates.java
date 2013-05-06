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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.io.WKBReader;

/**
 * FFMPHucTemplate maker/factory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/29/09      2152       D. Hladky   Initial release
 * 07/09/10      3914       D. Hladky   Localization work
 * 12/13/10      7484       D. Hladky   Service Backup
 * 02/01/13      1569       D.Hladky    Constants
 * 03/01/13      DR13228    G. Zhang    Add VGB county and related code 
 * 02/20/13      1635       D. Hladky   Constants
 * 03/18/13      1817       D. Hladky   Fixed issue with BOX where only 1 HUC was showing up.
 * 04/15/13      1902       M. Duff     Generic List
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPTemplates {

    /** Template file location dir **/
    private String TEMPLATE_FILE_LOC = null;

    /** County Warning Area **/
    private DomainXML primaryCWA = null;

    /** Domains to load **/
    private ArrayList<DomainXML> domains = new ArrayList<DomainXML>();

    /** HASH of the aggregates, lists of pfafs **/
    private HashMap<String, HashMap<String, HashMap<String, LinkedHashMap<Long, ?>>>> domainMap = null;

    private HashMap<String, HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>>> virtualGageBasinsInParentPfaf = null;

    private HashMap<String, HashMap<String, HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>>>> vgbsInCounty = null;// DR
                                                                                                                           // 13228

    private HashMap<String, HashMap<String, LinkedHashMap<String, FFMPVirtualGageBasinMetaData>>> virtualDomainMap = null;

    private final Map<String, SoftReference<Map<Long, Geometry>>> cwaRawGeometries = new ConcurrentHashMap<String, SoftReference<Map<Long, Geometry>>>();

    /** Singleton instance of this class */
    private static FFMPTemplates template = null;

    /** Mode of operation **/
    private MODE mode = null;

    /** Start depth for HUC lookups **/
    private int hucDepthStart = 4;

    /** virtual basins **/
    private boolean virtual = false;

    /** Default max extent buffer **/
    private double maxExtent = 0.0f;

    /** number of HUC levels to use **/
    private int totalHucLevels = 0;

    /** Template configuration **/
    private FFMPTemplateConfigurationManager ftcm = null;

    /** Runner configuration **/
    private FFMPRunConfigurationManager frcm = null;

    /** The FFMP **/
    private FFMPRunXML runner = null;

    /** geometry factory **/
    public static GeometryFactory factory = new GeometryFactory();

    /** ALL huc key **/
    public static long ALL_HUC_KEY = 0l;

    public boolean done = false;

    private final Map<Long, FFMPCounty> countyMap = new HashMap<Long, FFMPCounty>();

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPTemplates.class);

    private final IPathManager pathManager;

    /**
     * Single constructor
     * 
     * @return
     */
    public static synchronized FFMPTemplates getInstance(DomainXML domain,
            String siteKey, MODE mode) {

        if (template == null) {
            template = new FFMPTemplates(domain, mode);
            template.loadTemplate(siteKey, domain.getCwa(), domain.isPrimary());
        }
        return template;
    }

    /**
     * Multiple constructor loads multiple
     * 
     * @return
     */
    public static synchronized FFMPTemplates getInstance(DomainXML primaryCWA,
            MODE mode) {

        if (template == null) {
            template = new FFMPTemplates(primaryCWA, mode);
            template.addDomain(primaryCWA);
        }
        return template;
    }

    /**
     * EDEX constructor
     * 
     * @param awipsShare
     *            DIR
     * @param shareDir
     * @param cwa
     */
    public FFMPTemplates(DomainXML primaryCWA, MODE mode) {
        this.pathManager = PathManagerFactory.getPathManager();
        this.mode = mode;
        this.primaryCWA = primaryCWA;
        init();
    }

    public enum MODE {

        CAVE("CAVE"), EDEX("EDEX");

        private final String mode;

        private MODE(String mode) {
            this.mode = mode;
        }

        public String getMode() {
            return mode;
        }
    };

    /**
     * localization and such
     */
    private void init() {

        TEMPLATE_FILE_LOC = "ffmp" + File.separator + "templates"
                + File.separator;

        domainMap = new HashMap<String, HashMap<String, HashMap<String, LinkedHashMap<Long, ?>>>>();
        virtualDomainMap = new HashMap<String, HashMap<String, LinkedHashMap<String, FFMPVirtualGageBasinMetaData>>>();
        virtualGageBasinsInParentPfaf = new HashMap<String, HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>>>();
        ftcm = FFMPTemplateConfigurationManager.getInstance();
        frcm = FFMPRunConfigurationManager.getInstance();
        vgbsInCounty = new HashMap<String, HashMap<String, HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>>>>();// DR
                                                                                                                        // 13228

        try {
            ftcm.readConfigXml();

            if (ftcm.isRegenerate() && (mode == MODE.EDEX)) {
                dumpTemplates();
            }

            setHucDepthStart(ftcm.getHucDepth());
            setTotalHucLevels(ftcm.getNumberOfHuc());
            setMaxExtent(ftcm.getExtents());
            setVirtual(ftcm.getVirtual());
            domains.add(primaryCWA);

        } catch (Exception e) {
            if (mode == MODE.EDEX) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "No configuration file found, default settings applied");

                // we use 4 because it is the 90% solution as a start point for
                // the analysis. Added check to make sure at least 2 HUC layers
                // are created.
                int preliminarystart = 4;
                // first crack
                ArrayList<Integer> hucParams = FFMPUtils.getHucParameters(
                        preliminarystart, primaryCWA.getCwa());
                int startDepth = hucParams.get(0);
                int numlevels = hucParams.get(1);
                int i = 1;
                // recursively call until we have two layers
                while (numlevels < 2) {
                    int checkDepth = preliminarystart - i;
                    hucParams = FFMPUtils.getHucParameters(checkDepth,
                            primaryCWA.getCwa());
                    startDepth = hucParams.get(0);
                    numlevels = hucParams.get(1);
                    i++;

                    // safety value in case it just won't work with this shape
                    if (checkDepth == 0) {
                        // bail, won't work
                        statusHandler
                                .handle(Priority.ERROR,
                                        "Cannot create a good template. There are not enough unique HUC's to create more than 1 layer.");
                        return;
                    }
                }

                setHucDepthStart(startDepth);
                setTotalHucLevels(numlevels);
                setExtents(20000.0);
                setVirtual(true);

                ArrayList<String> vgbs = new ArrayList<String>();
                domains.add(primaryCWA);

                // create a new entry
                ftcm.setExcludedVGBs(vgbs);
                ftcm.setExtents(getMaxExtent());
                ftcm.setHucDepth(getHucDepthStart());
                ftcm.setNumberOfHuc(getTotalHucLevels());
                ftcm.setVirtual(true);
                ftcm.setRegenerate(false);
                ftcm.saveConfigXml();

            } else {
                statusHandler
                        .handle(Priority.ERROR,
                                "No configuration files can be created, can not create templates");
            }
        }
    }

    public void setTotalHucLevels(int totalHucLevels) {
        this.totalHucLevels = totalHucLevels;
    }

    public int getTotalHucLevels() {
        return totalHucLevels;
    }

    /**
     * Gets the FFMPBasinData template file/object
     * 
     * @param hucName
     * @param cwa
     * @return
     * @throws StorageException
     */
    @SuppressWarnings("unchecked")
    private LinkedHashMap<Long, ?> readTemplateFile(String dataKey, String huc,
            String cwa) {
        LinkedHashMap<Long, ?> map = null;
        long[] list = readDomainList(huc, cwa, dataKey);

        if (huc.equals(FFMPRecord.ALL)) {
            map = new LinkedHashMap<Long, FFMPBasinMetaData>();
            HashMap<Long, FFMPBasinMetaData> protoMap = (HashMap<Long, FFMPBasinMetaData>) readDomainMap(
                    dataKey, huc, cwa);
            // add them to the master hash
            for (long l : list) {
                ((LinkedHashMap<Long, FFMPBasinMetaData>) map).put(l,
                        protoMap.get(l));
            }
        } else {
            map = fromPrimitive(
                    (HashMap<Long, long[]>) readDomainMap(dataKey, huc, cwa),
                    list);
        }

        return map;
    }

    /**
     * Gets the FFMPBasinData template file/object
     * 
     * @param hucName
     * @param cwa
     * @return
     */
    private LinkedHashMap<String, FFMPVirtualGageBasinMetaData> readVGBFile(
            String name, String cwa, String dataKey) {

        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map = new LinkedHashMap<String, FFMPVirtualGageBasinMetaData>();

        HashMap<String, FFMPVirtualGageBasinMetaData> protoMap = readVGBDomainMap(
                dataKey, cwa);
        String[] list = readVGBDomainList(dataKey, cwa);
        // construct ordered map
        for (String lid : list) {
            map.put(lid, protoMap.get(lid));
        }

        return map;
    }

    /**
     * Writes out the byte array from thrift
     * 
     * @param map
     * @param cwa
     * @param dataKey
     * @throws IOException
     */
    private void writeVGBFile(
            LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map,
            String cwa, String dataKey) {

        String[] list = new String[map.keySet().size()];

        int i = 0;
        for (String lid : map.keySet()) {
            list[i] = lid;
            i++;
        }

        try {

            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            LocalizationFile lfmap = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, "VIRTUAL", cwa, "map"));
            LocalizationFile lflist = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, "VIRTUAL", cwa, "list"));

            FileUtil.bytes2File(SerializationUtil.transformToThrift(list),
                    lflist.getFile(), true);
            lflist.save();

            FileUtil.bytes2File(SerializationUtil.transformToThrift(map),
                    lfmap.getFile(), true);
            lfmap.save();

            list = null;

        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (LocalizationOpFailedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Writes out the byte array from thrift
     * 
     * @param huc
     * @param cwa
     * @param map
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    private void writeTemplateFile(String dataKey, String huc, String cwa,
            LinkedHashMap<Long, ?> map) {

        int x = 0;
        long[] list = new long[map.keySet().size()];
        for (long l : map.keySet()) {
            list[x] = l;
            x++;
        }

        try {
            if (huc.equals(FFMPRecord.ALL)) {

                LocalizationContext lc = pathManager.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

                LocalizationFile lflist = pathManager.getLocalizationFile(lc,
                        getAbsoluteFileName(dataKey, huc, cwa, "list"));
                LocalizationFile lfmap = pathManager.getLocalizationFile(lc,
                        getAbsoluteFileName(dataKey, huc, cwa, "map"));

                FileUtil.bytes2File(SerializationUtil.transformToThrift(list),
                        lflist.getFile(), true);
                FileUtil.bytes2File(SerializationUtil.transformToThrift(map),
                        lfmap.getFile(), true);

                lflist.save();
                lfmap.save();

            } else {

                LocalizationContext lc = pathManager.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

                LocalizationFile lfmap = pathManager.getLocalizationFile(lc,
                        getAbsoluteFileName(dataKey, huc, cwa, "map"));
                LocalizationFile lflist = pathManager.getLocalizationFile(lc,
                        getAbsoluteFileName(dataKey, huc, cwa, "list"));

                FileUtil.bytes2File(
                        SerializationUtil
                                .transformToThrift(toPrimitive((LinkedHashMap<Long, ArrayList<Long>>) map)),
                        lfmap.getFile(), true);
                FileUtil.bytes2File(SerializationUtil.transformToThrift(list),
                        lflist.getFile(), true);

                lfmap.save();
                lflist.save();

            }

            list = null;

        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (FileNotFoundException fnfe) {
            fnfe.printStackTrace();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } catch (LocalizationOpFailedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Gets the completed filename
     * 
     * @return
     */

    private String getAbsoluteFileName(String dataKey, String huc, String cwa,
            String appendage) {
        String filename = null;
        if (appendage != null) {
            filename = getTemplateFileLocation() + huc + "-" + cwa + "-"
                    + dataKey + "-" + appendage + ".bin";
        } else {
            filename = getTemplateFileLocation() + huc + "-" + cwa + "-"
                    + dataKey + ".bin";
        }

        return filename;
    }

    /**
     * Gets the template file directory
     * 
     * @return
     */
    public String getTemplateFileLocation() {
        return TEMPLATE_FILE_LOC;
    }

    /**
     * Double for max Extent of the radar
     * 
     * @return
     */
    public double getMaxExtent() {
        return maxExtent;
    }

    /**
     * This maxExtent
     * 
     * @param maxExtent
     */
    public void setMaxExtent(Double maxExtent) {
        this.maxExtent = maxExtent;
    }

    /**
     * Gets a basin
     * 
     * @param pfaf
     * @return
     */
    public FFMPBasinMetaData getBasin(String dataKey, Long pfaf) {
        FFMPBasinMetaData fmbd = null;
        for (DomainXML domain : domains) {
            LinkedHashMap<Long, ?> map = getMap(dataKey, domain.getCwa(),
                    FFMPRecord.ALL);
            fmbd = (FFMPBasinMetaData) map.get(pfaf);
            if (fmbd != null) {
                break;
            }
        }
        return fmbd;
    }

    /**
     * Gets a basin in terms of total loaded domains and sites
     * 
     * @param pfaf
     * @return
     */
    public FFMPBasinMetaData getBasin(Long pfaf) {
        FFMPBasinMetaData fmbd = null;

        if (runner == null) {
            runner = frcm.getRunner(getPrimaryDomain().getCwa());
        }
        for (ProductRunXML product : runner.getProducts()) {
            if (isSiteLoaded(product.getProductKey())) {
                for (DomainXML domain : domains) {
                    LinkedHashMap<Long, ?> map = getMap(
                            product.getProductKey(), domain.getCwa(),
                            FFMPRecord.ALL);
                    fmbd = (FFMPBasinMetaData) map.get(pfaf);
                    if (fmbd != null) {
                        return fmbd;
                    }
                }
            }
        }
        return fmbd;
    }

    /**
     * Load the templates
     */
    public synchronized void loadTemplate(String dataKey, String cwa,
            boolean isPrimary) {
        // synchronized this, don't want half loaded templates
        done = false;

        long time = System.currentTimeMillis();

        List<String> totalHucs = ftcm.getHucLevels();
        for (String huc : totalHucs) {
            statusHandler.handle(Priority.INFO,
                    "FFMPTemplate: Starting site template process " + dataKey
                            + ":" + cwa + ":" + huc);
            // special handling for VGB's
            if (huc.equals(FFMPRecord.VIRTUAL)) {
                getVirtualGageBasins(dataKey, cwa);
            } else {
                getMap(dataKey, cwa, huc);
            }

            statusHandler.handle(Priority.INFO,
                    "FFMPTemplate: Finishing template process " + dataKey
                            + ": " + cwa + ":" + huc);
        }

        statusHandler.handle(Priority.INFO,
                "FFMPTemplate: Template Load for " + dataKey + ": " + cwa
                        + ": took " + (System.currentTimeMillis() - time)
                        + " ms");
    }

    /**
     * Finds the aggregated pfaf of a given pfaf
     * 
     * @param pfaf
     * @return
     */
    public Long findAggregatedPfaf(Long pfaf, String dataKey, String huc) {
        Long rpfaf = null;
        try {
            rpfaf = getAggregatedPfaf(pfaf, dataKey, huc);
        } catch (NullPointerException npe) {
        }
        return rpfaf;
    }

    /**
     * Finds the aggregated pfaf of a given VGB
     * 
     * @param pfaf
     * @return
     */
    public Long findAggregatedVGB(String lid, String dataKey, String huc) {
        Long rpfaf = null;
        try {
            rpfaf = findAggregatedPfaf(
                    getVirtualGageBasinMetaData(dataKey, lid).getParentPfaf(),
                    dataKey, huc);
        } catch (NullPointerException npe) {
            // do nothing, can be null on occasion
        }
        return rpfaf;
    }

    public ArrayList<Long> getAggregatePfafs(Object pfaf, String dataKey,
            String huc, ArrayList<DomainXML> domainList) {
        ArrayList<Long> list = new ArrayList<Long>();
        for (DomainXML domain : domainList) {
            ArrayList<Long> pfafList = getAggregatePfafsByDomain(pfaf, dataKey,
                    domain.getCwa(), huc);
            if (pfafList != null) {
                list.addAll(pfafList);
            }
        }
        return list;
    }

    /**
     * Gets the aggregate mappings for all domains
     * 
     * @param pfaf
     * @return
     */
    public ArrayList<Long> getAggregatePfafs(Object pfaf, String dataKey,
            String huc) {
        ArrayList<Long> list = new ArrayList<Long>();
        for (DomainXML domain : domains) {
            ArrayList<Long> domainList = getAggregatePfafsByDomain(pfaf,
                    dataKey, domain.getCwa(), huc);
            if (domainList != null) {
                list.addAll(domainList);
            }
        }
        return list;
    }

    /**
     * Gets the aggregate mappings for all domains, used by the FFFG dialog If
     * you want ALL and I mean ALL of the basins in a given coverage
     * 
     * @param pfaf
     * @return
     */

    public ArrayList<Long> getAllAggregatePfafs(Object pfaf, String huc) {
        ArrayList<Long> list = new ArrayList<Long>();
        Set<Long> domainSet = new HashSet<Long>();
        if (runner == null) {
            runner = frcm.getRunner(getPrimaryDomain().getCwa());
        }
        for (ProductRunXML product : runner.getProducts()) {
            if (isSiteLoaded(product.getProductKey())) {
                for (DomainXML domain : domains) {
                    ArrayList<Long> domainList = getAggregatePfafsByDomain(
                            pfaf, product.getProductKey(), domain.getCwa(), huc);
                    if (domainList != null) {
                        domainSet.addAll(domainList);
                    }
                }
            }
        }

        Iterator<Long> iter = domainSet.iterator();
        while (iter.hasNext()) {
            list.add(iter.next());
        }

        return list;
    }

    /**
     * Gets the aggreagate mappings by domain
     * 
     * @param pfaf
     * @param domain
     * @param huc
     * @return
     */
    @SuppressWarnings("unchecked")
    public ArrayList<Long> getAggregatePfafsByDomain(Object pfaf,
            String dataKey, String domain, String huc) {

        ArrayList<Long> list = new ArrayList<Long>();

        LinkedHashMap<Long, ArrayList<Long>> map = (LinkedHashMap<Long, ArrayList<Long>>) getMap(
                dataKey, domain, huc);
        try {
            // Virtual Gage Basin centered
            if (pfaf instanceof String) {
                Long pfafl = findAggregatedPfaf(
                        getVirtualGageBasinMetaData(dataKey, (String) pfaf)
                                .getParentPfaf(), dataKey, huc);
                list = map.get(pfafl);
            } else {
                Object object = map.get(pfaf);
                if (object instanceof FFMPBasinMetaData) {
                    list.add(((FFMPBasinMetaData) object).getPfaf());
                } else {
                    list = (ArrayList<Long>) object;
                }
            }
        } catch (NullPointerException npe) {
            // can be null
        }

        return list;
    }

    /**
     * Don't ever call this for anything other than FFTI or FFFG calls
     * 
     * @param pfaf
     * @param huc
     * @return
     */
    @SuppressWarnings("unchecked")
    public ArrayList<FFMPBasinMetaData> getAggregationBasins(Long pfaf,
            String dataKey, String huc) {
        ArrayList<FFMPBasinMetaData> basins = new ArrayList<FFMPBasinMetaData>();
        for (DomainXML domain : domains) {
            ArrayList<Long> basinList = (ArrayList<Long>) getMap(dataKey,
                    domain.getCwa(), huc).get(pfaf);

            if (basinList != null) {
                LinkedHashMap<Long, ?> basinMap = getMap(dataKey,
                        domain.getCwa(), FFMPRecord.ALL);
                for (Long key : basinList) {
                    basins.add((FFMPBasinMetaData) basinMap.get(key));
                }
            }
        }

        return basins;
    }

    /**
     * Get the basin by Lat/Lon
     * 
     * @param coor
     * @return
     */
    @SuppressWarnings("unchecked")
    public FFMPBasinMetaData findBasinByLatLon(String dataKey, Coordinate coor) {
        Point point = factory.createPoint(coor);
        HucLevelGeometriesFactory geomFactory = HucLevelGeometriesFactory
                .getInstance();
        try {
            String huc = "HUC0";
            for (DomainXML domain : getDomains()) {
                String cwa = domain.getCwa();
                Map<Long, Envelope> envMap = geomFactory.getEnvelopes(this,
                        dataKey, cwa, huc);
                Map<Long, ArrayList<Long>> aggrMap = (Map<Long, ArrayList<Long>>) getMap(
                        dataKey, cwa, huc);
                Map<Long, Geometry> geomMap = geomFactory.getGeometries(this,
                        dataKey, cwa, FFMPRecord.ALL);
                for (Long aggrPfaf : aggrMap.keySet()) {
                    Envelope env = envMap.get(aggrPfaf);
                    if ((env != null) && env.contains(coor)) {
                        // in the general envelope, check individual basins
                        for (Long pfaf : aggrMap.get(aggrPfaf)) {
                            if (geomMap.get(pfaf).contains(point)) {
                                return (FFMPBasinMetaData) getMap(dataKey, cwa,
                                        FFMPRecord.ALL).get(pfaf);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    /**
     * Gets the pfaf by basinId
     * 
     * @param basinId
     * @return
     */
    public Long findPfafByBasinId(String dataKey, int basinId) {
        // TODO: make reverse lookup...
        FFMPBasinMetaData basin = null;
        for (DomainXML domain : domains) {
            LinkedHashMap<Long, ?> map = getMap(dataKey, domain.getCwa(),
                    FFMPRecord.ALL);
            for (Long key : map.keySet()) {
                basin = ((FFMPBasinMetaData) map.get(key));
                if (basin.getBasinId() == basinId) {
                    return basin.getPfaf();
                }
            }
        }
        return null;
    }

    /**
     * Finds the center of an aggregation of basins (Roughly)
     * 
     * @param arrayList
     * @return
     */
    public Coordinate findAggregationCenter(Long pfaf, String dataKey,
            String huc) {
        HucLevelGeometriesFactory geomFactory = HucLevelGeometriesFactory
                .getInstance();
        Envelope env = null;

        for (DomainXML domain : getDomains()) {
            String cwa = domain.getCwa();
            Envelope tmp = null;

            try {
                tmp = geomFactory.getEnvelope(this, dataKey, cwa, huc, pfaf);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid Geometry Envelope....dataKey: " + dataKey
                                + " CWA: " + cwa);
            }

            if (tmp != null) {
                if (env == null) {
                    env = tmp;
                } else {
                    env.expandToInclude(tmp);
                }
                break;
            }
        }

        if (env != null) {
            return env.centre();
        }

        return null;
    }

    /**
     * Check to see if file is there
     * 
     * @param hucName
     * @return
     */
    private boolean templateGood(String dataKey, String huc, String cwa) {

        boolean good = false;
        boolean good2 = false;
        try {

            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            LocalizationFile listf = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, huc, cwa, "list"));
            LocalizationFile mapf = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, huc, cwa, "map"));

            if (listf.exists()) {
                good = true;
            } else {
                good = false;
            }
            if (mapf.exists()) {
                good2 = true;
            } else {
                good2 = false;
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "FFMPTemplates: Template file not good!");
        }

        if (good && good2) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Gets the starting index for searching
     * 
     * @return
     */
    public int getHucDepthStart() {
        return hucDepthStart;
    }

    /**
     * Gets the starting index for searching
     * 
     * @return
     */
    public void setHucDepthStart(int hucDepthStart) {
        this.hucDepthStart = hucDepthStart;
    }

    /**
     * Get a listing of the counties in the FFMP monitored area
     * 
     * @return
     */
    public FFMPCounties getCounties(String dataKey) {
        ArrayList<FFMPCounty> countyList = new ArrayList<FFMPCounty>();
        FFMPCounty county;

        try {

            for (DomainXML domain : domains) {
                for (Long key : getMap(dataKey, domain.getCwa(),
                        FFMPRecord.COUNTY).keySet()) {
                    if (countyMap.get(key) == null) {
                        county = FFMPUtils.getCounty(key, MODE.CAVE.getMode());
                        if (county != null) {
                            if ((county.getGid() != null)
                                    && (county.getCountyFips() != null)
                                    && (county.getCountyName() != null)
                                    && (county.getDisplayFips() != null)
                                    && (county.getState() != null)) {
                                countyMap.put(key, county);
                            }
                        }
                    } else {
                        county = countyMap.get(key);
                    }
                    if ((county != null) && !countyList.contains(county)) {
                        countyList.add(county);
                    }
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        FFMPCounties counties = new FFMPCounties(countyList);

        return counties;
    }

    /**
     * Finds the parent aggregated pfaf of a given pfaf.
     * 
     * @param pfaf
     * @param huc
     * @return
     */
    @SuppressWarnings("unchecked")
    public Long getAggregatedPfaf(Long key, String dataKey, String huc) {
        if (huc.equals(FFMPRecord.ALL)) {
            return key;
        } else if (huc.equals(FFMPRecord.COUNTY)) {
            // TODO: use envelope contains to limit search area?
            for (DomainXML domain : domains) {
                LinkedHashMap<Long, ?> map = getMap(dataKey, domain.getCwa(),
                        huc);
                for (Entry<Long, ?> entry : map.entrySet()) {
                    if (((ArrayList<Long>) entry.getValue()).contains(key)) {
                        return entry.getKey();
                    }
                }
            }
        } else {
            int hucNum = Integer.parseInt(huc.substring(3));
            int endIndex = getHucDepthStart() + hucNum;
            return Long.parseLong(key.toString().substring(0, endIndex));
        }
        return null;
    }

    /**
     * Find the extents of this domain
     * 
     * @param cwa
     * @return
     */
    public Geometry getCWAExtents(String cwa) {

        Geometry cwaGeometry = FFMPUtils.getCwaGeometry(cwa, mode.getMode());

        return cwaGeometry;
    }

    /**
     * Find the extents of the collective sites
     * 
     * @param cwa
     * @return
     */
    public String getSiteExtents(String dataKey) {

        String siteExtents = null;
        // figure out which product to apply this to
        if (runner == null) {
            runner = frcm.getRunner(getPrimaryDomain().getCwa());
        }

        ProductRunXML product = runner.getProduct(dataKey);
        FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager
                .getInstance();
        SourceXML primeSource = fscm.getSource(product.getProductName());

        if (primeSource.getDataType().equals(
                FFMPSourceConfigurationManager.DATA_TYPE.XMRG.getDataType())) {

            Rectangle rect = null;

            try {
                rect = HRAPCoordinates.getHRAPCoordinates();
                rect.setBounds(rect.x * primeSource.getHrapGridFactor(), rect.y
                        * primeSource.getHrapGridFactor(), rect.width
                        * primeSource.getHrapGridFactor(), rect.height
                        * primeSource.getHrapGridFactor());

                HRAPSubGrid hrapgrid = new HRAPSubGrid(rect,
                        primeSource.getHrapGridFactor());
                Geometry geo = hrapgrid.getGeometry();
                siteExtents = FFMPUtils.getGeometryText(geo);

            } catch (Exception e) {
                e.printStackTrace();
            }
        } else if (primeSource.getDataType().equals(
                FFMPSourceConfigurationManager.DATA_TYPE.RADAR.getDataType())) {

            Coordinate siteCoor = FFMPUtils.getRadarCenter(dataKey,
                    mode.getMode());
            Polygon poly = FFMPUtils.getRadarPolygon(siteCoor,
                    120.0 * ScanUtils.NMI_TO_KM * 1000.0);
            siteExtents = FFMPUtils.getRadarPolygonText(poly);

        } else if (primeSource.getDataType().equals(
                FFMPSourceConfigurationManager.DATA_TYPE.GRID.getDataType())) {
            statusHandler.handle(Priority.PROBLEM,
                    "GRID Not yet implemented:  " + dataKey);

        } else if (primeSource.getDataType().equals(
                FFMPSourceConfigurationManager.DATA_TYPE.PDO.getDataType())) {
            statusHandler.handle(Priority.PROBLEM, "PDO Not yet implemented:  "
                    + dataKey);

        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "No method of extracting siteExtents for this type:  "
                            + dataKey);
        }

        return siteExtents;
    }

    /**
     * Sets the extents
     * 
     * @param extents
     */
    public void setExtents(double maxExtent) {
        this.maxExtent = maxExtent;
    }

    private void setVirtual(boolean virtual) {
        this.virtual = virtual;
    }

    /**
     * gets the virtuals or not
     * 
     * @return
     */
    public boolean getVirtual() {
        return virtual;
    }

    /**
     * Get the maps from storage or create them
     * 
     * @param huc
     * @param cwa
     * @return
     */
    @SuppressWarnings("unchecked")
    private LinkedHashMap<Long, ?> readMap(String dataKey, String cwa,
            String huc) {

        LinkedHashMap<Long, ?> map = null;

        if (!templateGood(dataKey, huc, cwa)) {
            if (mode == MODE.EDEX) {
                ArrayList<Long> list = null;
                Map<Long, TreeSet<Long>> aggrPfafToAllChildPfafsMap = null;

                if (huc.equals(FFMPRecord.ALL)) {
                    map = loadBasins(dataKey, cwa, FFMPUtils.getBasins(cwa,
                            getMaxExtent(), getSiteExtents(dataKey),
                            mode.getMode()));

                } else if (huc.equals(FFMPRecord.COUNTY)) {
                    list = getCountyFips(cwa, dataKey);
                } else {
                    int myHucNum = Integer.parseInt(huc.substring(3));
                    TreeSet<Long> aggrPfafs = new TreeSet<Long>();
                    aggrPfafToAllChildPfafsMap = new HashMap<Long, TreeSet<Long>>();
                    if (myHucNum + 1 == getTotalHucLevels()) {
                        Set<Long> allPfafs = getMap(dataKey, cwa,
                                FFMPRecord.ALL).keySet();
                        for (Long pfaf : allPfafs) {
                            int endIndex = getHucDepthStart() + myHucNum;
                            Long aggrPfaf = Long.parseLong(pfaf.toString()
                                    .substring(0, endIndex));
                            aggrPfafs.add(aggrPfaf);
                            TreeSet<Long> allChildPfafs = aggrPfafToAllChildPfafsMap
                                    .get(aggrPfaf);
                            if (allChildPfafs == null) {
                                allChildPfafs = new TreeSet<Long>();
                                aggrPfafToAllChildPfafsMap.put(aggrPfaf,
                                        allChildPfafs);
                            }
                            allChildPfafs.add(pfaf);
                        }
                    } else {
                        String childHuc = "HUC" + (myHucNum + 1);
                        LinkedHashMap<Long, List<Long>> childHucMap = (LinkedHashMap<Long, List<Long>>) getMap(
                                dataKey, cwa, childHuc);
                        for (Entry<Long, List<Long>> entry : childHucMap
                                .entrySet()) {
                            Long aggrPfaf = new Long(
                                    entry.getKey().longValue() / 10);
                            aggrPfafs.add(aggrPfaf);
                            TreeSet<Long> childAggrPfafs = aggrPfafToAllChildPfafsMap
                                    .get(aggrPfaf);
                            if (childAggrPfafs == null) {
                                childAggrPfafs = new TreeSet<Long>();
                                aggrPfafToAllChildPfafsMap.put(aggrPfaf,
                                        childAggrPfafs);
                            }
                            childAggrPfafs.addAll(entry.getValue());
                        }
                    }
                    list = new ArrayList<Long>(aggrPfafs);
                }

                if (!huc.equals(FFMPRecord.ALL)) {
                    map = new LinkedHashMap<Long, ArrayList<Long>>();
                    Map<Long, Geometry> rawGeometries = null;
                    LinkedHashMap<Long, FFMPBasinMetaData> allMap = (LinkedHashMap<Long, FFMPBasinMetaData>) getMap(
                            dataKey, cwa, FFMPRecord.ALL);

                    for (Long key : list) {
                        ArrayList<Long> innerList = null;

                        if (huc.equals(FFMPRecord.COUNTY)) {
                            innerList = new ArrayList<Long>();
                            ArrayList<?> countyInfo = FFMPUtils.getCountyInfo(
                                    key, mode.getMode());

                            PreparedGeometry countyGeometry = PreparedGeometryFactory
                                    .prepare((Geometry) countyInfo.get(0));
                            String countyName = (String) countyInfo.get(1);
                            String state = (String) countyInfo.get(2);
                            statusHandler.handle(Priority.INFO,
                                    "Processing --- County: " + countyName
                                            + " State: " + state);
                            boolean primary = false;
                            if (cwa.equals(getPrimaryDomain().getCwa())) {
                                primary = true;
                            }

                            if (rawGeometries == null) {
                                rawGeometries = getRawGeometries(dataKey, cwa);
                            }

                            for (Long pfaf : allMap.keySet()) {

                                if (countyGeometry.contains(rawGeometries.get(
                                        pfaf).getCentroid())) {
                                    FFMPBasinMetaData basin = allMap.get(pfaf);
                                    basin.setCounty(countyName);
                                    basin.setState(state);
                                    basin.setPrimaryCwa(primary);
                                    innerList.add(pfaf);
                                }
                            }

                        } else {
                            innerList = new ArrayList<Long>(
                                    aggrPfafToAllChildPfafsMap.get(key));
                        }

                        statusHandler.handle(Priority.DEBUG, "HUC: " + huc
                                + " INNERLIST SIZE:  " + innerList.size());

                        if ((innerList != null) && (innerList.size() > 0)) {

                            ((HashMap<Long, ArrayList<Long>>) map).put(key,
                                    innerList);
                        }
                    }

                    // trigger the write of the "ALL" now that the counties are
                    // set.
                    if (huc.equals(FFMPRecord.COUNTY)) {

                        if (allMap != null) {
                            writeTemplateFile(dataKey, FFMPRecord.ALL, cwa,
                                    allMap);
                        }
                    }
                }

                aggrPfafToAllChildPfafsMap = null;
                list = null;

                // clean up
                System.runFinalization();
                System.gc();

                if (!huc.equals(FFMPRecord.ALL) && map != null) {
                    writeTemplateFile(dataKey, huc, cwa, map);
                }
            }
        } else {
            map = readTemplateFile(dataKey, huc, cwa);
        }
        return map;
    }

    /**
     * load up the maps
     * 
     * @param huc
     * @return
     */
    public synchronized LinkedHashMap<Long, ?> getMap(String dataKey, String cwa, String huc) {

        LinkedHashMap<Long, ?> map = null;
        HashMap<String, LinkedHashMap<Long, ?>> hucMap = null;

        HashMap<String, HashMap<String, LinkedHashMap<Long, ?>>> cwaMap = domainMap
                .get(dataKey);
        if (cwaMap == null) {
            cwaMap = new HashMap<String, HashMap<String, LinkedHashMap<Long, ?>>>();
            domainMap.put(dataKey, cwaMap);
        }

        hucMap = cwaMap.get(cwa);
        if (hucMap == null) {
            hucMap = new HashMap<String, LinkedHashMap<Long, ?>>();
            cwaMap.put(cwa, hucMap);
        }

        map = hucMap.get(huc);
        if (map == null) {
            map = readMap(dataKey, cwa, huc);
            hucMap.put(huc, map);
        }

        return map;
    }

    /**
     * Gets the template config manager
     * 
     * @return
     */
    public FFMPTemplateConfigurationManager getTemplateMgr() {
        return ftcm;
    }

    /**
     * Read the file or generate VGB's for primary domain
     * 
     * @param cwa
     * @return
     */
    private LinkedHashMap<String, FFMPVirtualGageBasinMetaData> readVirtualGageBasins(
            String dataKey, String cwa) {

        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> virtuals = null;

        if (!templateGood(dataKey, "VIRTUAL", cwa)) {
            if (mode == MODE.EDEX) {
                double extent = 0.0;
                if (cwa.equals(getPrimaryDomain().getCwa())) {
                    extent = getMaxExtent();
                }

                virtuals = FFMPUtils.getVirtualGageBasins(extent, cwa,
                        mode.getMode());
                ArrayList<String> removes = new ArrayList<String>();
                Map<Long, Geometry> rawGeometries = getRawGeometries(dataKey,
                        cwa);
                // assign pfafs

                for (String lid : virtuals.keySet()) {
                    FFMPVirtualGageBasinMetaData vb = virtuals.get(lid);
                    Point vgbPoint = factory.createPoint(vb.getCoordinate());
                    // Expensive..., use envelopes first to get rough idea and
                    // skip
                    // unnecessary checks
                    LinkedHashMap<Long, ?> map = getMap(dataKey, cwa,
                            FFMPRecord.ALL);
                    for (Entry<Long, ?> entry : map.entrySet()) {
                        Long pfaf = entry.getKey();
                        Geometry geometry = rawGeometries.get(pfaf);
                        if (geometry != null) {
                            if (vgbPoint.within(geometry)) {
                                FFMPBasinMetaData basin = (FFMPBasinMetaData) entry
                                        .getValue();
                                vb.setParentPfaf(pfaf);
                                vb.setCwa(basin.getCwa());
                                break;
                            }
                        }
                    }
                    // if it dosen't have a pfaf, we can't locate it.
                    if (vb.getParentPfaf() == null) {
                        removes.add(vb.getLid());
                    }
                }
                // gets rid of excluded ones
                if ((ftcm.getExcludedVGBs() != null)
                        && (ftcm.getExcludedVGBs().getLids().size() > 0)) {
                    for (String lid : ftcm.getExcludedVGBs().getLids()) {
                        removes.add(lid);
                    }
                }

                for (String lid : removes) {
                    virtuals.remove(lid);
                }

                // write them out
                writeVGBFile(virtuals, cwa, dataKey);
            }
        } else {

            virtuals = readVGBFile("VIRTUAL", cwa, dataKey);

        }

        return virtuals;
    }

    /**
     * Generate the Virtual Gage Basins Meta Data for primary domain
     * 
     * @return
     */
    public synchronized LinkedHashMap<String, FFMPVirtualGageBasinMetaData> getVirtualGageBasins(
            String dataKey, String cwa) {

        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map = null;

        HashMap<String, LinkedHashMap<String, FFMPVirtualGageBasinMetaData>> virtualmap = virtualDomainMap
                .get(dataKey);
        if (virtualmap == null) {
            virtualmap = new HashMap<String, LinkedHashMap<String, FFMPVirtualGageBasinMetaData>>();
        }

        virtualDomainMap.put(dataKey, virtualmap);

        map = virtualmap.get(cwa);
        if (map == null) {
            map = readVirtualGageBasins(dataKey, cwa);
            if (map == null) {
                return map;
            }

            virtualmap.put(cwa, map);

            HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>> vgbMap = new HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>>();// DR
                                                                                                                                             // 13228

            HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>> virtualGageBasins = new HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>(
                    (int) (map.size() * 1.3));

            for (FFMPVirtualGageBasinMetaData vgb : map.values()) {
                Long id = vgb.getParentPfaf();
                String stateCommaCnty = vgb.getState() + ", " + vgb.getCounty();// DR
                                                                                // 13228
                                                                                // see
                                                                                // getCountyStateName(,)

                ArrayList<FFMPVirtualGageBasinMetaData> list = virtualGageBasins
                        .get(id);
                ArrayList<FFMPVirtualGageBasinMetaData> list2 = vgbMap
                        .get(stateCommaCnty.toUpperCase());// DR 13228

                if (list == null) {
                    list = new ArrayList<FFMPVirtualGageBasinMetaData>();
                    virtualGageBasins.put(id, list);
                }
                list.add(vgb);

                // DR 13228
                if (list2 == null) {
                    list2 = new ArrayList<FFMPVirtualGageBasinMetaData>();
                    vgbMap.put(stateCommaCnty.toUpperCase(), list2);
                }
                list2.add(vgb);// DR 13228
            }

            HashMap<String, HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>>> vMapCounty = vgbsInCounty
                    .get(dataKey);// DR 13228
            HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>> virtualMapPfaf = virtualGageBasinsInParentPfaf
                    .get(dataKey);
            if (virtualMapPfaf == null) {
                virtualMapPfaf = new HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>>();

            }

            // DR 13228
            if (vMapCounty == null) {
                vMapCounty = new HashMap<String, HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>>>();// DR
                                                                                                             // 13228
            }

            vMapCounty.put(cwa, vgbMap);// DR 13228
            vgbsInCounty.put(dataKey, vMapCounty);// DR 13228

            virtualMapPfaf.put(cwa, virtualGageBasins);
            virtualGageBasinsInParentPfaf.put(dataKey, virtualMapPfaf);
        }
        return map;
    }

    /**
     * Gets the Virtual Gage Basin MetaData
     * 
     * @param lid
     * @return
     */
    public FFMPVirtualGageBasinMetaData getVirtualGageBasinMetaData(
            String dataKey, String lid) {
        FFMPVirtualGageBasinMetaData vgbmd = null;
        for (DomainXML domain : domains) {
            LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map = getVirtualGageBasins(
                    dataKey, domain.getCwa());
            if (map.containsKey(lid)) {
                vgbmd = map.get(lid);
                break;
            }
        }

        return vgbmd;
    }

    /**
     * Gets the Virtual Gage Basin MetaData
     * 
     * @param pfaf
     * @return
     */
    public synchronized ArrayList<FFMPVirtualGageBasinMetaData> getVirtualGageBasinMetaData(
            String dataKey, String cwa, Long parentPfaf) {

        if (cwa == null) {
            for (DomainXML domain : domains) {
                ArrayList<FFMPVirtualGageBasinMetaData> result = getVirtualGageBasinMetaData(
                        dataKey, domain.getCwa(), parentPfaf);
                if (result != null)
                    return result;
            }
            return null;
        }

        HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>> virtualMap = virtualGageBasinsInParentPfaf
                .get(dataKey);

        HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>> map = virtualMap
                .get(cwa);

        if (map != null)
            return map.get(parentPfaf);

        return null;
    }

    /**
     * Finds the Lid string used by the aggregated VGB's
     * 
     * @param fbmd
     * @param huc
     * @return
     */
    public String getVGBLidString(FFMPBasinMetaData fbmd, Long aggPfaf,
            String huc) {
        String lid = null;
        if (huc.equals("COUNTY")) {
            lid = fbmd.getState() + ", " + fbmd.getCounty();
        } else {
            lid = fbmd.getHucName() + "-" + aggPfaf;
        }
        return lid;
    }

    /**
     * Gets you the list of pfafs that have VGB's for a given HUC level DONT
     * EVER CALL THIS WITH "ALL" HUC level use getVirtualGageBasins() for that
     * 
     * @param huc
     * @return
     */
    public ArrayList<Long> getVGBsInAggregate(Long pfaf, String dataKey,
            String huc) {
        ArrayList<Long> vgbPfafs = null;
        for (Long iPfaf : getAggregatePfafs(pfaf, dataKey, huc)) {
            if (checkVirtualGageBasinMetaData(dataKey, iPfaf)) {
                if (vgbPfafs == null) {
                    vgbPfafs = new ArrayList<Long>();
                }
                vgbPfafs.add(iPfaf);
            }
        }

        return vgbPfafs;
    }

    /**
     * Check for VGB's in aggregate pfaf
     * 
     * @param pfaf
     * @param dataKey
     * @param huc
     * @return
     */
    public boolean checkVGBsInAggregate(Long pfaf, String dataKey, String huc) {
        for (Long iPfaf : getAggregatePfafs(pfaf, dataKey, huc)) {
            if (checkVirtualGageBasinMetaData(dataKey, iPfaf)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets the Virtual Gage Basin MetaData
     * 
     * @param pfaf
     * @return
     */
    public synchronized boolean checkVirtualGageBasinMetaData(String dataKey,
            Long pfaf) {

        HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>> virtualMap = virtualGageBasinsInParentPfaf
                .get(dataKey);

        for (DomainXML domain : domains) {

            HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>> map = virtualMap
                    .get(domain.getCwa());
            if (map != null) {
                ArrayList<FFMPVirtualGageBasinMetaData> list = map.get(pfaf);
                if (list != null && !list.isEmpty())
                    return true;
            }
        }

        return false;
    }

    /**
     * Gets you the list of pfafs that have VGB's for a given HUC level DONT
     * EVER CALL THIS WITH "ALL" HUC level use getVirtualGageBasins() for that
     * 
     * @param huc
     * @return
     */
    /*
     * // not used, so not fixed public ArrayList<Long>
     * getVGBsInDomainAggregate(Long pfaf, String huc, String cwa, String
     * dataKey) { ArrayList<Long> vgbPfafs = null; for (Long iPfaf :
     * getAggregatePfafs(pfaf, dataKey, huc)) { if
     * (getVirtualGageBasinMetaData(dataKey, iPfaf) != null) { if (vgbPfafs ==
     * null) { vgbPfafs = new ArrayList<Long>(); } vgbPfafs.add(iPfaf); } }
     * 
     * return vgbPfafs; }
     */

    /**
     * Gets list of aggregate pfafs that have VGB's within them * DONT EVER CALL
     * THIS WITH "ALL" HUC level
     * 
     * @param huc
     * @param cwa
     * @return
     */
    /*
     * // not used to not converted public ArrayList<Long>
     * getAggregateVGBs(String dataKey, String huc) { ArrayList<Long> vgbPfafs =
     * null; for (DomainXML domain : domains) { for (Long iPfaf :
     * getMap(dataKey, domain.getCwa(), huc).keySet()) { if
     * (getVGBsInAggregate(iPfaf, dataKey, huc) != null) { if (vgbPfafs == null)
     * { vgbPfafs = new ArrayList<Long>(); } vgbPfafs.add(iPfaf); } } }
     * 
     * return vgbPfafs; }
     */

    /**
     * Gets the Virtual Gage Basin MetaData
     * 
     * @param pfaf
     * @return
     */
    public synchronized ArrayList<Long> getVirtualGageBasinLookupIds(
            String dataKey, Long pfaf, String huc, String rowName) {
        if (isCountyRow(huc, rowName)) {
            return getVgbLookupIdsByCounty(dataKey, pfaf, huc, rowName);
        }
        HashMap<String, HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>>> virtualMap = virtualGageBasinsInParentPfaf
                .get(dataKey);

        for (DomainXML domain : domains) {

            HashMap<Long, ArrayList<FFMPVirtualGageBasinMetaData>> map = virtualMap
                    .get(domain.getCwa());
            if (map != null) {
                ArrayList<FFMPVirtualGageBasinMetaData> list = map.get(pfaf);
                if (list != null && !list.isEmpty()) {
                    ArrayList<Long> result = new ArrayList<Long>();
                    for (FFMPVirtualGageBasinMetaData md : list)
                        result.add(md.getLookupId());
                    return result;
                }
            }
        }

        return new ArrayList<Long>();

    }

    /**
     * Writes out all basins from DB by CWA
     * 
     * @param results
     */
    private LinkedHashMap<Long, FFMPBasinMetaData> loadBasins(String siteKey,
            String cwa, Object[] results) {
        LinkedHashMap<Long, FFMPBasinMetaData> basins = new LinkedHashMap<Long, FFMPBasinMetaData>();
        WKBReader reader = new WKBReader();
        FFMPBasinMetaData basin = null;
        int upstreamDepth = 0;

        if (results != null && results.length > 0) {
            for (int i = 0; i < results.length; i++) {
                Object[] row = (Object[]) results[i];
                basin = FFMPUtils.getMetaDataBasin(row, mode.getMode());

                if (upstreamDepth == 0) {
                    upstreamDepth = basin.getStreamPfafs().size();
                }

                basin.removeZeros();
                basins.put(basin.getPfaf(), basin);

                if ((row.length >= (upstreamDepth + 9))
                        && (row[upstreamDepth + 9] != null)) {
                    try {
                        addRawGeometry(siteKey, cwa, basin.getPfaf(), reader
                                .read((byte[]) row[upstreamDepth + 9])
                                .buffer(0));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Query to basins table returned no basin geometries within given area: "
                            + siteKey);
        }

        System.runFinalization();
        System.gc();

        return basins;
    }

    /**
     * compress to primitive
     * 
     * @param list
     * @return
     */
    private HashMap<Long, long[]> toPrimitive(
            LinkedHashMap<Long, ArrayList<Long>> map) {
        HashMap<Long, long[]> primList = new HashMap<Long, long[]>();
        for (Long l : map.keySet()) {
            long[] longs = new long[map.get(l).size()];
            for (int i = 0; i < map.get(l).size(); i++) {
                longs[i] = map.get(l).get(i).longValue();
            }
            primList.put(l.longValue(), longs);
        }
        return primList;
    }

    /**
     * back to LinkedHash
     * 
     * @param longs
     * @return
     */
    private LinkedHashMap<Long, ArrayList<Long>> fromPrimitive(
            HashMap<Long, long[]> longs, long[] list) {
        // reconstructs the LinkedHash in order
        LinkedHashMap<Long, ArrayList<Long>> map = new LinkedHashMap<Long, ArrayList<Long>>();
        for (Long l : list) {
            ArrayList<Long> longa = new ArrayList<Long>();
            for (int i = 0; i < longs.get(l).length; i++) {
                longa.add(longs.get(l)[i]);
            }
            map.put(l, longa);
        }
        return map;
    }

    /**
     * Get the basin ID of the county
     * 
     * @param name
     * @return
     */
    @SuppressWarnings("unchecked")
    public Long getCountyIdByNameState(String dataKey, String nameState) {
        String[] parts = nameState.split(",");
        if (parts.length == 2) {
            for (DomainXML domain : domains) {
                LinkedHashMap<Long, ArrayList<Long>> countyMap = (LinkedHashMap<Long, ArrayList<Long>>) getMap(
                        dataKey, domain.getCwa(), FFMPRecord.ALL);
                for (Long id : countyMap.keySet()) {
                    for (Long key : countyMap.get(id)) {
                        FFMPBasinMetaData basin = getBasin(dataKey, key);
                        if (basin.getState().equals(parts[0].trim())
                                && basin.getCounty().equals(parts[1].trim())) {
                            return id;
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * Sets the domains to load for this template instance
     * 
     * @param domains
     */
    public synchronized void setDomains(ArrayList<DomainXML> domains) {
        this.domains = domains;
    }

    /**
     * Gets the domains in this template
     * 
     * @return
     */
    public synchronized ArrayList<DomainXML> getDomains() {
        return domains;
    }

    /**
     * Add domains to the templates, This method is used by EDEX to add domains
     * 
     * @param domain
     */
    public synchronized void addDomain(DomainXML domain) {
        if (runner == null) {
            runner = frcm.getRunner(getPrimaryDomain().getCwa());
        }
        // create template for all of the site/domain combos
        // eliminate duplicates

        if (runner != null) {

            ArrayList<String> products = new ArrayList<String>();
            for (ProductRunXML product : runner.getProducts()) {
                if (!products.contains(product.getProductKey())) {
                    products.add(product.getProductKey());
                }
            }

            for (String dataKey : products) {
                loadTemplate(dataKey, domain.getCwa(), domain.isPrimary());
            }

            if (!domains.contains(domain)) {
                domains.add(domain);
            }
        } else {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "The Domain: "
                                    + domain.getCwa()
                                    + " your runner is null (not truly a primary domain), check your FFMPRunConfig.xml");
        }

        done = true;
    }

    /**
     * Add domains to the templates, This method is used by EDEX to add domains
     * 
     * @param domain
     */
    public synchronized void addDomain(String dataKey, DomainXML domain) {

        loadTemplate(dataKey, domain.getCwa(), domain.isPrimary());

        if (!domains.contains(domain)) {
            domains.add(domain);
        }
        done = true;
    }

    /**
     * dump a domain on the fly, CAVE side
     * 
     * @param domainName
     */
    public synchronized void removeDomain(String dataKey, String domainName) {

        domainMap.get(dataKey).get(domainName);
        virtualDomainMap.get(dataKey).get(domainName);

        for (DomainXML domain : domains) {
            if (domain.getCwa().equals(domainName)) {
                domains.remove(domain);
                break;
            }
        }
    }

    /**
     * dump a dataKey
     * 
     * @param domainName
     */
    public synchronized void removeDataKey(String dataKey) {
        domainMap.remove(dataKey);
        virtualDomainMap.remove(dataKey);
    }

    /**
     * Set the primary domain
     */
    public void setPrimaryDomain(DomainXML primaryCWA) {
        this.primaryCWA = primaryCWA;
    }

    /**
     * Gets the primary
     * 
     * @return
     */
    public DomainXML getPrimaryDomain() {
        return primaryCWA;
    }

    /**
     * Gets the list for all of the pfafs
     * 
     * @param huc
     * @param cwa
     * @return
     */
    public long[] readDomainList(String huc, String cwa, String dataKey) {

        long[] list = null;

        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, huc, cwa, "list"));

        try {
            list = (long[]) SerializationUtil.transformFromThrift(FileUtil
                    .file2bytes(f.getFile(), true));
        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return list;
    }

    /**
     * Reads the actual domain map
     * 
     * @param huc
     * @param cwa
     * @return
     */
    @SuppressWarnings("unchecked")
    public HashMap<Long, ?> readDomainMap(String dataKey, String huc, String cwa) {

        HashMap<Long, ?> map = null;

        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, huc, cwa, "map"));

        try {
            if (huc.equals(FFMPRecord.ALL)) {

                map = (HashMap<Long, FFMPBasinMetaData>) SerializationUtil
                        .transformFromThrift(FileUtil.file2bytes(f.getFile(),
                                true));
            } else {
                map = (HashMap<Long, long[]>) SerializationUtil
                        .transformFromThrift(FileUtil.file2bytes(f.getFile(),
                                true));
            }
        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return map;
    }

    /**
     * Reads the actual VGB domain map
     * 
     * @param huc
     * @param cwa
     * @return
     */
    @SuppressWarnings("unchecked")
    public HashMap<String, FFMPVirtualGageBasinMetaData> readVGBDomainMap(
            String dataKey, String cwa) {

        HashMap<String, FFMPVirtualGageBasinMetaData> map = null;

        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, FFMPRecord.VIRTUAL, cwa, "map"));

        try {
            map = (HashMap<String, FFMPVirtualGageBasinMetaData>) SerializationUtil
                    .transformFromThrift(FileUtil.file2bytes(f.getFile(), true));
        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return map;
    }

    /**
     * Reads the actual VGB domain map
     * 
     * @param huc
     * @param cwa
     * @return
     */
    public String[] readVGBDomainList(String dataKey, String cwa) {

        String[] list = null;

        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, FFMPRecord.VIRTUAL, cwa, "list"));

        try {
            list = (String[]) SerializationUtil.transformFromThrift(FileUtil
                    .file2bytes(f.getFile(), true));
        } catch (SerializationException se) {
            se.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return list;
    }

    public void verifyUnifiedGeometries(String huc, String domain) {
        HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                .getInstance();

        if (runner == null) {
            runner = frcm.getRunner(getPrimaryDomain().getCwa());
        }

        for (ProductRunXML product : runner.getProducts()) {
            if (isSiteLoaded(product.getProductKey())) {
                long t1 = System.currentTimeMillis();
                statusHandler.handle(Priority.INFO,
                        "FFMPTemplate: Starting geometry unify process "
                                + product.getProductKey() + ": " + domain
                                + ": " + huc);
                try {
                    hucGeomFactory.getGeometries(this, product.getProductKey(),
                            domain, huc);
                    hucGeomFactory.getEnvelopes(this, product.getProductKey(),
                            domain, huc);
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Failed persisting unified huc envelopes for huc: "
                                    + product.getProductKey() + ": " + huc
                                    + ": " + domain, e);
                }

                hucGeomFactory.clear();

                long t2 = System.currentTimeMillis();
                statusHandler.handle(Priority.INFO,
                        "FFMPTemplate: Finished geometry unify process "
                                + product.getProductKey() + ": " + domain
                                + ": " + huc + " in [" + (t2 - t1) + "] ms");
            }
        }
    }

    public void addRawGeometry(String siteKey, String cwa, Long pfaf, Geometry g) {

        String compositeKey = siteKey + cwa;
        SoftReference<Map<Long, Geometry>> rawGeomRef = cwaRawGeometries
                .get(compositeKey);
        Map<Long, Geometry> pfafGeometries = null;

        if (rawGeomRef != null) {
            pfafGeometries = rawGeomRef.get();
        }

        if (pfafGeometries == null) {
            pfafGeometries = new HashMap<Long, Geometry>(4000);
            cwaRawGeometries.put(compositeKey,
                    new SoftReference<Map<Long, Geometry>>(pfafGeometries));
        }

        pfafGeometries.put(pfaf, g);
    }

    /**
     * 
     * @param cwa
     * @return
     */
    public Map<Long, Geometry> getRawGeometries(String siteKey, String cwa) {

        String compositeKey = siteKey + cwa;
        SoftReference<Map<Long, Geometry>> rawGeomRef = cwaRawGeometries
                .get(compositeKey);
        Map<Long, Geometry> pfafGeometries = null;
        if (rawGeomRef != null) {
            pfafGeometries = rawGeomRef.get();
        }
        if (pfafGeometries == null) {
            // TODO: add sync locking per cwa
            long t0 = System.currentTimeMillis();
            pfafGeometries = FFMPUtils.getRawGeometries(getMap(siteKey, cwa,
                    FFMPRecord.ALL).keySet());
            long t1 = System.currentTimeMillis();
            System.out.println("Retrieval of raw geometries for site "
                    + siteKey + " cwa " + cwa + " took " + (t1 - t0) + " ms.");
            cwaRawGeometries.put(compositeKey,
                    new SoftReference<Map<Long, Geometry>>(pfafGeometries));
        }
        return pfafGeometries;
    }

    /**
     * Look for overlaps
     * 
     * @param pfaf
     * @param huc
     * @return
     */
    public boolean checkOverlap(long pfaf, String dataKey, String huc) {

        LinkedHashMap<Long, ?> map = getMap(dataKey, getPrimaryDomain()
                .getCwa(), huc);

        for (long lpfaf : map.keySet()) {
            if (lpfaf == pfaf) {
                return true;
            }
        }

        return false;
    }

    /**
     * Get the ares for a list of pfafs
     * 
     * @param pfafs
     * @return
     */
    public ArrayList<Double> getAreas(ArrayList<Long> pfafs) {
        ArrayList<Double> areas = new ArrayList<Double>(pfafs.size());
        for (Long pfaf : pfafs) {
            areas.add(getBasin(pfaf).getArea());
        }
        return areas;
    }

    /**
     * gets all up and down stream basins for a given pfaf
     * 
     * @param dataKey
     * @param pfaf
     * @return
     */
    public ArrayList<Long> getUpStreamBasins(String dataKey, Long pfaf) {

        ArrayList<Long> streamPfafs = new ArrayList<Long>();

        if (pfaf != null) {
            FFMPBasinMetaData fmbd = getBasin(pfaf);

            if (fmbd != null) {
                if (fmbd.getStreamPfafs() != null) {
                    for (Integer basinId : fmbd.getStreamPfafs()) {
                        Long basinPfaf = findPfafByBasinId(dataKey, basinId);
                        streamPfafs.add(basinPfaf);
                    }
                }
            }
        }

        // System.out.println("up streamPfafs : " + streamPfafs.toString());
        return streamPfafs;
    }

    /**
     * Gets the down stream trace
     * 
     * @param dataKey
     * @param pfaf
     * @return
     */
    public Long getDownStreamBasins(String dataKey, Long pfaf) {

        if (pfaf != null) {
            FFMPBasinMetaData basin = getBasin(pfaf);
            if (basin != null) {
                int basinId = getBasin(pfaf).getBasinId();

                for (DomainXML domain : domains) {
                    for (Entry<Long, ?> entry : getMap(dataKey,
                            domain.getCwa(), FFMPRecord.ALL).entrySet()) {
                        FFMPBasinMetaData downBasin = (FFMPBasinMetaData) entry
                                .getValue();

                        if (downBasin.getStreamPfafs() != null) {
                            if (downBasin.getStreamPfafs().contains(basinId)) {
                                return downBasin.getPfaf();
                            }
                        }
                    }
                }
            }
        }

        return null;
    }

    /**
     * Get the FIPS for the pfaf being looked at
     * 
     * @param dataKey
     * @param pfaf
     * @return
     */
    public long getCountyFipsByPfaf(long pfaf) {

        FFMPRunConfigurationManager.getInstance();
        ArrayList<FFMPRunXML> runners = FFMPRunConfigurationManager
                .getInstance().getFFMPRunners();
        for (FFMPRunXML runner : runners) {
            ArrayList<ProductRunXML> products = runner.getProducts();
            for (ProductRunXML product : products) {
                if (this.isSiteLoaded(product.getProductKey())) {
                    for (DomainXML domain : domains) {
                        for (Entry<Long, ?> entry : getMap(
                                product.getProductKey(), domain.getCwa(),
                                "COUNTY").entrySet()) {
                            @SuppressWarnings("unchecked")
                            ArrayList<Long> countyList = (ArrayList<Long>) entry
                                    .getValue();

                            if (countyList.contains(pfaf)) {
                                return entry.getKey();
                            }
                        }
                    }
                }
            }
        }

        return 0l;
    }

    /**
     * Gets a metadata basin contained within the domain listed.
     * 
     * @param dataKey
     * @param domainName
     * @param pfafs
     * @return
     */
    public FFMPBasinMetaData getBasinInDomains(String dataKey,
            List<DomainXML> domains, List<Long> pfafs) {

        FFMPBasinMetaData mbasin = null;

        for (DomainXML domain : domains) {

            LinkedHashMap<Long, ?> map = getMap(dataKey, domain.getCwa(),
                    FFMPRecord.ALL);

            for (Long key : pfafs) {
                if (map.containsKey(key)) {
                    mbasin = (FFMPBasinMetaData) map.get(key);
                    if (mbasin.getCwa().equals(domain.getCwa())
                            || mbasin.isPrimaryCwa()) {
                        return mbasin;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Check for site load or not
     * 
     * @param siteKey
     * @return
     */
    public boolean isSiteLoaded(String siteKey) {
        if (domainMap.containsKey(siteKey)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Work around for bad shape files
     * 
     * @param cwa
     * @param dataKey
     * @return
     */
    private ArrayList<Long> getCountyFips(String cwa, String dataKey) {

        ArrayList<String> resolutions = ScanUtils.getResolutionLevels("county");
        ArrayList<Long> list = null;

        for (String resolution : resolutions) {
            double res = Double.parseDouble(resolution.substring(9).replace(
                    '_', '.'));

            if (res >= 0.004) {

                list = FFMPUtils.getUniqueCountyFips(cwa, getMaxExtent(),
                        getSiteExtents(dataKey), mode.getMode(), resolution);

                if (list.size() > 0) {
                    break;
                }
            }
        }

        return list;
    }

    /**
     * Get the county info
     * 
     * @param siteKey
     * @param countyPfaf
     * @return
     */
    public String getCountyStateName(String siteKey, Long countyPfaf) {

        String rname = null;

        if (countyMap == null || countyMap.size() == 0) {
            getCounties(siteKey);
        }

        FFMPCounty county = FFMPUtils
                .getCounty(countyPfaf, MODE.CAVE.getMode());

        if (county != null) {
            StringBuffer name = new StringBuffer();
            name.append(county.getState() + ", ");
            name.append(county.getCountyName());
            rname = name.toString();
        }

        return rname;
    }

    /**
     * Causes a general recreation of template and template related files
     * (Aggregate Geometries, Source Bins)
     */
    public void dumpTemplates() {

        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile lfTemplateDir = pathManager.getLocalizationFile(lc,
                "ffmp/");

        if (lfTemplateDir != null) {
            File templateDirFile = lfTemplateDir.getFile();
            if (templateDirFile != null) {
                File[] files = templateDirFile.listFiles();
                if (files.length > 0) {
                    synchronized (files) {
                        for (File file : files) {
                            if (file.isDirectory()
                                    && file.listFiles().length > 0) {
                                for (File iFile : file.listFiles()) {
                                    iFile.delete();
                                }
                                statusHandler.handle(
                                        Priority.INFO,
                                        "Deleted Template directory..."
                                                + file.getName());
                            }
                        }

                        // write out the config XML so templates
                        // don't keep regening
                        ftcm.setRegenerate(false);
                        ftcm.saveConfigXml();
                        template = null;
                    }
                }
            }
        }
    }

    /**
     * DR 13228
     */
    public static boolean isCountyRow(String huc, String rowName) {

        return "COUNTY".equals(huc) && rowName.contains(",");// see
                                                             // getCountyStateName(,)

    }

    /**
     * DR 13228
     */
    public synchronized ArrayList<Long> getVgbLookupIdsByCounty(String dataKey,
            Long pfaf, String huc, String rowName) {

        String stateCommaCnty = rowName;

        HashMap<String, HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>>> virtualMap = vgbsInCounty
                .get(dataKey);

        for (DomainXML domain : domains) {

            HashMap<String, ArrayList<FFMPVirtualGageBasinMetaData>> map = virtualMap
                    .get(domain.getCwa());
            if (map != null) {
                ArrayList<FFMPVirtualGageBasinMetaData> list = map
                        .get(stateCommaCnty.trim().toUpperCase());

                if (list != null && !list.isEmpty()) {
                    ArrayList<Long> result = new ArrayList<Long>();
                    for (FFMPVirtualGageBasinMetaData md : list) {
                        result.add(md.getLookupId());

                    }
                    return result;
                }
            }
        }
        return new ArrayList<Long>();
    }

}
