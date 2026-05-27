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

import com.raytheon.uf.common.dataplugin.ffmp.templates.DataKeyCwaKey;
import com.raytheon.uf.common.dataplugin.ffmp.templates.FFMPTemplatesIO;
import com.raytheon.uf.common.dataplugin.ffmp.templates.TemplateData;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DataType;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;
import org.locationtech.jts.io.WKBReader;

/**
 * FFMPHucTemplate maker/factory
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jul 29, 2009  2152     D. Hladky  Initial release
 * Jul 09, 2010  3914     D. Hladky  Localization work
 * Dec 13, 2010  7484     D. Hladky  Service Backup
 * Feb 01, 2013  1569     D.Hladky   Constants
 * Mar 01, 2013  13228    G. Zhang   Add VGB county and related code
 * Feb 20, 2013  1635     D. Hladky  Constants
 * Mar 18, 2013  1817     D. Hladky  Fixed issue with BOX where only 1 HUC was
 *                                   showing up.
 * Apr 15, 2013  1902     M. Duff    Generic List
 * Jun 10, 2013  2085     njensen    Use countyMap for efficiency
 * Jul 01, 2013  2155     dhladky    Fixed duplicate pfafs that were in
 *                                   domainList arrays from overlapping domains.
 * Jul 15, 2013  2184     dhladky    Remove all HUC's for storage except ALL
 * Nov 18, 2014  3831     dhladky    StatusHandler logging. Proper list sizing.
 *                                   Geometry chunk sizing.
 * Aug 08, 2015  4722     dhladky    Improved Grid support.
 * Nov 12, 2015  4834     njensen    Changed LocalizationOpFailedException to
 *                                   LocalizationException
 * Feb 15, 2016  5244     nabowle    Replace deprecated LocalizationFile
 *                                   methods.
 * Apr 07, 2016  5491     tjensen    Fix NullPointerException from
 *                                   getRawGeometries
 * Aug 09, 2016  5819     mapeters   Template files moved from SITE to
 *                                   CONFIGURED
 * Apr 12, 2017  11250    lshi       VGBs inclusion for localization was
 *                                   incorrect
 * May 18, 2017  6266     nabowle    Removed mode from
 *                                   FFMPUtils.getRadarCenter(). Code cleanup.
 * Jun 21, 2018  6641     njensen    General cleanup, move Maps of data to new
 *                                   class TemplateData, move IO operations to
 *                                   new class FFMPTemplatesIO
 * Jul 10, 2018  6695     njensen    Updated for single FFMPRunXML
 * Jul 20, 2018  6642     randerso   Code cleanup.
 * Jul 15, 2019  7627     mroos      Added getMaxExtents method
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFMPTemplates {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPTemplates.class);

    /**
     * template mode
     */
    public enum MODE {
        /** CAVE mode */
        CAVE,

        /** EDEX mode */
        EDEX;
    }

    /** County Warning Area **/
    private DomainXML primaryCWA = null;

    /** Domains to load **/
    private List<DomainXML> domains = new ArrayList<>();

    private Map<DataKeyCwaKey, TemplateData> dataMap;

    /** Singleton instance of this class */
    private static FFMPTemplates template = null;

    /** Mode of operation **/
    private MODE mode = null;

    /** Start depth for HUC lookups **/
    private int hucDepthStart = 4;

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
    private static final GeometryFactory factory = new GeometryFactory();

    /** true when done */
    /*
     * TODO: having a public flag that is set outside the class seems really
     * hacky
     */
    public boolean done = false;

    private final Map<Long, FFMPCounty> countyMap = new HashMap<>();

    /**
     * Single constructor
     *
     * @param domain
     * @param siteKey
     * @param mode
     *
     * @return single template
     */
    public static synchronized FFMPTemplates getInstance(DomainXML domain,
            String siteKey, MODE mode) {
        if (template == null) {
            template = new FFMPTemplates(domain, mode);
            template.loadTemplate(siteKey, domain.getCwa());
        }
        return template;
    }

    /**
     * Multiple constructor loads multiple
     *
     * @param primaryCWA
     * @param mode
     *
     * @return multiple templates
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
     * @param primaryCWA
     * @param mode
     */
    public FFMPTemplates(DomainXML primaryCWA, MODE mode) {
        this.mode = mode;
        this.primaryCWA = primaryCWA;
        init();
    }

    /**
     * localization and such
     */
    private void init() {
        dataMap = new HashMap<>();
        ftcm = FFMPTemplateConfigurationManager.getInstance();
        frcm = FFMPRunConfigurationManager.getInstance();

        try {
            ftcm.readConfigXml();

            if (ftcm.isRegenerate() && (mode == MODE.EDEX)) {
                dumpTemplates();
            }

            this.hucDepthStart = ftcm.getHucDepth();
            this.totalHucLevels = ftcm.getNumberOfHuc();
            this.maxExtent = ftcm.getExtents();
            domains.add(primaryCWA);
        } catch (Exception e) {
            if (mode == MODE.EDEX) {
                statusHandler.error(
                        "No configuration file found, default settings applied",
                        e);

                /*
                 * we use 4 because it is the 90% solution as a start point for
                 * the analysis. Added check to make sure at least 2 HUC layers
                 * are created.
                 */
                int preliminarystart = 4;
                // first crack
                List<Integer> hucParams = FFMPUtils.getHucParameters(
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
                        statusHandler.error(
                                "Cannot create a good template. There are not enough unique HUC's to create more than 1 layer.");
                        return;
                    }
                }

                this.hucDepthStart = startDepth;
                this.totalHucLevels = numlevels;
                this.maxExtent = 20000.0;

                List<String> vgbs = new ArrayList<>();
                domains.add(primaryCWA);

                // create a new entry
                ftcm.setExcludedVGBs(vgbs);
                ftcm.setExtents(this.maxExtent);
                ftcm.setHucDepth(hucDepthStart);
                ftcm.setNumberOfHuc(getTotalHucLevels());
                ftcm.setVirtual(true);
                ftcm.setRegenerate(false);
                ftcm.saveConfigXml();
            } else {
                statusHandler.error(
                        "No configuration files can be created, cannot create templates",
                        e);
            }
        }
    }

    /**
     * @return the total HUC levels
     */
    public int getTotalHucLevels() {
        return totalHucLevels;
    }

    /**
     * Gets basin metadata
     *
     * @param dataKey
     * @param pfaf
     * @return the basin metadata
     */
    public FFMPBasinMetaData getBasin(String dataKey, Long pfaf) {
        FFMPBasinMetaData fmbd = null;
        for (DomainXML domain : domains) {
            Map<Long, ?> map = getMap(dataKey, domain.getCwa(), FFMPRecord.ALL);
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
     * @return the basin metadata
     */
    public FFMPBasinMetaData getBasin(Long pfaf) {
        FFMPBasinMetaData fmbd = null;

        if (runner == null) {
            runner = frcm.getRunner(primaryCWA.getCwa());
        }
        for (ProductRunXML product : runner.getProducts()) {
            if (isSiteLoaded(product.getProductKey())) {
                for (DomainXML domain : domains) {
                    Map<Long, ?> map = getMap(product.getProductKey(),
                            domain.getCwa(), FFMPRecord.ALL);
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
    private synchronized void loadTemplate(String dataKey, String cwa) {
        // synchronized this, don't want half loaded templates
        done = false;

        long time = System.currentTimeMillis();

        List<String> totalHucs = ftcm.getHucLevels();
        for (String huc : totalHucs) {
            statusHandler.handle(Priority.INFO,
                    "Starting site template process " + dataKey + ":" + cwa
                            + ":" + huc);
            // special handling for VGB's
            if (huc.equals(FFMPRecord.VIRTUAL)) {
                getVirtualGageBasins(dataKey, cwa);
            } else {
                getMap(dataKey, cwa, huc);
            }

            statusHandler.handle(Priority.INFO, "Finishing template process "
                    + dataKey + ": " + cwa + ":" + huc);
        }

        statusHandler.handle(Priority.INFO,
                "Template Load for " + dataKey + ": " + cwa + ": took "
                        + (System.currentTimeMillis() - time) + " ms");
    }

    /**
     * Finds the aggregated pfaf of a given pfaf
     *
     * @param pfaf
     * @param dataKey
     * @param huc
     * @return the aggregated pfaf
     */
    public Long findAggregatedPfaf(Long pfaf, String dataKey, String huc) {
        Long rpfaf = null;
        try {
            rpfaf = getAggregatedPfaf(pfaf, dataKey, huc);
        } catch (NullPointerException npe) {
            /*
             * FIXME Leave catch in place until we can figure out what is null
             * and correct the code
             */
            statusHandler.error("NPE for pfaf=" + pfaf + ", dataKey=" + dataKey
                    + ", huc=" + huc, npe);
        }
        return rpfaf;
    }

    /**
     * Finds the aggregated pfaf of a given VGB
     *
     * @param lid
     * @param dataKey
     * @param huc
     * @return the aggregated pfaf
     */
    public Long findAggregatedVGB(String lid, String dataKey, String huc) {
        Long rpfaf = null;
        try {
            FFMPVirtualGageBasinMetaData metadata = getVirtualGageBasinMetaData(
                    dataKey, lid);
            if (metadata != null) {
                rpfaf = findAggregatedPfaf(metadata.getParentPfaf(), dataKey,
                        huc);
            }
        } catch (NullPointerException npe) {
            /*
             * FIXME Leave catch in place until we can figure out what is null
             * and correct the code
             */
            statusHandler.error("NPE for lid=" + lid + ", dataKey=" + dataKey
                    + ", huc=" + huc, npe);
        }
        return rpfaf;
    }

    /**
     * Get the aggregate pfaf ids
     *
     * @param pfaf
     * @param dataKey
     * @param huc
     * @param domainList
     * @return the aggregate pfafs
     */
    public List<Long> getAggregatePfafs(Object pfaf, String dataKey, String huc,
            List<DomainXML> domainList) {
        List<Long> list = new ArrayList<>();
        for (DomainXML domain : domainList) {
            List<Long> pfafList = getAggregatePfafsByDomain(pfaf, dataKey,
                    domain.getCwa(), huc);
            // Sometimes the domains have overlaps in basins.
            // You can't blindly add the domain list to the main list.
            // You have to check if it already exists in the list.
            if (pfafList != null) {
                for (Long lpfaf : pfafList) {
                    if (!list.contains(lpfaf)) {
                        list.add(lpfaf);
                    }
                }
            }
        }
        return list;
    }

    /**
     * Gets the aggregate mappings for all domains
     *
     * @param pfaf
     * @param dataKey
     * @param huc
     * @return the aggregate pfafs
     */
    public List<Long> getAggregatePfafs(Object pfaf, String dataKey,
            String huc) {
        List<Long> list = new ArrayList<>();
        for (DomainXML domain : domains) {
            List<Long> domainList = getAggregatePfafsByDomain(pfaf, dataKey,
                    domain.getCwa(), huc);
            // Sometimes the domains have overlaps in basins.
            // You can't blindly add the domain list to the main list.
            // You have to check if it already exists in the list.
            if (domainList != null) {
                for (Long lpfaf : domainList) {
                    if (!list.contains(lpfaf)) {
                        list.add(lpfaf);
                    }
                }
            }
        }
        return list;
    }

    /**
     * Gets the aggregate mappings for all domains, used by the FFFG dialog If
     * you want ALL and I mean ALL of the basins in a given coverage
     *
     * @param pfaf
     * @param huc
     * @return the aggregate pfafs
     */

    public List<Long> getAllAggregatePfafs(Object pfaf, String huc) {
        List<Long> list = new ArrayList<>();
        Set<Long> domainSet = new HashSet<>();
        if (runner == null) {
            runner = frcm.getRunner(primaryCWA.getCwa());
        }
        for (ProductRunXML product : runner.getProducts()) {
            if (isSiteLoaded(product.getProductKey())) {
                for (DomainXML domain : domains) {
                    List<Long> domainList = getAggregatePfafsByDomain(pfaf,
                            product.getProductKey(), domain.getCwa(), huc);
                    // Sometimes the domains have overlaps in basins.
                    // You can't blindly add the domain list to the main list.
                    // You have to check if it already exists in the list.
                    if (domainList != null) {
                        for (Long lpfaf : domainList) {
                            if (!list.contains(lpfaf)) {
                                list.add(lpfaf);
                            }
                        }
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
     * Gets the aggregate mappings by domain
     *
     * @param pfaf
     * @param dataKey
     * @param domain
     * @param huc
     * @return the aggregate pfafs
     */
    @SuppressWarnings("unchecked")
    public List<Long> getAggregatePfafsByDomain(Object pfaf, String dataKey,
            String domain, String huc) {

        List<Long> list = new ArrayList<>();

        Map<Long, List<Long>> map = (LinkedHashMap<Long, List<Long>>) getMap(
                dataKey, domain, huc);
        if (map == null) {
            return list;
        }
        try {
            // Virtual Gage Basin centered
            if (pfaf instanceof String) {
                FFMPVirtualGageBasinMetaData metadata = getVirtualGageBasinMetaData(
                        dataKey, (String) pfaf);
                if (metadata != null) {
                    Long pfafl = findAggregatedPfaf(metadata.getParentPfaf(),
                            dataKey, huc);
                    list = map.get(pfafl);
                }
            } else {
                Object object = map.get(pfaf);
                if (object instanceof FFMPBasinMetaData) {
                    list.add(((FFMPBasinMetaData) object).getPfaf());
                } else {
                    list = (List<Long>) object;
                }
            }
        } catch (NullPointerException npe) {
            /*
             * FIXME Leave catch in place until we can figure out what is null
             * and correct the code
             */
            statusHandler.error("NPE for pfaf=" + pfaf + ", dataKey=" + dataKey
                    + ", domain=" + domain + ", huc=" + huc, npe);
        }

        return list;
    }

    /**
     * Get the basin by Lat/Lon
     *
     * @param dataKey
     * @param coor
     * @return the basin metadata
     */
    public FFMPBasinMetaData findBasinByLatLon(String dataKey,
            Coordinate coor) {
        Point point = factory.createPoint(coor);
        HucLevelGeometriesFactory geomFactory = HucLevelGeometriesFactory
                .getInstance();
        try {
            String huc = "HUC0";
            for (DomainXML domain : getDomains()) {
                String cwa = domain.getCwa();
                Map<Long, Envelope> envMap = geomFactory.getEnvelopes(this,
                        dataKey, cwa, huc);

                @SuppressWarnings("unchecked")
                Map<Long, List<Long>> aggrMap = (Map<Long, List<Long>>) getMap(
                        dataKey, cwa, huc);
                Map<Long, Geometry> geomMap = geomFactory.getGeometries(this,
                        dataKey, cwa, FFMPRecord.ALL);
                for (Entry<Long, List<Long>> entry : aggrMap.entrySet()) {
                    Envelope env = envMap.get(entry.getKey());
                    if ((env != null) && env.contains(coor)) {
                        // in the general envelope, check individual basins
                        for (Long pfaf : entry.getValue()) {
                            if (geomMap.get(pfaf).contains(point)) {
                                return (FFMPBasinMetaData) getMap(dataKey, cwa,
                                        FFMPRecord.ALL).get(pfaf);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error("Find Basin by lon lat failed: dataKey: "
                    + dataKey + " coor:" + coor.toString(), e);
        }

        return null;
    }

    /**
     * Gets the pfaf by basinId
     *
     * @param dataKey
     * @param basinId
     * @return
     */
    private Long findPfafByBasinId(String dataKey, int basinId) {
        // TODO: make reverse lookup...
        FFMPBasinMetaData basin = null;
        for (DomainXML domain : domains) {
            Map<Long, ?> map = getMap(dataKey, domain.getCwa(), FFMPRecord.ALL);
            for (Object mapVal : map.values()) {
                basin = ((FFMPBasinMetaData) mapVal);
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
     * @param pfaf
     * @param dataKey
     * @param huc
     * @return the center coordinate
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
                String msg = "Error getting envelope: dataKey=" + dataKey
                        + ", cwa=" + cwa + ", huc=" + huc + ", pfaf=" + pfaf;
                statusHandler.handle(Priority.PROBLEM, msg, e);
            }

            if (env == null) {
                env = tmp;
            } else if (tmp != null) {
                env.expandToInclude(tmp);
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
        try {
            IPathManager pathManager = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile listf = pathManager.getLocalizationFile(lc,
                    FFMPTemplatesIO.getAbsoluteFileName(dataKey, huc, cwa,
                            "list"));
            ILocalizationFile mapf = pathManager.getLocalizationFile(lc,
                    FFMPTemplatesIO.getAbsoluteFileName(dataKey, huc, cwa,
                            "map"));

            good = listf.exists() && mapf.exists();
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error determining if list and map files exist for dataKey="
                                    + dataKey + ", huc=" + huc + ", cwa=" + cwa,
                            e);
        }
        return good;
    }

    /**
     * Get a listing of the counties in the FFMP monitored area
     *
     * @param dataKey
     * @return the counties listing
     */
    public FFMPCounties getCounties(String dataKey) {
        List<FFMPCounty> countyList = new ArrayList<>();
        FFMPCounty county;

        try {

            for (DomainXML domain : domains) {
                for (Long key : getMap(dataKey, domain.getCwa(),
                        FFMPRecord.COUNTY).keySet()) {
                    if (countyMap.get(key) == null) {
                        county = FFMPUtils.getCounty(key);
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
            statusHandler.error("Failed to lookup County: dataKey: " + dataKey,
                    e);
        }

        FFMPCounties counties = new FFMPCounties(countyList);

        return counties;
    }

    /**
     * Finds the parent aggregated pfaf of a given pfaf.
     *
     * @param key
     * @param dataKey
     * @param huc
     * @return the aggregated parent pfaf
     */
    public Long getAggregatedPfaf(Long key, String dataKey, String huc) {
        if (huc.equals(FFMPRecord.ALL)) {
            return key;
        } else if (huc.equals(FFMPRecord.COUNTY)) {
            // TODO: use envelope contains to limit search area?
            for (DomainXML domain : domains) {
                Map<Long, ?> map = getMap(dataKey, domain.getCwa(), huc);
                for (Entry<Long, ?> entry : map.entrySet()) {

                    @SuppressWarnings("unchecked")
                    List<Long> list = (List<Long>) entry.getValue();
                    if (list.contains(key)) {
                        return entry.getKey();
                    }
                }
            }
        } else {
            int hucNum = Integer.parseInt(huc.substring(3));
            int endIndex = hucDepthStart + hucNum;
            return Long.parseLong(key.toString().substring(0, endIndex));
        }
        return null;
    }

    /**
     * Find the extents of the collective sites
     *
     * @param dataKey
     * @return
     */
    private String getSiteExtents(String dataKey) throws Exception {
        String siteExtents = null;
        // figure out which product to apply this to
        if (runner == null) {
            runner = frcm.getRunner(primaryCWA.getCwa());
        }

        ProductRunXML product = runner.getProduct(dataKey);
        FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager
                .getInstance();
        SourceXML primeSource = fscm.getSource(product.getProductName());

        DataType dataType = primeSource.getDataType();
        switch (dataType) {
        case XMRG:
            Rectangle rect = null;

            rect = HRAPCoordinates.getHRAPCoordinates();
            rect.setBounds(rect.x * primeSource.getHrapGridFactor(),
                    rect.y * primeSource.getHrapGridFactor(),
                    rect.width * primeSource.getHrapGridFactor(),
                    rect.height * primeSource.getHrapGridFactor());

            HRAPSubGrid hrapgrid = new HRAPSubGrid(rect,
                    primeSource.getHrapGridFactor());
            Geometry geo = hrapgrid.getGeometry();
            siteExtents = FFMPUtils.getGeometryText(geo);
            break;
        case RADAR:
            Coordinate siteCoor = FFMPUtils.getRadarCenter(dataKey);
            Polygon poly = FFMPUtils.getRadarPolygon(siteCoor,
                    120.0 * ScanUtils.NMI_TO_KM * 1000.0);
            siteExtents = FFMPUtils.getRadarPolygonText(poly);
            break;
        case GRID:
            // extract the Grid Coverage for use in site extents creation
            GridCoverage coverage = FFMPUtils
                    .getGridCoverageRecord(primeSource.getDataPath());
            if (coverage == null) {
                throw new FFMPException("Unable to get grid coverage for "
                        + primeSource.getDataPath());
            }
            siteExtents = FFMPUtils.getGeometryText(coverage.getGeometry());
            break;
        case PDO:
            statusHandler.handle(Priority.PROBLEM,
                    "PDO Not yet implemented:  " + dataKey);
            break;
        default:
            statusHandler.handle(Priority.PROBLEM,
                    "No method of extracting siteExtents for this type:  "
                            + dataKey);
            break;
        }

        return siteExtents;
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
                List<Long> list = null;
                Map<Long, Set<Long>> aggrPfafToAllChildPfafsMap = null;

                if (huc.equals(FFMPRecord.ALL)) {
                    try {
                        map = loadBasins(dataKey, cwa, FFMPUtils.getBasins(cwa,
                                this.maxExtent, getSiteExtents(dataKey)));
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to create FFMP Template for this dataKey: "
                                        + dataKey,
                                e);
                    }
                } else if (huc.equals(FFMPRecord.COUNTY)) {
                    list = getCountyFips(dataKey, cwa);
                } else {
                    int myHucNum = Integer.parseInt(huc.substring(3));
                    TreeSet<Long> aggrPfafs = new TreeSet<>();
                    aggrPfafToAllChildPfafsMap = new HashMap<>();
                    if (myHucNum + 1 == getTotalHucLevels()) {
                        Set<Long> allPfafs = getMap(dataKey, cwa,
                                FFMPRecord.ALL).keySet();
                        for (Long pfaf : allPfafs) {
                            int endIndex = hucDepthStart + myHucNum;
                            Long aggrPfaf = Long.parseLong(
                                    pfaf.toString().substring(0, endIndex));
                            aggrPfafs.add(aggrPfaf);
                            Set<Long> allChildPfafs = aggrPfafToAllChildPfafsMap
                                    .get(aggrPfaf);
                            if (allChildPfafs == null) {
                                allChildPfafs = new TreeSet<>();
                                aggrPfafToAllChildPfafsMap.put(aggrPfaf,
                                        allChildPfafs);
                            }
                            allChildPfafs.add(pfaf);
                        }
                    } else {
                        String childHuc = "HUC" + (myHucNum + 1);
                        Map<Long, List<Long>> childHucMap = (Map<Long, List<Long>>) getMap(
                                dataKey, cwa, childHuc);
                        for (Entry<Long, List<Long>> entry : childHucMap
                                .entrySet()) {
                            Long aggrPfaf = new Long(
                                    entry.getKey().longValue() / 10);
                            aggrPfafs.add(aggrPfaf);
                            Set<Long> childAggrPfafs = aggrPfafToAllChildPfafsMap
                                    .get(aggrPfaf);
                            if (childAggrPfafs == null) {
                                childAggrPfafs = new TreeSet<>();
                                aggrPfafToAllChildPfafsMap.put(aggrPfaf,
                                        childAggrPfafs);
                            }
                            childAggrPfafs.addAll(entry.getValue());
                        }
                    }
                    list = new ArrayList<>(aggrPfafs);
                }

                if (!huc.equals(FFMPRecord.ALL) && list != null) {
                    map = new LinkedHashMap<>();
                    Map<Long, Geometry> rawGeometries = null;
                    LinkedHashMap<Long, FFMPBasinMetaData> allMap = (LinkedHashMap<Long, FFMPBasinMetaData>) getMap(
                            dataKey, cwa, FFMPRecord.ALL);

                    for (Long key : list) {
                        List<Long> innerList = null;

                        if (huc.equals(FFMPRecord.COUNTY)) {
                            innerList = new ArrayList<>();
                            FFMPCounty countyInfo = FFMPUtils
                                    .getCountyInfo(key);

                            PreparedGeometry countyGeometry = PreparedGeometryFactory
                                    .prepare(countyInfo.getGeometry());
                            String countyName = countyInfo.getCountyName();
                            String state = countyInfo.getState();
                            statusHandler.handle(Priority.INFO,
                                    "Processing --- County: " + countyName
                                            + " State: " + state);
                            boolean primary = false;
                            if (cwa.equals(primaryCWA.getCwa())) {
                                primary = true;
                            }

                            if (rawGeometries == null
                                    || rawGeometries.isEmpty()) {
                                rawGeometries = getRawGeometries(dataKey, cwa);
                            }

                            for (Entry<Long, FFMPBasinMetaData> entry : allMap
                                    .entrySet()) {

                                if (countyGeometry.contains(rawGeometries
                                        .get(entry.getKey()).getCentroid())) {
                                    FFMPBasinMetaData basin = entry.getValue();
                                    basin.setCounty(countyName);
                                    basin.setState(state);
                                    basin.setPrimaryCwa(primary);
                                    innerList.add(entry.getKey());
                                }
                            }

                        } else {
                            innerList = new ArrayList<>(
                                    aggrPfafToAllChildPfafsMap.get(key));
                        }

                        statusHandler.handle(Priority.DEBUG, "HUC: " + huc
                                + " INNERLIST SIZE:  " + innerList.size());

                        if (!innerList.isEmpty()) {
                            ((Map<Long, List<Long>>) map).put(key, innerList);
                        }
                    }

                    /*
                     * trigger the write of the "ALL" now that the counties are
                     * set.
                     */
                    if (huc.equals(FFMPRecord.COUNTY)) {
                        if (allMap != null) {
                            FFMPTemplatesIO.writeTemplateFile(dataKey,
                                    FFMPRecord.ALL, cwa, allMap);
                        }
                    }
                }

                aggrPfafToAllChildPfafsMap = null;
                list = null;

                // clean up
                System.runFinalization();
                System.gc();

                if (!huc.equals(FFMPRecord.ALL) && map != null) {
                    FFMPTemplatesIO.writeTemplateFile(dataKey, huc, cwa, map);
                }
            }
        } else {
            map = FFMPTemplatesIO.readTemplateFile(dataKey, huc, cwa);
        }
        return map;
    }

    /**
     * load up the maps
     *
     * @param dataKey
     * @param cwa
     * @param huc
     * @return the maps
     */
    public synchronized LinkedHashMap<Long, ?> getMap(String dataKey,
            String cwa, String huc) {
        DataKeyCwaKey key = new DataKeyCwaKey(dataKey, cwa);
        TemplateData data = dataMap.get(key);
        if (data == null) {
            data = new TemplateData();
            dataMap.put(key, data);
        }
        LinkedHashMap<Long, ?> map = data.getHucPfafMap(huc);
        if (map == null) {
            map = readMap(dataKey, cwa, huc);
            if (map == null) {
                /*
                 * This empty map is used to prevent NullPointerExceptions. If
                 * this method should return null, then you will need to add
                 * null checks to all calls to this method.
                 */
                map = new LinkedHashMap<>();
                statusHandler.error("Error loading template for dataKey: "
                        + dataKey + ", cwa: " + cwa + ", huc: " + huc
                        + ". Processing of dataKey " + dataKey
                        + " will be skipped.");
            }
            data.putInHucPfafMap(huc, map);
        }
        return map;
    }

    /**
     * Find the list of pfafs for this HUC level
     *
     * @param siteKey
     * @param huc
     * @param domains
     * @return the pfafs
     */
    public synchronized List<Long> getHucKeyList(String siteKey, String huc,
            List<DomainXML> domains) {

        Set<Long> keys = new HashSet<>();

        for (DomainXML domain : domains) {
            Map<Long, ?> map = getMap(siteKey, domain.getCwa(), huc);
            keys.addAll(map.keySet());
        }

        return new ArrayList<>(keys);
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
                virtuals = FFMPUtils.getVirtualGageBasins(cwa);
                Map<Long, Geometry> rawGeometries = getRawGeometries(dataKey,
                        cwa);

                // assign pfafs
                for (FFMPVirtualGageBasinMetaData vb : virtuals.values()) {
                    Point vgbPoint = factory.createPoint(vb.getCoordinate());
                    /*
                     * Expensive..., use envelopes first to get rough idea and
                     * skip unnecessary checks
                     */
                    Map<Long, ?> map = getMap(dataKey, cwa, FFMPRecord.ALL);
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
                }

                /*
                 * DR 11250, the filtering code is moved to
                 * FFMPRowGenerator.run() to avoid breaking the indexing of vgb
                 * and values arrays.
                 */

                // write them out
                FFMPTemplatesIO.writeVGBFile(virtuals, dataKey, cwa);
            }
        } else {
            virtuals = FFMPTemplatesIO.readVGBFile(dataKey, cwa);
        }

        return virtuals;
    }

    /**
     * Generate the Virtual Gage Basins Meta Data for primary domain
     *
     * @param dataKey
     * @param cwa
     * @return the VGB metadata
     */
    public synchronized LinkedHashMap<String, FFMPVirtualGageBasinMetaData> getVirtualGageBasins(
            String dataKey, String cwa) {
        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map = null;
        DataKeyCwaKey key = new DataKeyCwaKey(dataKey, cwa);
        TemplateData data = dataMap.get(key);
        map = data.getLidToVGBMap();
        if (map == null) {
            map = readVirtualGageBasins(dataKey, cwa);
            if (map == null) {
                return map;
            }

            data.setLidToVGBMap(map);

            Map<String, List<FFMPVirtualGageBasinMetaData>> vgbMap = new HashMap<>();

            Map<Long, List<FFMPVirtualGageBasinMetaData>> virtualGageBasins = new HashMap<>(
                    (int) (map.size() * 1.3));

            for (FFMPVirtualGageBasinMetaData vgb : map.values()) {
                Long id = vgb.getParentPfaf();
                // see getCountyStateName()
                String stateCommaCnty = vgb.getState() + ", " + vgb.getCounty();

                List<FFMPVirtualGageBasinMetaData> list = virtualGageBasins
                        .get(id);
                List<FFMPVirtualGageBasinMetaData> list2 = vgbMap
                        .get(stateCommaCnty.toUpperCase());

                if (list == null) {
                    list = new ArrayList<>();
                    virtualGageBasins.put(id, list);
                }
                list.add(vgb);

                if (list2 == null) {
                    list2 = new ArrayList<>();
                    vgbMap.put(stateCommaCnty.toUpperCase(), list2);
                }
                list2.add(vgb);
            }

            data.setVgbsInParentPfaf(virtualGageBasins);
            data.setVgbsInCounty(vgbMap);
        }
        return map;
    }

    /**
     * Gets the Virtual Gage Basin MetaData
     *
     * @param dataKey
     * @param lid
     * @return the VGB metadata
     */
    public FFMPVirtualGageBasinMetaData getVirtualGageBasinMetaData(
            String dataKey, String lid) {
        FFMPVirtualGageBasinMetaData vgbmd = null;
        for (DomainXML domain : domains) {
            Map<String, FFMPVirtualGageBasinMetaData> map = getVirtualGageBasins(
                    dataKey, domain.getCwa());
            if (map != null && map.containsKey(lid)) {
                vgbmd = map.get(lid);
                break;
            }
        }

        return vgbmd;
    }

    /**
     * Gets the Virtual Gage Basin MetaData
     *
     * @param dataKey
     * @param cwa
     * @param parentPfaf
     * @return the VGB metadata
     */
    public synchronized List<FFMPVirtualGageBasinMetaData> getVirtualGageBasinMetaData(
            String dataKey, String cwa, Long parentPfaf) {
        if (cwa == null) {
            for (DomainXML domain : domains) {
                List<FFMPVirtualGageBasinMetaData> result = getVirtualGageBasinMetaData(
                        dataKey, domain.getCwa(), parentPfaf);
                if (result != null) {
                    return result;
                }
            }
            return null;
        }

        DataKeyCwaKey key = new DataKeyCwaKey(dataKey, cwa);
        TemplateData data = dataMap.get(key);
        return data.getVgbsInParentPfaf(parentPfaf);
    }

    /**
     * Check for VGBs in aggregate pfaf
     *
     * @param pfaf
     * @param dataKey
     * @param huc
     * @return true if VGBs in aggregate pfaf
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
    private synchronized boolean checkVirtualGageBasinMetaData(String dataKey,
            Long pfaf) {
        for (DomainXML domain : domains) {
            DataKeyCwaKey key = new DataKeyCwaKey(dataKey, domain.getCwa());
            TemplateData data = dataMap.get(key);
            if (data == null) {
                data = new TemplateData();
                dataMap.put(key, data);
            }
            if (data.getVgbsInParentPfaf(pfaf) != null
                    && !data.getVgbsInParentPfaf(pfaf).isEmpty()) {
                return true;
            }
        }

        return false;
    }

    /**
     * Gets the Virtual Gage Basin IDs
     *
     * @param dataKey
     * @param pfaf
     * @param huc
     * @param rowName
     * @return the VGB IDs
     */
    public synchronized List<Long> getVirtualGageBasinLookupIds(String dataKey,
            Long pfaf, String huc, String rowName) {
        if (isCountyRow(huc, rowName)) {
            return getVgbLookupIdsByCounty(dataKey, rowName);
        }

        List<Long> result = new ArrayList<>();
        for (DomainXML domain : domains) {
            DataKeyCwaKey key = new DataKeyCwaKey(dataKey, domain.getCwa());
            TemplateData data = dataMap.get(key);
            if (data == null) {
                data = new TemplateData();
                dataMap.put(key, data);
            }
            List<FFMPVirtualGageBasinMetaData> list = data
                    .getVgbsInParentPfaf(pfaf);

            if (list != null && !list.isEmpty()) {
                for (FFMPVirtualGageBasinMetaData md : list) {
                    if (!result.contains(md.getLookupId())) {
                        result.add(md.getLookupId());
                    }
                }
            }
        }

        return result;
    }

    /**
     * Writes out all basins from DB by CWA
     *
     * @param results
     */
    private LinkedHashMap<Long, FFMPBasinMetaData> loadBasins(String siteKey,
            String cwa, Object[] results) {
        LinkedHashMap<Long, FFMPBasinMetaData> basins = new LinkedHashMap<>();
        WKBReader reader = new WKBReader();
        FFMPBasinMetaData basin = null;
        int upstreamDepth = 0;
        DataKeyCwaKey compositeKey = new DataKeyCwaKey(siteKey, cwa);
        SoftReference<Map<Long, Geometry>> rawGeomRef = dataMap
                .get(compositeKey).getCwaRawGeometries();
        Map<Long, Geometry> pfafGeometries = null;

        if (rawGeomRef != null) {
            pfafGeometries = rawGeomRef.get();
        }

        if (results != null && results.length > 0) {

            if (pfafGeometries == null) {
                pfafGeometries = new HashMap<>(results.length, 1.0f);
                dataMap.get(compositeKey).setCwaRawGeometries(
                        new SoftReference<>(pfafGeometries));
            }

            for (int i = 0; i < results.length; i++) {
                Object[] row = (Object[]) results[i];
                basin = FFMPUtils.getMetaDataBasin(row);

                if (upstreamDepth == 0) {
                    upstreamDepth = basin.getStreamPfafs().size();
                }

                basin.removeZeros();
                basins.put(basin.getPfaf(), basin);

                if ((row.length >= (upstreamDepth + 9))
                        && (row[upstreamDepth + 9] != null)) {
                    try {
                        pfafGeometries.put(basin.getPfaf(),
                                reader.read((byte[]) row[upstreamDepth + 9])
                                        .buffer(0));

                    } catch (Exception e) {
                        statusHandler
                                .error("Failure to add rawGeometry in loadBasins: "
                                        + siteKey, e);
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
     * Gets the domains in this template
     *
     * @return the domains
     */
    public synchronized List<DomainXML> getDomains() {
        return domains;
    }

    /**
     * Add domains to the templates, This method is used by EDEX to add domains
     *
     * @param domain
     */
    public synchronized void addDomain(DomainXML domain) {
        if (runner == null) {
            runner = frcm.getRunner(primaryCWA.getCwa());
        }
        // create template for all of the site/domain combos
        // eliminate duplicates

        if (runner != null) {

            List<String> products = new ArrayList<>();
            for (ProductRunXML product : runner.getProducts()) {
                if (!products.contains(product.getProductKey())) {
                    products.add(product.getProductKey());
                }
            }

            for (String dataKey : products) {
                loadTemplate(dataKey, domain.getCwa());
            }

            if (!domains.contains(domain)) {
                domains.add(domain);
            }
        } else {
            statusHandler.handle(Priority.PROBLEM, "The Domain: "
                    + domain.getCwa()
                    + " your runner is null (not truly a primary domain), check your FFMPRunConfig.xml");
        }

        done = true;
    }

    /**
     * Add domains to the templates, This method is used by EDEX to add domains
     *
     * @param dataKey
     * @param domain
     */
    public synchronized void addDomain(String dataKey, DomainXML domain) {
        loadTemplate(dataKey, domain.getCwa());

        if (!domains.contains(domain)) {
            domains.add(domain);
        }
        done = true;
    }

    /**
     * Verify the unified geometries for a particular huc/domain combination
     *
     * @param huc
     * @param domain
     */
    public void verifyUnifiedGeometries(String huc, String domain) {
        HucLevelGeometriesFactory hucGeomFactory = HucLevelGeometriesFactory
                .getInstance();

        if (runner == null) {
            runner = frcm.getRunner(primaryCWA.getCwa());
        }

        for (ProductRunXML product : runner.getProducts()) {
            if (isSiteLoaded(product.getProductKey())) {
                long t1 = System.currentTimeMillis();
                statusHandler.handle(Priority.INFO,
                        "Starting geometry unify process "
                                + product.getProductKey() + ": " + domain + ": "
                                + huc);
                try {
                    hucGeomFactory.getGeometries(this, product.getProductKey(),
                            domain, huc);
                    hucGeomFactory.getEnvelopes(this, product.getProductKey(),
                            domain, huc);
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Failed persisting unified huc envelopes for huc: "
                                    + product.getProductKey() + ": " + huc
                                    + ": " + domain,
                            e);
                }

                hucGeomFactory.clear();

                long t2 = System.currentTimeMillis();
                statusHandler.handle(Priority.INFO,
                        "Finished geometry unify process "
                                + product.getProductKey() + ": " + domain + ": "
                                + huc + " in [" + (t2 - t1) + "] ms");
            }
        }
    }

    /**
     * Gets the raw basin geometries
     *
     * @param dataKey
     * @param cwa
     * @return the raw basin geometries
     */
    public Map<Long, Geometry> getRawGeometries(String dataKey, String cwa) {
        DataKeyCwaKey compositeKey = new DataKeyCwaKey(dataKey, cwa);
        SoftReference<Map<Long, Geometry>> rawGeomRef = dataMap
                .get(compositeKey).getCwaRawGeometries();
        Map<Long, Geometry> pfafGeometries = null;
        if (rawGeomRef != null) {
            pfafGeometries = rawGeomRef.get();
        }
        if (pfafGeometries == null || pfafGeometries.isEmpty()) {
            // TODO: add sync locking per cwa
            long t0 = System.currentTimeMillis();
            pfafGeometries = FFMPUtils.getRawGeometries(
                    getMap(dataKey, cwa, FFMPRecord.ALL).keySet());
            long t1 = System.currentTimeMillis();
            statusHandler.handle(Priority.INFO,
                    "Retrieval of raw geometries for site " + dataKey + " cwa "
                            + cwa + " took " + (t1 - t0) + " ms.");
            dataMap.get(compositeKey)
                    .setCwaRawGeometries(new SoftReference<>(pfafGeometries));
        }
        return pfafGeometries;
    }

    /**
     * Gets all up and down stream basins for a given pfaf
     *
     * @param dataKey
     * @param pfaf
     * @return the up/down stream basins
     */
    public List<Long> getUpStreamBasins(String dataKey, Long pfaf) {
        List<Long> streamPfafs = new ArrayList<>();

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

        return streamPfafs;
    }

    /**
     * Gets the downstream trace
     *
     * @param dataKey
     * @param pfaf
     * @return the pfaf ID of the downstream basin
     */
    public Long getDownStreamBasins(String dataKey, Long pfaf) {
        if (pfaf != null) {
            FFMPBasinMetaData basin = getBasin(pfaf);
            if (basin != null) {
                int basinId = getBasin(pfaf).getBasinId();

                for (DomainXML domain : domains) {
                    for (Entry<Long, ?> entry : getMap(dataKey, domain.getCwa(),
                            FFMPRecord.ALL).entrySet()) {
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
     * @param pfaf
     * @return the county FIPS code
     */
    public long getCountyFipsByPfaf(long pfaf) {
        FFMPRunConfigurationManager.getInstance();
        FFMPRunXML runner = FFMPRunConfigurationManager.getInstance()
                .getFFMPRunner();
        List<ProductRunXML> products = runner.getProducts();
        for (ProductRunXML product : products) {
            if (this.isSiteLoaded(product.getProductKey())) {
                for (DomainXML domain : domains) {
                    for (Entry<Long, ?> entry : getMap(product.getProductKey(),
                            domain.getCwa(), "COUNTY").entrySet()) {
                        @SuppressWarnings("unchecked")
                        List<Long> countyList = (List<Long>) entry.getValue();

                        if (countyList.contains(pfaf)) {
                            return entry.getKey();
                        }
                    }
                }
            }
        }

        return 0L;
    }

    /**
     * Gets the metadata for the basins contained within the domain listed.
     *
     * @param dataKey
     * @param domains
     * @param pfafs
     * @return the basin metadata
     */
    public FFMPBasinMetaData getBasinInDomains(String dataKey,
            List<DomainXML> domains, List<Long> pfafs) {
        FFMPBasinMetaData mbasin = null;

        for (DomainXML domain : domains) {

            Map<Long, ?> map = getMap(dataKey, domain.getCwa(), FFMPRecord.ALL);

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
     * @return true if site is loaded
     */
    public boolean isSiteLoaded(String siteKey) {
        for (DataKeyCwaKey key : dataMap.keySet()) {
            if (key.getDataKey().equals(siteKey)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Work around for bad shape files
     *
     * @param dataKey
     * @param cwa
     * @return the county FIPS codes for a cwa
     */
    private List<Long> getCountyFips(String dataKey, String cwa) {
        List<String> resolutions = ScanUtils.getResolutionLevels("county");
        List<Long> list = null;

        for (String resolution : resolutions) {
            double res = Double
                    .parseDouble(resolution.substring(9).replace('_', '.'));

            if (res >= 0.004) {
                try {
                    list = FFMPUtils.getUniqueCountyFips(cwa, this.maxExtent,
                            getSiteExtents(dataKey), resolution);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to create FFMP Template for this dataKey: "
                                    + dataKey,
                            e);
                }

                if (list != null && !list.isEmpty()) {
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
     * @return string containing "state, countyname"
     */
    public String getCountyStateName(String siteKey, Long countyPfaf) {
        String rname = null;

        if (countyMap == null || countyMap.size() == 0) {
            getCounties(siteKey);
        }

        FFMPCounty county = countyMap.get(countyPfaf);
        if (county == null) {
            county = FFMPUtils.getCounty(countyPfaf);
            countyMap.put(countyPfaf, county);
        }

        if (county != null) {
            StringBuilder name = new StringBuilder();
            name.append(county.getState() + ", ");
            name.append(county.getCountyName());
            rname = name.toString();
        }

        return rname;
    }

    /**
     * Causes a general recreation of template and template-related binary files
     * (Aggregate Geometries, Source Bins, FFTI files)
     */
    public void dumpTemplates() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationContext configCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        LocalizationContext[] contexts = new LocalizationContext[] { siteCtx,
                configCtx };
        ILocalizationFile[] lfs = pathManager.listFiles(contexts, "ffmp",
                new String[] { ".bin" }, true, true);

        if (lfs != null) {
            for (ILocalizationFile lf : lfs) {
                try {
                    lf.delete();
                } catch (LocalizationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error deleting " + lf.getPath(), e);
                }
            }
        }

        /*
         * write out the config XML so templates don't keep regenerating
         */
        ftcm.setRegenerate(false);
        ftcm.saveConfigXml();
        synchronized (this) {
            template = null;
        }
    }

    /**
     * @see #getCountyStateName()
     */
    private static boolean isCountyRow(String huc, String rowName) {
        return "COUNTY".equals(huc) && rowName.contains(",");
    }

    private synchronized List<Long> getVgbLookupIdsByCounty(String dataKey,
            String rowName) {
        String stateCommaCnty = rowName;
        List<Long> result = new ArrayList<>();

        for (DomainXML domain : domains) {
            DataKeyCwaKey key = new DataKeyCwaKey(dataKey, domain.getCwa());
            TemplateData data = dataMap.get(key);
            if (data == null) {
                data = new TemplateData();
                dataMap.put(key, data);
            }
            List<FFMPVirtualGageBasinMetaData> list = data
                    .getVgbsInCounty(stateCommaCnty.trim().toUpperCase());
            if (list != null && !list.isEmpty()) {
                for (FFMPVirtualGageBasinMetaData md : list) {
                    if (!result.contains(md.getLookupId())) {
                        result.add(md.getLookupId());
                    }
                }
            }
        }

        return result;
    }

    /**
     * Quick getter to easier pass on the maxExtent directly
     * 
     * @return the maxExtent
     */
    public double getMaxExtent() {
        return this.maxExtent;
    }

}
