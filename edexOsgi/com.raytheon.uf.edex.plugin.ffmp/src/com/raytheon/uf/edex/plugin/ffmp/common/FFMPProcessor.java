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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPConfigurationException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPDataContainer;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPProcessingException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinList;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DataType;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;

/**
 * FFMP Processor
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jul 14, 2009  2152     D. Hladky  Initial release
 * Oct 25, 2012  15514    G. Zhang   Fix ConcurrentModificationException
 * Feb 01, 2013  1569     D. Hladky  Added constants
 * Feb 25, 2013  1660     D. Hladky  FFTI design change to help mosaic
 *                                   processing.
 * May 01, 2013  15684    zhao       Unlock when Exception caught
 * Jul 15, 2013  2184     dhladky    Remove all HUC's for storage except ALL
 * Sep 03, 2013  13083    G. Zhang   Added a fix in
 *                                   processRADAR(ArrayList<SourceBinEntry>).
 * Apr 03, 2014  2940     dhladky    Better error message for bad
 *                                   configurations.
 * Apr 15, 2014  3026     mpduff     Set the xmrg filename into the metadata
 *                                   column.
 * Aug 08, 2015  4722     dhladky    Simplified processing by data type.
 * Aug 26, 2015  4777     dhladky    Fixed bug in DPR accumulations.
 * Sep 28, 2015  4756     dhladky    Multiple Guidance upgrades.
 * Feb 04, 2016  5311     dhladky    Bug in creation of source bins fixed.
 * Apr 07, 2016  5491     tjensen    Fix NullPointerException from
 *                                   getRawGeometries
 * May 17, 2016  19009    dhladky    (code checked in by zhao) Modified DPR
 *                                   calculation in processRADAR()
 * Feb 22, 2017  19392    zhao       change coord ref for grib data from
 *                                   CELL_CENTER to CELL_CORNER to match
 *                                   hydro/ncep RFC FFG
 * Jun 05, 2018  6560     njensen    Cleanup, separate out FFTI logic to
 *                                   FFTIChecker
 * Jun 07, 2018  6560     njensen    Split logic into four subclasses, one per
 *                                   DATA_TYPE
 * Jul 23, 2018  6642     randerso   Code cleanup.
 * Jul 30, 2018  6720     njensen    Update for changed method names
 * Aug 14, 2018  6720     njensen    Use simplified enums
 * Sep 04, 2018  6720     njensen    Added fixme comment
 *
 * </pre>
 *
 * @author dhladky
 */
public abstract class FFMPProcessor {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    protected static final String sourceBinTaskName = "FFMP Source bin";

    protected final FFMPConfig config;

    protected final FFMPTemplates template;

    protected final FFMPGenerator generator;

    protected FFMPRecord ffmpRec = null;

    protected SourceXML source = null;

    protected String dataKey = null;

    // only used by FFG and XMRG(HPE)
    protected String siteKey = null;

    protected String sourceId = null;

    protected SourceBinList sbl = null;

    protected boolean existingSBL = false;

    /** Used for caching */
    protected Geometry[][] pointGeometries = null;

    /** Used for caching */
    protected Map<Long, Geometry> cwaGeometries = null;

    protected GeometryFactory geomFactory = new GeometryFactory();

    /**
     * Public constructor
     *
     * @param config
     */
    public FFMPProcessor(FFMPConfig config, FFMPGenerator generator,
            FFMPRecord ffmpRec, FFMPTemplates template) {
        this.config = config;
        this.ffmpRec = ffmpRec;
        this.template = template;
        this.generator = generator;
        this.siteKey = ffmpRec.getSiteKey();
        this.dataKey = ffmpRec.getDataKey();
    }

    /**
     * Process data input to set values of the FFMPRecord
     */
    public FFMPRecord processFFMP(SourceXML source) throws Exception {
        this.source = source;
        long time = System.currentTimeMillis();

        // setup data files, source access, get record time
        Object data = getSourceData();
        Date recdate = initialSetup(data);
        // set the time for this record
        if (recdate != null) {
            ffmpRec.setDataTime(new DataTime(recdate));
        }

        if (source.isGuidance()) {
            processGuidances();
        } else if (source.getSourceType() == SourceType.GAGE) {
            processVirtualGageBasins();
        } else {
            processSource();
        }

        statusHandler.handle(Priority.INFO,
                "Processed Source: " + source.getSourceName() + " sitekey: "
                        + siteKey + " dataKey: " + dataKey + " time: "
                        + (System.currentTimeMillis() - time));

        return ffmpRec;
    }

    /**
     * Configures the variables necessary to do the processing of the data and
     * gets the ffmpRec's time based on the data's time.
     *
     * @param data
     * @return the time that should be used for the FFMPRecord
     * @throws Exception
     */
    protected abstract Date initialSetup(Object data) throws Exception;

    /**
     * Processes the data for the given pfaf
     *
     * @param pfaf
     * @param cwa
     * @return
     * @throws Exception
     */
    protected abstract float processBasin(Long pfaf, String cwa)
            throws Exception;

    /**
     * Processes the data for the given coordinate
     *
     * @param coord
     * @param area
     * @return
     */
    protected abstract float processCoordinate(Coordinate coord, double area);

    /**
     * Sets up a cache to speed up processing. This is only used if there was
     * not an existing SourceBinList.
     *
     * @throws Exception
     */
    protected abstract void setupCache() throws Exception;

    /**
     * Convenience method to get the Date of the underlying data being
     * processed.
     *
     * @return
     */
    protected abstract Date getDataDate();

    /**
     * Gets the GridGeometry2D associated with the underlying data being
     * processed. May be null depending on the data type.
     *
     * @return
     */
    protected abstract GridGeometry2D getGridGeometry();

    /**
     * Adds extra logging to the StringBuilder for configuration errors
     *
     * @param sb
     */
    protected abstract void addExtraLogging(StringBuilder sb);

    /**
     * Log bad configuration message and throw Configuration Exception
     *
     * @param type
     * @param e
     * @throws FFMPConfigurationException
     */
    protected void logAndThrowConfigurationException(DataType type, Exception e)
            throws FFMPConfigurationException {
        StringBuilder sb = new StringBuilder();
        sb.append(type).append(" Source: ").append(source.getSourceName())
                .append(" has a non-functional configuration! \n");
        sb.append("DataKey: ").append(dataKey).append(" SiteKey: ")
                .append(siteKey).append(" \n");

        addExtraLogging(sb);

        // null out the record it is garbage.
        ffmpRec = null;
        // throw FFMP config exception to stop processing of this source
        throw new FFMPConfigurationException(sb.toString(), e);
    }

    /**
     * Gets the input data for processing
     *
     * @return
     */
    protected Object getSourceData() {
        return config.getSourceData(source.getSourceName()).get(dataKey);
    }

    /**
     * Process source
     *
     * @throws Exception
     */
    protected void processSource() throws Exception {
        try {
            /*
             * process each domain separately, but within the same URI/HDF5
             * named by primary domain
             */
            for (DomainXML domain : template.getDomains()) {

                // reset the geometries
                cwaGeometries = null;

                String cwa = domain.getCwa();
                try {
                    LinkedHashMap<Long, ?> map = template.getMap(siteKey, cwa,
                            FFMPRecord.ALL);

                    // if map is empty the data is outside your domain
                    if (!map.isEmpty()) {
                        existingSBL = false;
                        sbl = null;

                        /*
                         * if bin list doesn't exist create it for future use
                         * when running
                         */
                        this.sourceId = getSourceBinNaming(source) + "-" + cwa
                                + "-" + dataKey;

                        if (sourceId != null) {
                            if (generator.isExistingSourceBin(sourceId)) {
                                sbl = generator.getSourceBinList(sourceId);
                                existingSBL = true;
                            } else {
                                if (checkLockStatus()) {
                                    lock();
                                    sbl = makeNewSBL(cwa, map);
                                    setupCache();
                                } else {
                                    continue;
                                }
                            }
                        } else {
                            ffmpRec = null;
                            return;
                        }

                        for (Long key : map.keySet()) {
                            FFMPBasin basin = getBasin(key);

                            Date date = getDataDate();
                            Float val = processBasin(key, cwa);

                            if (date != null) {
                                basin.setValue(date, val);
                            }
                        }

                        // write new source bins if necessary
                        if (!existingSBL && sbl != null
                                && !sbl.getSourceMap().isEmpty()) {
                            generator.setSourceBinList(sbl);
                        }

                    } else {
                        if (ffmpRec != null) {
                            statusHandler.handle(Priority.DEBUG,
                                    "Source: " + ffmpRec.getSourceName()
                                            + " sitekey: " + siteKey
                                            + " domain: " + cwa
                                            + " : Data outside of domain");
                        }
                    }
                } catch (Exception e) {
                    throw new FFMPProcessingException(
                            "FFMPProcessor: Failed to process source domain: "
                                    + source.getSourceName() + ": " + cwa,
                            e);
                } finally {
                    if (sourceId != null) {
                        if (!checkLockStatus()) {
                            ClusterLockUtils.unlock(sourceBinTaskName,
                                    sourceId);
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw new FFMPProcessingException(
                    "FFMPProcessor: Failed to process source: "
                            + source.getSourceName(),
                    e);
        }
    }

    /**
     * Makes a new SourceBinList. Parameters are only used by subclass
     * implementations.
     *
     * @param cwa
     * @param map
     * @return
     */
    protected SourceBinList makeNewSBL(String cwa, LinkedHashMap<Long, ?> map) {
        return new SourceBinList(sourceId);
    }

    /**
     * Process the Guidance types
     *
     * @throws Exception
     */
    protected void processGuidances() throws Exception {
        /*
         * process each domain separately, but within the same URI/HDF5 named by
         * primary domain
         */
        try {
            List<DomainXML> domains = template.getDomains();

            synchronized (domains) {
                for (DomainXML domain : domains) {
                    // reset the cwa geometries
                    cwaGeometries = null;

                    try {
                        LinkedHashMap<Long, ?> map = template.getMap(siteKey,
                                domain.getCwa(), FFMPRecord.ALL);

                        // if map is empty it's outside my domain
                        if (!map.isEmpty()) {
                            String sourceFamilyName = getSourceBinNaming(
                                    source);
                            this.sourceId = sourceFamilyName + "-"
                                    + domain.getCwa() + "-" + dataKey + "-"
                                    + siteKey;

                            if (sourceId != null) {
                                existingSBL = false;
                                sbl = null;
                                /*
                                 * if bin list dosen't exist create it for
                                 * future use when running
                                 */
                                if (generator.isExistingSourceBin(sourceId)) {
                                    sbl = generator.getSourceBinList(sourceId);
                                    existingSBL = true;
                                } else {
                                    if (checkLockStatus()) {
                                        lock();
                                        sbl = new SourceBinList(sourceId);
                                    } else {
                                        return;
                                    }
                                }
                            } else {
                                ffmpRec = null;
                                return;
                            }

                            for (Long key : map.keySet()) {
                                FFMPBasin basin = getBasin(key);
                                float val = 0.0f;

                                /*
                                 * Cover other types just in case the 0.1% of
                                 * non RFCFFG Gridded data shows up.
                                 */
                                val = processBasin(key, domain.getCwa());

                                if (basin != null) {
                                    setBasin(basin, val);
                                }
                            }

                            // write new source bins if necessary
                            if (!existingSBL && sbl != null
                                    && !sbl.getSourceMap().isEmpty()) {
                                generator.setSourceBinList(sbl);
                            }

                        } else {
                            statusHandler.handle(Priority.INFO,
                                    "Source: " + ffmpRec.getSourceName()
                                            + " sitekey: " + siteKey
                                            + " domain: " + domain.getCwa()
                                            + " : Data outside of domain");
                        }
                    } catch (Exception e) {
                        ffmpRec = null;
                        throw new FFMPProcessingException(
                                "FFMPProcessor: Failed to process source domain: "
                                        + source.getSourceName() + ": "
                                        + domain.getCwa(),
                                e);
                    } finally {
                        if (sourceId != null) {
                            if (!checkLockStatus()) {
                                ClusterLockUtils.unlock(sourceBinTaskName,
                                        sourceId);
                            }
                        }
                    }
                }
            }

            if (source.getInterpolatedGuidanceDelay()) {
                // over rides the date setting.
                Date recdate = ffmpRec.getDataTime().getRefTime();

                try {
                    // FFG , we still use the display name for it
                    String sourceNameString = null;

                    if (source.isMosaic()) {
                        /*
                         * The method is named processGuidances(), so we know we
                         * have a source family associated.
                         */
                        sourceNameString = source.getSourceFamily();
                    } else {
                        sourceNameString = ffmpRec.getSiteKey() + "-"
                                + source.getSourceName();
                    }

                    Date backDate = new Date(
                            ffmpRec.getDataTime().getRefTime().getTime()
                                    - (FFMPGenerator.SOURCE_CACHE_TIME
                                            * TimeUtil.MILLIS_PER_HOUR));

                    FFMPDataContainer ffgContainer = generator
                            .getFFMPDataContainer(sourceNameString, backDate);

                    if (ffgContainer != null && ffgContainer
                            .containsKey(source.getSourceName())) {
                        Date previousDate = ffgContainer.getNewest();

                        if (previousDate != null) {
                            long guidFrequency = ffmpRec.getDataTime()
                                    .getRefTime().getTime()
                                    - previousDate.getTime();

                            /*
                             * used reverse logic from AWIPS I code here,
                             * instead of returning I switched the greater than
                             * and less than so it will process
                             */
                            if (guidFrequency < (FFMPGenerator.SOURCE_CACHE_TIME
                                    * TimeUtil.MILLIS_PER_HOUR)
                                    && guidFrequency >= (TimeUtil.MILLIS_PER_HOUR)) {
                                /*
                                 * FIXME This if statement never evaluates to
                                 * true. If it did, the code will fail with a
                                 * NullPointerException in
                                 * figd.calculateDelayedGuidance(). See related
                                 * comment in
                                 * FFMPInterpolatedGuidanceDelay.java. This code
                                 * should be removed or fixed.
                                 */
                                long newTime = recdate.getTime()
                                        + (int) (source.getDurationHour()
                                                * TimeUtil.MILLIS_PER_HOUR);
                                // this is the new date
                                recdate = new Date(newTime);
                                ProductRunXML productRunner = generator
                                        .getRunConfig().getProduct(siteKey);
                                ProductXML product = generator.getSourceConfig()
                                        .getProductByPrimarySourceName(
                                                productRunner.getProductName());
                                SourceXML qpeSource = generator
                                        .getSourceConfig()
                                        .getSource(product.getQpe());
                                // populate previous rec
                                FFMPInterpolatedGuidanceDelay figd = new FFMPInterpolatedGuidanceDelay(
                                        siteKey, guidFrequency, source,
                                        qpeSource, previousDate, recdate,
                                        generator, ffgContainer.getBasinData(),
                                        ffmpRec);

                                boolean delayGuidance = figd
                                        .calculateDelayedGuidance();
                                // sets the new data time for the record
                                if (delayGuidance) {
                                    ffmpRec.setDataTime(new DataTime(recdate));
                                }

                            }
                        }
                    }
                } catch (Exception e) {
                    throw new FFMPProcessingException(
                            "FFMPProcessor: Failed to Guidance Transition Delay source "
                                    + source.getSourceName(),
                            e);
                }
            }
        } catch (Exception e) {
            ffmpRec = null;
            throw new FFMPProcessingException(
                    "FFMPProcessor: Failed to process source: "
                            + source.getSourceName(),
                    e);
        }
    }

    /**
     * Process the Virtual Gage Basins
     */
    protected void processVirtualGageBasins() {
        // process each domain separately, but within the same URI/HDF5
        // named by primary domain

        for (DomainXML domain : template.getDomains()) {
            LinkedHashMap<String, FFMPVirtualGageBasinMetaData> vmap = template
                    .getVirtualGageBasins(siteKey, domain.getCwa());

            for (Entry<String, FFMPVirtualGageBasinMetaData> entry : vmap
                    .entrySet()) {
                try {
                    FFMPVirtualGageBasinMetaData fvgbmd = entry.getValue();

                    if (fvgbmd != null) {
                        FFMPVirtualGageBasin basin = getVirtualBasin(
                                fvgbmd.getLid(), fvgbmd.getLookupId());

                        Coordinate coor = null;
                        if (getGridGeometry() != null) {
                            // used by Grid and PDO
                            ReferencedCoordinate rc = new ReferencedCoordinate(
                                    fvgbmd.getCoordinate());
                            try {
                                coor = rc.asGridCell(getGridGeometry(),
                                        PixelInCell.CELL_CORNER);
                            } catch (TransformException | FactoryException e) {
                                statusHandler.error("VGB Gridded error", e);
                                continue;
                            }
                        } else {
                            // used by XMRG
                            coor = fvgbmd.getCoordinate();
                        }
                        Date date = getDataDate();
                        Float val = processCoordinate(coor, 1.0);

                        // Missing doesn't work well with Virtual Gage's
                        if (val == FFMPUtils.MISSING) {
                            val = 0.0f;
                        }

                        if (date != null) {
                            basin.setValue(date, val);
                        }
                    }
                } catch (Exception e) {
                    ffmpRec = null;
                    statusHandler.error("Unable to process VGB", e);
                }
            }
        }
    }

    /**
     * Gets the FFMPRecord's basin data
     *
     * @param ffmp
     */
    private FFMPBasinData getBasinData() {
        return ffmpRec.getBasinData();
    }

    /**
     * Gets the basin corresponding to the pfaf
     *
     * @param pfaf
     * @return
     */
    protected FFMPBasin getBasin(Long pfaf) {
        FFMPBasin basin = getBasinData().get(pfaf);
        if (basin == null) {
            basin = new FFMPBasin(pfaf, false);
            getBasinData().put(pfaf, basin);
        }
        return basin;
    }

    /**
     * Gets the virtual gage basin corresponding to the pfaf
     *
     * @param lid
     * @param pfaf
     * @return
     */
    protected FFMPVirtualGageBasin getVirtualBasin(String lid, Long pfaf) {
        FFMPVirtualGageBasin basin = (FFMPVirtualGageBasin) getBasinData()
                .get(pfaf);
        if (basin == null) {
            basin = new FFMPVirtualGageBasin(lid, pfaf, false);
            getBasinData().put(pfaf, basin);
        }
        return basin;
    }

    /**
     * Sets the Basin values
     *
     * @param basin
     * @param val
     */
    protected void setBasin(FFMPBasin basin, Float val) {
        basin.setValue(config.getDate(), val);
    }

    /**
     * Process gradually increasing nest looking for points that fall within the
     * geometry
     *
     * @param geom
     * @param p
     * @param nx
     * @param ny
     * @param x
     * @param y
     * @param returnLatLon
     * @return
     */
    protected List<SourceBinEntry> processNest(Geometry geom, int p, int nx,
            int ny, int x, int y, boolean returnLatLon) {
        List<SourceBinEntry> myPoints = new ArrayList<>();

        try {
            for (int i = p * (-1); i <= p; i++) {
                int xx = x + i;
                // process entire row
                if (i == p * (-1) || i == p) {
                    for (int j = p * (-1); j <= p; j++) {
                        int yy = y + j;
                        if ((yy > ny - 1) || (xx > nx - 1) || (yy < 0)
                                || (xx < 0)) {
                            continue;
                        } else if (geom.contains(
                                pointGeometries[xx][yy].getCentroid())) {
                            SourceBinEntry sbe = new SourceBinEntry();

                            if (returnLatLon) {
                                sbe.setCoor(pointGeometries[xx][yy]
                                        .getCentroid().getCoordinate());
                            } else {
                                sbe.setCoor(new Coordinate(xx, yy));
                                double arealPercent = getArealPercentage(
                                        pointGeometries[xx][yy], geom);
                                sbe.setArea(arealPercent);
                                myPoints.add(sbe);
                            }
                        }
                    }
                }
                // process only book ends
                else {
                    for (int j = p * (-1); j <= p; j++) {
                        int yy = y + j;
                        if (yy == y - p || yy == y + p) {
                            if ((yy > ny - 1) || (xx > nx - 1) || (yy < 0)
                                    || (xx < 0)) {
                                continue;
                            } else if (geom.contains(
                                    pointGeometries[xx][yy].getCentroid())) {
                                SourceBinEntry sbe = new SourceBinEntry();

                                if (returnLatLon) {
                                    sbe.setCoor(pointGeometries[xx][yy]
                                            .getCentroid().getCoordinate());
                                } else {
                                    sbe.setCoor(new Coordinate(xx, yy));
                                    double arealPercent = getArealPercentage(
                                            pointGeometries[xx][yy], geom);
                                    sbe.setArea(arealPercent);
                                    myPoints.add(sbe);
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Unable to process nest! ", e);
        }

        return myPoints;
    }

    /**
     * Gets the correct area percent coverage
     *
     * @param dataPointGeo
     * @param basinGeo
     * @return
     */
    private double getArealPercentage(Geometry dataPointGeo,
            Geometry basinGeo) {
        double arealPercent = 0.0;

        Geometry intersectGeo = basinGeo.intersection(dataPointGeo);
        arealPercent = intersectGeo.getArea() / dataPointGeo.getArea();

        return arealPercent;
    }

    /**
     * Gets the data type associated with the source
     *
     * @return
     */
    protected DataType getDataType() {
        return source.getDataType();
    }

    /**
     * Checks the status of the source bin creation
     *
     * @param sourceId
     * @return
     */
    protected boolean checkLockStatus() {
        ClusterTask task = ClusterLockUtils.lookupLock(sourceBinTaskName,
                sourceId);
        if (task.getLockState() != LockState.ALREADY_RUNNING) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * lock on the cluster
     */
    protected void lock() {
        ClusterLockUtils.lock(sourceBinTaskName, sourceId, 1800 * 1000, true);
    }

    /**
     * Gets the name used for the source bins produced by all sources. Some
     * sources have identical Grid/RADAR bins/etc. So, it's more efficient to
     * name the bins the same so that we don't create identical ones for every
     * single source that uses that bin schema.
     *
     * @param source
     * @return
     */
    protected String getSourceBinNaming(SourceXML source) {
        if (source.isGuidance()) {
            /*
             * ARI brought this forward because there are 6 different ARI
             * sources that all use the same grid. Was pointless to have
             * different source bins for each of them when they are identical.
             */

            /*
             * njensen: If I understand his comment correctly, he's referring to
             * six grids such as ARI30M100YR, ARI1H100YR, ARI3H100YR,
             * ARI6H100YR, ARI12H100YR, ARI24H100YR, aka 30 minute, 1 hour, 3
             * hour, 6 hour, 12 hour, and 24 hour for 100 year floods. But
             * couldn't you take the caching optimization a step further, cause
             * wouldn't the 500 year floods, 1000 year floods, etc also have the
             * same grid and therefore the same source bin?  i.e. Couldn't there
             * be one source bin set for all ARI data?
             */
            return source.getSourceFamily();
        } else if (source.getDataType() == DataType.XMRG) {
            // HPE/BHPE use this type exclusively
            return DataType.XMRG.name();
        } else {
            return source.getSourceName();
        }
    }
}
