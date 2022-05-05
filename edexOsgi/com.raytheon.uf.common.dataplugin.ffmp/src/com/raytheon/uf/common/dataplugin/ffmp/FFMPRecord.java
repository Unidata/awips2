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

import java.awt.Point;
import java.io.File;
import java.io.FileNotFoundException;
import java.lang.ref.WeakReference;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * Record implementation for FFMP plugin
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jun 03, 2009  2521     D. Hladky  Initial release
 * Jan 27, 2013  1478     D. Hladky  OUN memory help
 * Feb 28, 2013  1729     dhladky    Supressed un-necessary debug loggers
 * Apr 04, 2013  1846     bkowal     Added an index on refTime and forecastTime
 * Apr 08, 2013  1293     bkowal     Removed references to hdffileid.
 * Apr,09, 2013  1890     dhladky    Moved dates to referenced map in record
 *                                   rather than multiple dates in FFMPBasin
 *                                   objs.
 * Apr 12, 2013  1857     bgonzale   Added SequenceGenerator annotation.
 * Apr 16, 2013  1912     bsteffen   Initial bulk hdf5 access for ffmp
 * Apr 18, 2013  1919     dhladky    Added method for VGB loading
 * May 07, 2013  1869     bsteffen   Remove dataURI column from
 *                                   PluginDataObject.
 * Jul 15, 2013  2184     dhladky    Remove all HUC's for storage except ALL
 * Aug 30, 2013  2298     rjpeter    Make getPluginName abstract
 * Oct 14, 2013  2361     njensen    Removed XML annotations
 * May 01, 2014  3026     mpduff     Added metadata column.
 * Jun 11, 2014  2061     bsteffen   Remove IDecoderGettable
 * Jul 23, 2015  2360     rferrel    Add name to unique constraint.
 * Oct 10, 2015  4756     dhladky    Got rid of e.printStacks.
 * Jul 20, 2018  6642     randerso   Code cleanup.
 * Aug 06, 2018  6720     njensen    Minor cleanup
 * Aug 14, 2018  6720     njensen    Use simplified enums
 *
 * </pre>
 *
 * @author dhladky
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ffmpseq")
@Table(name = "ffmp", uniqueConstraints = {
        @UniqueConstraint(name = "uk_ffmp_datauri_fields", columnNames = {
                "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ffmp", indexes = {
        @Index(name = "ffmp_refTimeIndex", columnNames = { "refTime",
                "forecastTime" }) })
@DynamicSerialize
public class FFMPRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 76774564365671L;

    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String wfo;

    @Column(length = 32)
    @DataURI(position = 2)
    @DynamicSerializeElement
    private String sourceName;

    @Column(length = 32)
    @DataURI(position = 3)
    @DynamicSerializeElement
    private String dataKey;

    @Column(length = 32)
    @DataURI(position = 4)
    @DynamicSerializeElement
    private String siteKey;

    @Column(length = 255)
    @DynamicSerializeElement
    private String metaData;

    @Transient
    private FFMPBasinData basins = new FFMPBasinData();

    @Transient
    private int expiration = 0;

    @Transient
    private boolean isRate = false;

    protected static ConcurrentMap<Long, WeakReference<ImmutableDate>> cacheTimes = new ConcurrentHashMap<>();

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPRecord.class);

    /** ALL HUC LEVEL **/
    public static final String ALL = "ALL";

    /** COUNTY HUC LEVEL **/
    public static final String COUNTY = "COUNTY";

    /** VIRTUAL HUC LEVEL **/
    public static final String VIRTUAL = "VIRTUAL";

    /**
     * Default Constructor
     */
    public FFMPRecord() {
    }

    /**
     * Constructs a record from a dataURI
     *
     * @param uri
     *            The dataURI
     */
    public FFMPRecord(String uri) {
        super(uri);
    }

    /** Click Type enum */
    public enum CLICK_TYPE {
        UP_DOWN, UP, DOWN, TREND, CLEAR;
    }

    /** ZOOM levels in FFMP */
    public enum ZOOM {
        WFO, AGGREGATE, BASIN;
    }

    /** Enumeration of the data fields for FFMP */
    public enum FIELDS {
        NAME("name"),
        RATE("rate"),
        QPE("qpe"),
        QPF("qpf"),
        GUIDANCE("guidance"),
        RATIO("ratio"),
        DIFF("diff"),
        VIRTUAL("virtual");

        private final String fieldName;

        private FIELDS(String name) {
            fieldName = name;
        }

        /**
         * @return the field name
         */
        public String getFieldName() {
            return fieldName;
        }
    }

    /**
     * Gets the units
     *
     * @param field
     * @return the units
     */
    public static String getUnitType(FIELDS field) {
        String unit = null;
        if (field == FIELDS.RATE) {
            unit = "in/hr";
        } else if ((field == FIELDS.QPE) || (field == FIELDS.QPF)
                || (field == FIELDS.GUIDANCE) || (field == FIELDS.DIFF)) {
            unit = "in";
        } else if (field == FIELDS.RATIO) {
            unit = "%";
        }

        return unit;
    }

    /**
     * Finds the correct text for the screen string
     *
     * @param field
     * @return the field description
     */
    public static String getFieldLongDescription(FIELDS field) {
        String desc = null;
        if (field == FIELDS.RATE) {
            desc = "Instantaneous Precip Rate";
        } else if (field == FIELDS.QPE) {
            desc = "Precipitation Accumulation";
        } else if (field == FIELDS.QPF) {
            desc = "Precipitation Accumulation";
        } else if (field == FIELDS.GUIDANCE) {
            desc = "Precip Guidance";
        } else if (field == FIELDS.DIFF) {
            desc = "Precip - Guidance Diff";
        } else if (field == FIELDS.RATIO) {
            desc = "Precip / Guidance Ratio";
        }
        return desc;
    }

    /**
     * Set the site
     *
     * @param wfo
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * @return the wfo
     */
    public String getWfo() {
        return wfo;
    }

    /**
     * Set the sourceName
     *
     * @param sourceName
     */
    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    /**
     * @return the sourceName
     */
    public String getSourceName() {
        return sourceName;
    }

    /**
     * finds the correct basin bin by hucName to place into
     *
     * @param basins
     */
    public void setBasinData(FFMPBasinData basins) {
        this.basins = basins;
    }

    /**
     * @return the basin data
     */
    public FFMPBasinData getBasinData() {
        return basins;
    }

    /**
     * Gets the Hash out of the datastore by HUC
     *
     * @param datastoreFile
     * @param uri
     * @param template
     * @param date
     * @param sourceName
     * @throws Exception
     */
    public void retrieveMapFromDataStore(File datastoreFile, String uri,
            FFMPTemplates template, Date date, String sourceName)
            throws Exception {
        FFMPBasinData fbd = getBasinData();
        ImmutableDate idate = getCacheDate(date);
        boolean aggregate = false;

        for (DomainXML domain : template.getDomains()) {
            LinkedHashMap<Long, ?> map = template.getMap(getSiteKey(),
                    domain.getCwa(), FFMPRecord.ALL);

            if ((map != null) && !map.isEmpty()) {
                fbd.addBasins(datastoreFile, uri, getSiteKey(), domain.getCwa(),
                        FFMPRecord.ALL, sourceName, idate, map.keySet(),
                        aggregate);
            }
        }
    }

    /**
     * Retrieve map from data store
     *
     * @param template
     * @throws Exception
     */
    public void retrieveMapFromDataStore(FFMPTemplates template)
            throws Exception {
        retrieveMapFromDataStore(getDataStoreFile(), getDataURI(), template,
                getDataTime().getRefTime(), getSourceName());
    }

    /**
     * Retrieve virtual map from data store
     *
     * @param template
     * @throws Exception
     */
    public void retrieveVirtualMapFromDataStore(FFMPTemplates template)
            throws Exception {
        retrieveVirtualMapFromDataStore(getDataStoreFile(), getDataURI(),
                template, getDataTime().getRefTime());
    }

    private File getDataStoreFile() {
        IHDFFilePathProvider pathProvider = getHDFPathProvider();

        String path = pathProvider.getHDFPath(getPluginName(), this);
        String fileName = pathProvider.getHDFFileName(getPluginName(), this);

        File datastoreFile = new File(getPluginName() + IPathManager.SEPARATOR
                + path + IPathManager.SEPARATOR + fileName);
        return datastoreFile;
    }

    /**
     * Gets a single basin out of the dataStore
     *
     * @param dataStore
     * @param uri
     * @param template
     * @param huc
     * @param date
     * @param sourceName
     * @param basin
     */
    public void retrieveBasinFromDataStore(IDataStore dataStore, String uri,
            FFMPTemplates template, String huc, Date date, String sourceName,
            FFMPBasin basin) {
        try {

            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(sourceName);
            Long pfaf = basin.getPfaf();
            ImmutableDate idate = getCacheDate(date);

            for (DomainXML domain : template.getDomains()) {

                LinkedHashMap<Long, ?> map = template.getMap(getSiteKey(),
                        domain.getCwa(), huc);

                if ((map != null) && (map.get(pfaf) != null)) {

                    int index = 0;
                    for (Long pfafToCheck : map.keySet()) {
                        if (pfafToCheck.equals(pfaf)) {
                            break;
                        }
                        index++;
                    }

                    try {
                        IDataRecord rec = dataStore.retrieve(
                                uri + DataStoreFactory.DEF_SEPARATOR
                                        + domain.getCwa(),
                                huc,
                                Request.buildPointRequest(new Point(index, 0)));

                        if (rec != null) {
                            float[] values = ((FloatDataRecord) rec)
                                    .getFloatData();

                            boolean isFFG = source.isGuidance();

                            if (values != null) {
                                if (isFFG) {
                                    ((FFMPGuidanceBasin) basin).setValue(
                                            sourceName, idate, values[0]);
                                } else {
                                    basin.setValue(idate, values[0]);
                                }
                            }
                        }
                    } catch (Throwable e) {
                        statusHandler.error("Error retrieving map for URI: "
                                + uri + " and HUC " + huc, e);
                    }
                }
            }

        } catch (Exception e) {
            statusHandler.error("Error retrieving HUC " + huc, e);
        }
    }

    /**
     * Gets the Virtual Hash out of the datastore by HUC
     *
     * @param datastoreFile
     * @param uri
     * @param template
     * @param date
     * @throws StorageException
     * @throws FileNotFoundException
     */
    public void retrieveVirtualMapFromDataStore(File datastoreFile, String uri,
            FFMPTemplates template, Date date)
            throws StorageException, FileNotFoundException {
        FFMPBasinData fbd = getBasinData();
        String key = getDataKey();
        ImmutableDate idate = getCacheDate(date);

        for (DomainXML domain : template.getDomains()) {

            LinkedHashMap<String, FFMPVirtualGageBasinMetaData> lids = template
                    .getVirtualGageBasins(key, domain.getCwa());

            if (lids != null) {
                int size = lids.size();

                if (size > 0) {
                    fbd.addVirtualBasins(datastoreFile, uri, key,
                            domain.getCwa(), idate, lids.values());
                }
            }
        }
    }

    /**
     * Gets the Virtual Hash out of the datastore by HUC
     *
     * @param datastoreFile
     * @param uri
     * @param template
     * @param date
     */
    public void retrieveVirtualBasinFromDataStore(File datastoreFile,
            String uri, FFMPTemplates template, Date date) {
        try {
            // Should this be retrieving a single basin instead of all of them?
            retrieveVirtualMapFromDataStore(datastoreFile, uri, template, date);
        } catch (Throwable e) {
            statusHandler.error("Error retrieving virtual for URI: " + uri, e);
        }
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n dataURI: " + getDataURI() + "\n");
        if (basins != null) {
            for (Long key : basins.getBasins().keySet()) {
                sb.append(key + " : " + basins.get(key).getValue() + "\n");
            }
        }

        sb.append("WFO/CWA: " + getWfo() + "\n");
        sb.append("SourceName: " + getSourceName() + "\n");
        sb.append("SiteKey: " + getSiteKey() + "\n");
        sb.append("DataKey: " + getDataKey() + "\n");
        sb.append("dataTime: "
                + getDataTime().getValidTime().getTime().toString() + "\n");
        sb.append("PersistanceTime: " + getPersistenceTime().toString() + "\n");

        return sb.toString();
    }

    /**
     * Gets you the path to the FFMP data
     *
     * @return the path to the ffmp data file
     */
    public String getDataPath() {
        return getWfo() + File.separator + getSourceName() + File.separator
                + getDataKey();
    }

    /**
     * corresponds to What is processed in the FFMPRunConfig
     *
     * @param dataKey
     */
    public void setDataKey(String dataKey) {
        this.dataKey = dataKey;
    }

    /**
     * corresponds to What is processed in the FFMPRunConfig
     *
     * @return the dataKey
     */
    public String getDataKey() {
        return dataKey;
    }

    /**
     * Sets the expiration
     *
     * @param expiration
     */
    public void setExpiration(int expiration) {
        this.expiration = expiration;
    }

    /**
     * @return the expiration
     */
    public int getExpiration() {
        return expiration;
    }

    /**
     * Sets the rate flag
     *
     * @param isRate
     */
    public void setRate(boolean isRate) {
        this.isRate = isRate;
    }

    /**
     * @return the rate flag
     */
    public boolean isRate() {
        return isRate;
    }

    /**
     * @return the metaData
     */
    public String getMetaData() {
        return metaData;
    }

    /**
     * @param metaData
     *            the metaData to set
     */
    public void setMetaData(String metaData) {
        this.metaData = metaData;
    }

    /**
     * Purges out old data
     *
     * @param date
     */
    public void purgeData(Date date) {
        basins.purgeData(date);
    }

    /**
     * Sets the siteKey
     *
     * @param siteKey
     */
    public void setSiteKey(String siteKey) {
        this.siteKey = siteKey;
    }

    /**
     * @return the siteKey
     */
    public String getSiteKey() {
        return siteKey;
    }

    /**
     * Get the fully populated aggregate record
     *
     * @return the aggregate record
     */
    public FFMPAggregateRecord getAggregateRecord() {
        FFMPAggregateRecord fdcr = new FFMPAggregateRecord();

        fdcr.setBasins(basins);

        return fdcr;
    }

    /**
     * Creates and populates a version of this record from an aggregate record
     *
     * @param fdcr
     */
    public FFMPRecord(FFMPAggregateRecord fdcr) {

        List<Long> times = fdcr.getTimes();
        FFMPBasinData fdcrBasins = fdcr.getBasins();
        fdcrBasins.populate(times);
        setBasinData(fdcrBasins);
    }

    /**
     * Gets and maintains the list of times. This will lesson memory consumption
     * because it means all FFMPBasin TreeMap date keys reference back to this
     * Hash. Seeing as there are 10000+ of those this will certainly help.
     *
     * @param date
     * @return
     */
    protected ImmutableDate getCacheDate(Date date) {

        WeakReference<ImmutableDate> idate = cacheTimes.get(date.getTime());
        ImmutableDate myDate = null;

        if (idate != null) {
            myDate = idate.get();
        }

        if (myDate == null) {
            long time = date.getTime();
            myDate = new ImmutableDate(time);
            idate = new WeakReference<>(myDate);
            cacheTimes.putIfAbsent(time, idate);
        }

        return myDate;
    }

    /**
     * Populate data from the cache files
     *
     * @param basins
     */
    public void populate(FFMPBasinData basins) {
        setBasinData(basins);
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "ffmp";
    }
}
