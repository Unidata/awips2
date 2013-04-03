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
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.ServerSpecificPersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Record implementation for FFMP plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/03/09     2521     D. Hladky   Initial release
 * 01/27/13     1478        D. Hladky   OUN memory help
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@Entity
@Table(name = "ffmp", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FFMPRecord extends ServerSpecificPersistablePluginDataObject
        implements IPersistable {

    private static final long serialVersionUID = 76774564365671L;

    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String wfo;

    @Column(length = 32)
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String sourceName;

    @Column(length = 32)
    @DataURI(position = 3)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String dataKey;

    @Column(length = 32)
    @DataURI(position = 4)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String siteKey;

    @Transient
    private HashMap<String, FFMPBasinData> basinsMap = new HashMap<String, FFMPBasinData>();

    @Transient
    private int expiration = 0;

    @Transient
    private boolean isRate = false;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPRecord.class);

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
     * @param tableDef
     *            The table definition associated with this class
     */
    public FFMPRecord(String uri) {
        super(uri);
    }

    public enum CLICK_TYPE {

        UP_DOWN("both"), UP("up"), DOWN("DOWN"), TREND("TREND"), CLEAR("CLEAR");

        private final String clickName;

        private CLICK_TYPE(String name) {
            clickName = name;
        }

        public String getClickName() {
            return clickName;
        }
    };

    /**
     * ZOOM levels in FFMP
     * 
     * @author dhladky
     * 
     */
    public enum ZOOM {

        WFO("WFO"), AGGREGATE("AGGREGATE"), BASIN("BASIN");

        private final String zoomLevel;

        private ZOOM(String name) {
            zoomLevel = name;
        }

        public String getZoom() {
            return zoomLevel;
        }
    };

    /**
     * 
     * Enumeration of the of columns you can label by
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum LABEL {

        COUNTY("countyname"), STREAM("streamname"), HUC("huc_name"), PFAF(
                "pfaf_id");

        private final String labelName;

        private LABEL(String name) {
            labelName = name;
        }

        public String getLabelName() {
            return labelName;
        }
    };

    /**
     * 
     * Enumeration of the data fields for FFMP
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum FIELDS {
        NAME("name"), RATE("rate"), QPE("qpe"), QPF("qpf"), GUIDANCE("guidance"), RATIO(
                "ratio"), DIFF("diff"), VIRTUAL("virtual");

        private final String fieldName;

        private FIELDS(String name) {
            fieldName = name;
        }

        public String getFieldName() {
            return fieldName;
        }
    };

    /**
     * Gets the units
     * 
     * @param field
     * @return
     */
    public static String getUnitType(FIELDS field) {
        String unit = null;
        if (field == FIELDS.RATE) {
            unit = "in/hr";
        } else if (field == FIELDS.QPE || field == FIELDS.QPF
                || field == FIELDS.GUIDANCE || field == FIELDS.DIFF) {
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
     * @return
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
     * @param site
     */
    public void setWfo(String wfo) {
        this.wfo = wfo;
    }

    /**
     * Gets the wfo
     * 
     * @return
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
     * Gets the sourceName
     * 
     * @return
     */
    public String getSourceName() {
        return sourceName;
    }

    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return The IDecoderGettable reference for this record. Null for this
     *         class.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    /**
     * finds the correct basin bin by hucName to place into
     * 
     * @param basins
     * @param hucName
     */

    public void setBasinData(FFMPBasinData basins, String hucName) {
        basinsMap.put(hucName, basins);
    }

    /**
     * finds the correct basin bin by hucName to place into
     * 
     * @param basins
     * @param hucName
     */
    public FFMPBasinData getBasinData(String hucName) {
        FFMPBasinData basins = basinsMap.get(hucName);
        if (basins == null) {
            basins = new FFMPBasinData(hucName);
            basinsMap.put(hucName, basins);
        }
        return basins;
    }

    /**
     * Gets the map if you need it
     * 
     * @return
     */
    public HashMap<String, FFMPBasinData> getBasinsMap() {
        return basinsMap;
    }

    /**
     * Gets the Hash out of the datastore by HUC
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveMapFromDataStore(IDataStore dataStore, String uri,
            FFMPTemplates template, String huc, Date date, String sourceName)
            throws Exception {
        FFMPBasinData fbd = null;
        boolean aggregate = true;

        if (huc.equals("ALL")) {
            aggregate = false;
        }

        fbd = getBasinData(huc);

        synchronized (template) {

            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(sourceName);

            for (DomainXML domain : template.getDomains()) {
                LinkedHashMap<Long, ?> map = template.getMap(getSiteKey(),
                        domain.getCwa(), huc);

                if (map != null && map.keySet().size() > 0) {

                    IDataRecord rec = null;

                    try {
                        rec = dataStore.retrieve(uri + "/" + domain.getCwa(),
                                huc, Request.ALL);
                    } catch (Exception e) {
                        statusHandler.handle(Priority.DEBUG,
                                "FFMPRecord: no data record for: " + uri + "/"
                                        + domain.getCwa());
                    }

                    if (rec != null) {
                        float[] values = ((FloatDataRecord) rec).getFloatData();

                        int j = 0;
                        if (values != null) {
                            // System.err.println(sourceName);
                            if (source.getSourceType().equals(
                                    SOURCE_TYPE.GUIDANCE.getSourceType())) {
                                for (Long pfaf : map.keySet()) {
                                    try {
                                        FFMPGuidanceBasin basin = (FFMPGuidanceBasin) fbd
                                                .get(pfaf);

                                        if (basin == null) {
                                            basin = new FFMPGuidanceBasin(pfaf,
                                                    aggregate);
                                            fbd.put(pfaf, basin);
                                        }

                                        if (basin.containsKey(date, sourceName)) {
                                            if (basin
                                                    .getValue(date, sourceName) == FFMPUtils.MISSING
                                                    || basin.getValue(date,
                                                            sourceName).isNaN()) {

                                                float curval = basin.getValue(
                                                        date, sourceName);

                                                if (curval >= 0.0f
                                                        && values[j] >= 0.0f) {
                                                    basin.setValue(sourceName,
                                                            date, (curval + values[j])/ 2);
                                                } else {
                                                    basin.setValue(sourceName,
                                                            date, values[j]);
                                                }

                                            }
                                        } else {
                                            basin.setValue(sourceName, date,
                                                    values[j]);
                                        }

                                        j++;
                                    } catch (Exception e) {
                                        break;
                                    }

                                }
                            } else {
                                for (Long pfaf : map.keySet()) {
                                    try {
                                        FFMPBasin basin = fbd.get(pfaf);
                                        if (basin == null) {
                                            basin = new FFMPBasin(pfaf,
                                                    aggregate);
                                            fbd.put(pfaf, basin);
                                        }

                                        if (basin.contains(date)) {
                                            float curval = basin.getValue(date);
                                            if (curval >= 0.0f
                                                    && values[j] >= 0.0f) {
                                                basin.setValue(date, (curval + values[j])/ 2);;
                                            } else {
                                                basin.setValue(date, values[j]);
                                            }
                                        } else {
                                            basin.setValue(date, values[j]);
                                        }
                                        j++;
                                    } catch (Exception e) {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Gets a single basin out of the dataStore
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveBasinFromDataStore(IDataStore dataStore, String uri,
            FFMPTemplates template, String huc, Date date, String sourceName,
            FFMPBasin basin) {
        try {

            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(sourceName);
            Long pfaf = basin.getPfaf();

            synchronized (template) {

                for (DomainXML domain : template.getDomains()) {

                    LinkedHashMap<Long, ?> map = template.getMap(getSiteKey(),
                            domain.getCwa(), huc);

                    if (map != null && map.get(pfaf) != null) {

                        int index = 0;
                        for (Long pfafToCheck : map.keySet()) {
                            if (pfafToCheck.equals(pfaf)) {
                                break;
                            }
                            index++;
                        }

                        try {
                            IDataRecord rec = dataStore.retrieve(uri + "/"
                                    + domain.getCwa(), huc, Request
                                    .buildPointRequest(new Point(index, 0)));

                            if (rec != null) {
                                float[] values = ((FloatDataRecord) rec)
                                        .getFloatData();

                                boolean isFFG = false;

                                if (source.getSourceType().equals(
                                        SOURCE_TYPE.GUIDANCE.getSourceType())) {
                                    isFFG = true;
                                }

                                if (values != null) {
                                    // System.err.println(sourceName);
                                    if (isFFG) {
                                        ((FFMPGuidanceBasin) basin).setValue(
                                                sourceName, date, values[0]);
                                    } else {
                                        basin.setValue(date, values[0]);
                                    }
                                }
                            }
                        } catch (Throwable e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "ERROR Retrieving Map for URI: " + uri
                                            + "..." + huc);
                            e.printStackTrace();
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "ERROR Retrieving HUC..."
                    + huc);
        }
    }

    /**
     * Gets the Virtual Hash out of the datastore by HUC
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveVirtualMapFromDataStore(IDataStore dataStore,
            String uri, FFMPTemplates template, Date date, String sourceName)
            throws StorageException, FileNotFoundException {
        FFMPBasinData fbd = null;
        boolean aggregate = false;
        fbd = getBasinData("ALL");
        String key = getDataKey();

        synchronized (template) {

            for (DomainXML domain : template.getDomains()) {

                LinkedHashMap<String, FFMPVirtualGageBasinMetaData> lids = template
                        .getVirtualGageBasins(key, domain.getCwa());

                if (lids != null) {
                    int size = lids.size();

                    if (size > 0) {

                        IDataRecord rec = null;

                        try {
                            rec = dataStore.retrieve(
                                    uri + "/" + domain.getCwa(), "ALL",
                                    Request.ALL);
                        } catch (Exception e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "FFMPRecord: no data for: " + uri + "/"
                                            + domain.getCwa());
                        }

                        if (rec != null) {
                            float[] values = ((FloatDataRecord) rec)
                                    .getFloatData();
                            if (values != null) {
                                int j = 0;

                                for (Entry<String, FFMPVirtualGageBasinMetaData> entry : lids
                                        .entrySet()) {
                                    FFMPVirtualGageBasinMetaData fvgbmd = entry
                                            .getValue();
                                    FFMPVirtualGageBasin vgbasin = (FFMPVirtualGageBasin) fbd
                                            .get(fvgbmd.getLookupId());
                                    if (vgbasin == null) {
                                        vgbasin = new FFMPVirtualGageBasin(
                                                fvgbmd.getLid(),
                                                fvgbmd.getLookupId(), aggregate);
                                        fbd.put(fvgbmd.getLookupId(), vgbasin);
                                    }
                                    vgbasin.setValue(date, values[j]);
                                    j++;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Gets the Virtual Hash out of the datastore by HUC
     * 
     * @param dataStore
     * @param huc
     */
    public void retrieveVirtualBasinFromDataStore(IDataStore dataStore,
            String uri, FFMPTemplates template, Date date, FFMPBasin basin) {
        FFMPBasinData fbd = null;
        try {
            boolean aggregate = false;
            fbd = getBasinData("ALL");
            String key = getDataKey();

            for (DomainXML domain : template.getDomains()) {

                LinkedHashMap<String, FFMPVirtualGageBasinMetaData> lids = template
                        .getVirtualGageBasins(key, domain.getCwa());
                int size = lids.size();

                if (size > 0) {
                    try {
                        IDataRecord rec = dataStore
                                .retrieve(uri + "/" + domain.getCwa(), "ALL",
                                        Request.ALL);

                        if (rec != null) {
                            float[] values = ((FloatDataRecord) rec)
                                    .getFloatData();
                            if (values != null) {
                                int j = 0;

                                for (Entry<String, FFMPVirtualGageBasinMetaData> entry : lids
                                        .entrySet()) {
                                    FFMPVirtualGageBasinMetaData fvgbmd = entry
                                            .getValue();
                                    FFMPVirtualGageBasin vgbasin = (FFMPVirtualGageBasin) fbd
                                            .get(fvgbmd.getLookupId());
                                    if (vgbasin == null) {
                                        vgbasin = new FFMPVirtualGageBasin(
                                                fvgbmd.getLid(),
                                                fvgbmd.getLookupId(), aggregate);
                                        fbd.put(fvgbmd.getLookupId(), vgbasin);
                                    }
                                    vgbasin.setValue(date, values[j]);
                                    j++;
                                }
                            }
                        }
                    }

                    catch (Throwable e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "ERROR Retrieving Virtual ..."
                                        + domain.getCwa() + " : " + "ALL");
                    }
                }
            }
        } catch (Throwable e) {
            statusHandler.handle(Priority.ERROR, "ERROR Retrieving Virtual..."
                    + "ALL");
        }
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("\n dataURI: " + getDataURI() + "\n");
        if (basinsMap != null) {
            for (String key : basinsMap.keySet()) {
                sb.append(key + " : " + basinsMap.get(key).getBasins().size()
                        + "\n");
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
     * @return
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
     */
    public String getDataKey() {
        return dataKey;
    }

    public void setExpiration(int expiration) {
        this.expiration = expiration;
    }

    public int getExpiration() {
        return expiration;
    }

    public void setRate(boolean isRate) {
        this.isRate = isRate;
    }

    public boolean isRate() {
        return isRate;
    }

    /**
     * Purges out old data
     * 
     * @param date
     */
    public void purgeData(Date date) {

        for (String ihuc : getBasinsMap().keySet()) {
            FFMPBasinData basinData = getBasinsMap().get(ihuc);
            basinData.purgeData(date);
        }
    }

    public void setSiteKey(String siteKey) {
        this.siteKey = siteKey;
    }

    public String getSiteKey() {
        return siteKey;
    }
    
    /**
     * Get the fully cache ready object
     * @param fileName
     * @return
     */
    public FFMPAggregateRecord getCacheRecord() {
        FFMPAggregateRecord fdcr = new FFMPAggregateRecord();
        
        for (Entry<String,FFMPBasinData> entry: basinsMap.entrySet()) {
            fdcr.setBasinData(entry.getValue());
        }
        
        return fdcr;
    }
    
    /**
     * Creates and populates a version of this record from a cache record
     * 
     * @param fdcr
     */
    public FFMPRecord(FFMPAggregateRecord fdcr) {

        List<Long> times = fdcr.getTimes();

        for (Entry<String, FFMPBasinData> entry : fdcr.getBasinsMap()
                .entrySet()) {

            FFMPBasinData fbd = entry.getValue();
            // Keep in mind times can be null, Guidance basins are like that
            fbd.populate(times);
            setBasinData(fbd, fbd.getHucLevel());
        }
    }

}
