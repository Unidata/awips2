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
package com.raytheon.uf.common.dataplugin.npp.viirs;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * VIIRS geographic data record object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@Entity
@Table(name = "viirs_spatial", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@DynamicSerialize
public class VIIRSSpatialRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = -2532225158997059309L;

    public static final String VALID_HEIGHT_ID = "validHeight";

    @DataURI(position = 1, embedded = true)
    @Embedded
    @DynamicSerializeElement
    private VIIRSCommonData commonData = new VIIRSCommonData();;

    public VIIRSSpatialRecord() {
        // We want pluginName to be "viirs"
        setPluginName(VIIRSDataRecord.class.getAnnotation(Table.class).name());
    }

    /**
     * @return
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#getRegion()
     */
    public String getRegion() {
        return commonData.getRegion();
    }

    /**
     * @param region
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#setRegion(java.lang.String)
     */
    public void setRegion(String region) {
        commonData.setRegion(region);
    }

    /**
     * @return
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#getChannelType()
     */
    public String getChannelType() {
        return commonData.getChannelType();
    }

    /**
     * @param channelType
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#setChannelType(java.lang.String)
     */
    public void setChannelType(String channelType) {
        commonData.setChannelType(channelType);
    }

    /**
     * @return
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#getLevels()
     */
    public int getLevels() {
        return commonData.getLevels();
    }

    /**
     * @param levels
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#setLevels(int)
     */
    public void setLevels(int levels) {
        commonData.setLevels(levels);
    }

    /**
     * @return
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#getWidth()
     */
    public int getWidth() {
        return commonData.getWidth();
    }

    /**
     * @param width
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#setWidth(int)
     */
    public void setWidth(int width) {
        commonData.setWidth(width);
    }

    /**
     * @return
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#getHeight()
     */
    public int getHeight() {
        return commonData.getHeight();
    }

    /**
     * @param height
     * @see com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData#setHeight(int)
     */
    public void setHeight(int height) {
        commonData.setHeight(height);
    }

    /**
     * @return the commonData
     */
    public VIIRSCommonData getCommonData() {
        return commonData;
    }

    /**
     * @param commonData
     *            the commonData to set
     */
    public void setCommonData(VIIRSCommonData commonData) {
        this.commonData = commonData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.persist.IPersistable#getPersistenceTime
     * ()
     */
    @Override
    public Date getPersistenceTime() {
        Calendar c = getInsertTime();
        if (c == null)
            return null;

        return c.getTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.persist.IPersistable#setPersistenceTime
     * (java.util.Date)
     */
    @Override
    public void setPersistenceTime(Date persistTime) {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTime(persistTime);
        setInsertTime(c);
    }

    /**
     * Get the name of the longitude dataset for the level
     * 
     * @param level
     * @return
     */
    public static String getLongitudeDataSet(int level) {
        return "Longitudes-" + level;
    }

    /**
     * Get the name of the latitude dataset for the level
     * 
     * @param level
     * @return
     */
    public static String getLatitudeDataSet(int level) {
        return "Latitudes-" + level;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.PluginDataObject#getDecoderGettable()
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }
}
