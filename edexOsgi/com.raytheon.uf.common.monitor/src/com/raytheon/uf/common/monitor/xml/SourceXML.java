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
package com.raytheon.uf.common.monitor.xml;

/**
 * XML
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 Jan, 2010 3915         dhladky     Initial creation
 * 18 Apr. 2012 DR 14619	 dhladky     Replace isOverride()
 * 28 Nov. 2012 DR 14412	 gzhang		 makes unit internal to this class
 * </pre>
 * @author dhladky
 * @version 1.0
 */

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.GUIDANCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.RATEORACCCUM;
import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlAccessorType(XmlAccessType.NONE)
public class SourceXML implements ISerializableObject {

    @XmlElement(name = "SourceName")
    protected String sourceName;

    @XmlElement(name = "DisplayName")
    protected String displayName;

    @XmlElement(name = "DurationHour")
    protected double durationHour = 1;

    @XmlElement(name = "dataPath")
    protected String dataPath;

    @XmlElement(name = "plugin")
    protected String plugin;

    @XmlElement(name = "pluginClass")
    protected String pluginClass;

    @XmlElement(name = "pluginDAOClass")
    protected String pluginDAOClass;

    @XmlElement(name = "dataType")
    protected String dataType;

    @XmlElement(name = "sourceType")
    protected String sourceType;

    @XmlElement(name = "rateOrAccum")
    protected String rateOrAccum;

    @XmlElement(name = "expirationMinutes")
    protected int expirationMinutes;

    @XmlElement(name = "interpolatedGuidanceDelay")
    protected boolean interpolatedGuidanceDelay;

    @XmlElement(name = "interpolatedGuidanceTransition")
    protected boolean interpolatedGuidanceTransition;

    //@XmlElement(name = "unit") // DR 14412
    protected String unit;
    
    public static final String UNIT_TXT = "inches"; // DR 14412

    @XmlElement(name = "conversion")
    protected Double conversion = 1.0;

    @XmlElement(name = "mosaic")
    protected boolean mosaic;

    @XmlElement(name = "guidanceType")
    protected String guidanceType;

    @XmlElement(name = "dateFormat")
    protected String dateFormat;

    @XmlElement(name = "hrapGridFactor")
    protected int hrapGridFactor;

    protected boolean isOverride = false;

    public HashMap<String, ArrayList<SourceOverrideParamXML>> sourceOverrideDataMap = null;

    /** YOU CANT OVERRIDE THESE!!!!!!!!!!!!!!!! **/
    /**
     * @return the SourceName
     */
    public String getSourceName() {
        return sourceName;
    }

    /**
     * @param accumSourceName
     */
    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getPlugin() {
        return plugin;
    }

    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }

    public String getSourceType() {
        return sourceType;
    }

    public void setSourceType(String sourceType) {
        this.sourceType = sourceType;
    }

    public String getPluginClass() {
        return pluginClass;
    }

    public void setPluginClass(String pluginClass) {
        this.pluginClass = pluginClass;
    }

    public String getPluginDAOClass() {
        return pluginDAOClass;
    }

    public void setPluginDAOClass(String pluginDAOClass) {
        this.pluginDAOClass = pluginDAOClass;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public boolean isMosaic() {
        return mosaic;
    }

    public void setMosaic(boolean mosaic) {
        this.mosaic = mosaic;
    }

    public String getGuidanceType() {
        return guidanceType;
    }

    public void setGuidanceType(String guidanceType) {
        this.guidanceType = guidanceType;
    }

    /** YOU CAN OVERRIDE THESE!!!!!!!!!!!!!!!!!!! **/

    public String getDataPath() {
        return dataPath;
    }

    public String getDataPath(String siteKey) {
        if (isOverride(siteKey, "dataPath")) {
            return getParamOverride(siteKey, "dataPath");
        }
        return getDataPath();
    }

    public void setDataPath(String dataPath) {
        this.dataPath = dataPath;
    }

    public double getDurationHour() {
        return durationHour;
    }

    public double getDurationHour(String siteKey) {
        if (isOverride(siteKey, "DurationHour")) {
            return new Double(getParamOverride(siteKey, "DurationHour"))
                    .doubleValue();
        }
        return getDurationHour();
    }

    public void setDurationHour(double durationHour) {
        this.durationHour = durationHour;
    }

    private String getRateOrAccum() {
        return rateOrAccum;
    }

    public String getRateOrAccum(String siteKey) {
        if (isOverride(siteKey, "rateOrAccum")) {
            return getParamOverride(siteKey, "rateOrAccum");
        }
        return getRateOrAccum();
    }

    public void setRateorAccum(String rateOrAccum) {
        this.rateOrAccum = rateOrAccum;
    }

    private int getExpirationMinutes() {
        return expirationMinutes;
    }

    public int getExpirationMinutes(String siteKey) {
        if (isOverride(siteKey, "expirationMinutes")) {
            return new Integer(getParamOverride(siteKey, "expirationMinutes"))
                    .intValue();
        }
        return getExpirationMinutes();
    }

    public void setExpirationMinutes(int expirationMinutes) {
        this.expirationMinutes = expirationMinutes;
    }

    public boolean getInterpolatedGuidanceDelay() {
        return interpolatedGuidanceDelay;
    }

    public boolean getInterpolatedGuidanceDelay(String siteKey) {
        if (isOverride(siteKey, "interpolatedGuidanceDelay")) {
            return new Boolean(getParamOverride(siteKey,
                    "interpolatedGuidanceDelay")).booleanValue();
        }
        return getInterpolatedGuidanceDelay();
    }

    public void setInterpolatedGuidanceDelay(boolean interpolatedGuidanceDelay) {
        this.interpolatedGuidanceDelay = interpolatedGuidanceDelay;
    }

    public boolean isInterpolatedGuidanceTransition() {
        return interpolatedGuidanceTransition;
    }

    public boolean isInterpolatedGuidanceTransition(String siteKey) {
        if (isOverride(siteKey, "interpolatedGuidanceTransition")) {
            return new Boolean(getParamOverride(siteKey,
                    "interpolatedGuidanceTransition")).booleanValue();
        }
        return isInterpolatedGuidanceTransition();
    }

    public void setInterpolatedGuidanceTransition(
            boolean interpolatedGuidanceTransition) {
        this.interpolatedGuidanceTransition = interpolatedGuidanceTransition;
    }

    public String getUnit() {
    	if( unit == null || unit.isEmpty()) // DR 14412
    		unit = UNIT_TXT;
        return unit;
    }

    public String getUnit(String siteKey) {
        if (isOverride(siteKey, "unit")) {
            return getParamOverride(siteKey, "unit");
        }
        return getUnit();
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    private Double getConversion() {
        return conversion;
    }

    public Double getConversion(String siteKey) {
        if (isOverride(siteKey, "conversion")) {
            return new Double(getParamOverride(siteKey, "conversion"))
                    .doubleValue();
        }
        return getConversion();
    }

    public void setConversion(Double conversion) {
        this.conversion = conversion;
    }

    public String getDateFormat() {
        return dateFormat;
    }

    public String getDateFormat(String siteKey) {
        if (isOverride(siteKey, "dateFormat")) {
            return getParamOverride(siteKey, "dateFormat");
        }
        return getDateFormat();
    }

    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    public void setHrapGridFactor(int hrapGridFactor) {
        this.hrapGridFactor = hrapGridFactor;
    }

    public int getHrapGridFactor() {
        return hrapGridFactor;
    }

    public int getHrapGridFactor(String siteKey) {
        if (isOverride(siteKey, "hrapGridFactor")) {
            return new Integer(getParamOverride(siteKey, "hrapGridFactor"))
                    .intValue();
        }
        return getHrapGridFactor();
    }

    /**
     * rate or accumulator
     * 
     * @return
     */
    public boolean isRate() {
        if (getRateOrAccum().equals(RATEORACCCUM.RATE.getRateOrAccum())) {
            return true;
        }

        else {
            return false;
        }
    }

    /**
     * is RFC guidance type set?
     * 
     * @return
     */
    public boolean isRfc() {
        if (getGuidanceType() != null
                && getGuidanceType()
                        .equals(GUIDANCE_TYPE.RFC.getGuidanceType())) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isOverride() {
        return isOverride;
    }

    public void setOverrride(boolean isOverride) {
        this.isOverride = isOverride;
    }

    public void setOverrideDataKeys(
            ArrayList<SourceOverrideDataKeyXML> sourceOverrideDataKeys) {
        sourceOverrideDataMap = new HashMap<String, ArrayList<SourceOverrideParamXML>>();
        for (SourceOverrideDataKeyXML sodkx : sourceOverrideDataKeys) {
            sourceOverrideDataMap.put(sodkx.getDataKey(),
                    sodkx.getSourceOverrideParams());
        }
    }

    /**
     * Get the object you are overriding
     * 
     * @param key
     * @param param
     * @return
     */
    private String getParamOverride(String key, String param) {
        for (SourceOverrideParamXML sopx : sourceOverrideDataMap.get(key)) {
            if (sopx.getParam().equals(param)) {
                return sopx.getValue();
            }
        }
        return null;
    }

    /**
     * Check to see if we are overridden
     * 
     * @param key
     * @param param
     * @return
     *//* 2012-04-18: Old version: keep for reference: Gang Zhang
    private boolean isOverride(String key, String param) {
        if (sourceOverrideDataMap != null) {
            if (sourceOverrideDataMap.containsKey(key)) {
                if (sourceOverrideDataMap.get(key).contains(param)) {
                    return true;
                }
            }
        }

        return false;
    }
*/
    /**2012-04-18: code is from David Hladky in Omaha 
     * for DR 14619/14620. Gang Zhang is checked in
     * Check to see if we are overridden
     * 
     * @param key
     * @param param
     * @return
     */    
    private boolean isOverride(String key, String param) {
        if (sourceOverrideDataMap != null) {
            if (sourceOverrideDataMap.containsKey(key)) {
                for (SourceOverrideParamXML paramx:  sourceOverrideDataMap.get(key)) {
                    if (paramx.getParam().equals(param)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }    
}
