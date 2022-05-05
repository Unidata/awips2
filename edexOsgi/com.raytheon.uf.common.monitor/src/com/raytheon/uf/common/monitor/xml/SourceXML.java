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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.DataType;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.GuidanceType;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.RateOrAccum;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;

/**
 * SourceXML. This corresponds to a source tag in FFMPSourceConfiguration.xml.
 * Note that some fields may be null for some sources.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 Jan, 2010 3915         dhladky   Initial creation
 * 18 Apr. 2012 DR 14619     dhladky   Replace isOverride()
 * 28 Nov. 2012 DR 14412     gzhang    makes unit internal to this class
 * 28 Sep  2015 4756         dhladky   Multiple Guidance upgrades.
 * Jul 11, 2018 6695         njensen   Overrode toString()
 * Jul 30, 2018 6720         njensen   Added getDisplayNameForQuestionablePurposes()
 * Aug 14, 2018 6720         njensen   Use enums for dataType, sourceType, guidanceType
 *                                     Added isGuidance()
 * Aug 24, 2018 6720         njensen   Added giant comment to isRate()
 * 
 * </pre>
 * 
 * @author dhladky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SourceXML {

    // DR 14412
    public static final String UNIT_TXT = "inches";

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
    protected DataType dataType;

    @XmlElement(name = "sourceType")
    protected SourceType sourceType;

    @XmlElement(name = "rateOrAccum")
    protected String rateOrAccum;

    @XmlElement(name = "expirationMinutes")
    protected int expirationMinutes;

    @XmlElement(name = "interpolatedGuidanceDelay")
    protected boolean interpolatedGuidanceDelay;

    @XmlElement(name = "interpolatedGuidanceTransition")
    protected boolean interpolatedGuidanceTransition;

    @XmlElement(name = "sourceFamily")
    protected String sourceFamily;

    // @XmlElement(name = "unit") // DR 14412
    protected String unit;

    @XmlElement(name = "conversion")
    protected Double conversion = 1.0;

    @XmlElement(name = "mosaic")
    protected boolean mosaic;

    @XmlElement(name = "guidanceType")
    protected GuidanceType guidanceType;

    @XmlElement(name = "dateFormat")
    protected String dateFormat;

    @XmlElement(name = "hrapGridFactor")
    protected int hrapGridFactor;

    protected boolean isOverride = false;

    public Map<String, List<SourceOverrideParamXML>> sourceOverrideDataMap = null;

    /* YOU CANT OVERRIDE THESE!!!!!!!!!!!!!!!! */

    /*
     * TODO I believe what he ^^^ meant is that the following methods apply to
     * all SourceXML objects, vs down below they only apply to some SourceXML
     * objects. A better solution would be to make a super class with the
     * commonality and subclasses for the remaining fields.
     */

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    /**
     * This method returns the displayName but indicates that it's not clear if
     * the calling code should be using displayName, sourceName, sourceFamily,
     * or something else. If the calling code should be using the display name,
     * switch the call to getDisplayName(). Eventually this method should be
     * deleted when nothing is calling it anymore.
     * 
     * @return
     */
    public String getDisplayNameForQuestionablePurposes() {
        return getDisplayName();
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

    public SourceType getSourceType() {
        return sourceType;
    }

    public void setSourceType(SourceType sourceType) {
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

    public DataType getDataType() {
        return dataType;
    }

    public void setDataType(DataType dataType) {
        this.dataType = dataType;
    }

    public boolean isMosaic() {
        return mosaic;
    }

    public void setMosaic(boolean mosaic) {
        this.mosaic = mosaic;
    }

    public GuidanceType getGuidanceType() {
        return guidanceType;
    }

    public void setGuidanceType(GuidanceType guidanceType) {
        this.guidanceType = guidanceType;
    }

    /* YOU CAN OVERRIDE THESE!!!!!!!!!!!!!!!!!!! */

    /*
     * TODO I believe what he ^^^ meant is that the following methods apply to
     * only some SourceXML objects, most likely dependent on dataType, vs above
     * they apply to all SourceXML objects. A better solution would be to make a
     * super class with the commonality and subclasses for the remaining fields.
     */

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

    private RateOrAccum getRateOrAccum() {
        return RateOrAccum.valueOf(rateOrAccum.toUpperCase());
    }

    public RateOrAccum getRateOrAccum(String siteKey) {
        if (isOverride(siteKey, "rateOrAccum")) {
            String override = getParamOverride(siteKey, "rateOrAccum");
            return RateOrAccum.valueOf(override.toUpperCase());
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
            return new Boolean(
                    getParamOverride(siteKey, "interpolatedGuidanceDelay"))
                            .booleanValue();
        }
        return getInterpolatedGuidanceDelay();
    }

    public void setInterpolatedGuidanceDelay(
            boolean interpolatedGuidanceDelay) {
        this.interpolatedGuidanceDelay = interpolatedGuidanceDelay;
    }

    public boolean isInterpolatedGuidanceTransition() {
        return interpolatedGuidanceTransition;
    }

    public boolean isInterpolatedGuidanceTransition(String siteKey) {
        if (isOverride(siteKey, "interpolatedGuidanceTransition")) {
            return new Boolean(
                    getParamOverride(siteKey, "interpolatedGuidanceTransition"))
                            .booleanValue();
        }
        return isInterpolatedGuidanceTransition();
    }

    public void setInterpolatedGuidanceTransition(
            boolean interpolatedGuidanceTransition) {
        this.interpolatedGuidanceTransition = interpolatedGuidanceTransition;
    }

    public String getUnit() {
        // DR 14412
        if (unit == null || unit.isEmpty()) {
            unit = UNIT_TXT;
        }
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
        /*
         * FIXME njensen: This method is implemented incorrectly. The previous
         * implementation of getRateOrAccum() did not take into account the case
         * (upper vs lower) of the value in the config file. Therefore the
         * previous code that used String.equals(String) instead of enums would
         * always return false as the case in the config file was lowercase
         * while the case from the enum was uppercase. The actual code evaluated
         * to 'return "rate".equals("RATE");' so isRate() always returned false.
         * I verified that all sites have the lowercase "rate" and not the
         * uppercase "RATE" in their FFMPSourceConfig.xml.
         * 
         * (What does rate or accum actually mean? Try making sense of this:
         * http://www.nws.noaa.gov/mdl/ffmp/FFMPAsourceFile_OB93.pdf)
         * 
         * In particular, this isRate() boolean alters the results of qpe,
         * guidance ratio, and guidance diff (and possibly more?) in the basins
         * table. Forcibly returning false here makes the code produce the same
         * outputs as before. It is currently suspected that the code in
         * FFMPBasin.getAccumValue() should have if(rate) instead of if(!rate).
         * Or perhaps remove the if check as the method is named
         * getAccumValue(), implying rate should be irrelevant. For other
         * interpretations and insights see the comment in
         * FFMPGeometryFactory.makeGeometryData().
         * 
         * The line commented out below would be the correct implementation of
         * this method, however, that significantly alters the values for qpe,
         * guidance diff, and guidance ratio. For now, I am leaving this method
         * always returning false so it matches the previous behavior and the
         * calculations produce the same results.
         */
        // return getRateOrAccum() == RateOrAccum.RATE;
        return false;
    }

    /**
     * is RFC guidance type set?
     * 
     * @return
     */
    public boolean isRfc() {
        return getGuidanceType() == GuidanceType.RFC;
    }

    public boolean isGuidance() {
        return getSourceType() == SourceType.GUIDANCE;
    }

    public boolean isOverride() {
        return isOverride;
    }

    public void setOverrride(boolean isOverride) {
        this.isOverride = isOverride;
    }

    public void setOverrideDataKeys(
            List<SourceOverrideDataKeyXML> sourceOverrideDataKeys) {
        sourceOverrideDataMap = new HashMap<>();
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
     */
    /*
     * 2012-04-18: Old version: keep for reference: Gang Zhang private boolean
     * isOverride(String key, String param) { if (sourceOverrideDataMap != null)
     * { if (sourceOverrideDataMap.containsKey(key)) { if
     * (sourceOverrideDataMap.get(key).contains(param)) { return true; } } }
     * 
     * return false; }
     */
    /**
     * 2012-04-18: code is from David Hladky in Omaha for DR 14619/14620. Gang
     * Zhang is checked in Check to see if we are overridden
     * 
     * @param key
     * @param param
     * @return
     */
    private boolean isOverride(String key, String param) {
        if (sourceOverrideDataMap != null) {
            if (sourceOverrideDataMap.containsKey(key)) {
                for (SourceOverrideParamXML paramx : sourceOverrideDataMap
                        .get(key)) {
                    if (paramx.getParam().equals(param)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Sources that have common backing grids can set this so that the source
     * bins used for processing only have to be produced once. This saves time
     * and CPU/memory
     * 
     * @return
     */
    public String getSourceFamily() {
        return sourceFamily;
    }

    /**
     * Set the sourceFamily
     * 
     * @param sourceFamily
     */
    public void setSourceFamily(String sourceFamily) {
        this.sourceFamily = sourceFamily;
    }

    @Override
    public String toString() {
        return "SourceXML [sourceName=" + sourceName + ", displayName="
                + displayName + ", durationHour=" + durationHour + ", dataPath="
                + dataPath + ", plugin=" + plugin + ", pluginClass="
                + pluginClass + ", pluginDAOClass=" + pluginDAOClass
                + ", dataType=" + dataType + ", sourceType=" + sourceType
                + ", rateOrAccum=" + rateOrAccum + ", expirationMinutes="
                + expirationMinutes + ", interpolatedGuidanceDelay="
                + interpolatedGuidanceDelay
                + ", interpolatedGuidanceTransition="
                + interpolatedGuidanceTransition + ", sourceFamily="
                + sourceFamily + ", unit=" + unit + ", conversion=" + conversion
                + ", mosaic=" + mosaic + ", guidanceType=" + guidanceType
                + ", dateFormat=" + dateFormat + ", hrapGridFactor="
                + hrapGridFactor + ", isOverride=" + isOverride
                + ", sourceOverrideDataMap=" + sourceOverrideDataMap + "]";
    }

}
