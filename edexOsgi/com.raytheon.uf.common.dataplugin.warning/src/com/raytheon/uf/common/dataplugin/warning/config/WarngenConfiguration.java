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
package com.raytheon.uf.common.dataplugin.warning.config;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.Unit;
import javax.measure.format.ParserException;
import javax.measure.quantity.Length;
import javax.measure.quantity.Speed;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration.AreaType;
import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

import systems.uom.common.USCustomary;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * WarngenConfiguration
 *
 * <pre>
 *
 *    SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Nov 21, 2007           chammack     Initial Creation.
 * Aug 26, 2008  1502     bclement     Added JAXB annotations
 * May 26, 2010  4649     Qinglu Lin   Made including TO.A and SV.A mandatory
 * Apr 24, 2013  1943     jsanchez     Marked areaConfig as Deprecated.
 * Oct 22, 2013  2361     njensen      Removed ISerializableObject
 * Apr 28, 2014  3033     jsanchez     Properly handled back up configuration
 *                                     (*.xml) files.
 * Aug 28, 2014  15658    D. Friedman  Add marine zone watch wording option.
 * Dec 21, 2015  17942    D. Friedman  Add extension area specification.
 * Mar 10, 2015  5411     randerso     Added productId to configuration
 * Aug 29, 2017  6328     randerso     Convert to use PresetInfoBullet
 *
 * </pre>
 *
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "warngenConfig")
public class WarngenConfiguration {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WarngenConfiguration.class);

    private static final Pattern p = Pattern
            .compile("<\\s{0,}include\\s{1,}file\\s{0,}=\\s{0,}\"(.*)\"\\s{0,}/>");

    private final JAXBManager jaxb;

    @XmlElement
    private GeospatialConfiguration geospatialConfig;

    @XmlElement
    private PathcastConfiguration pathcastConfig;

    private AreaSourceConfiguration hatchedAreaSource;

    @XmlElement
    @Deprecated
    private AreaConfiguration areaConfig;

    @XmlElementWrapper(name = "bulletActionGroups")
    @XmlElement(name = "bulletActionGroup")
    private BulletActionGroup[] bulletActionGroups;

    private Bullet[] bullets;

    /* TODO: remove in later release */
    @Deprecated
    private PresetInfoBullet[] presetInfoBullets;

    @XmlElementWrapper(name = "includedWatches")
    @XmlElement(name = "includedWatch")
    private String[] includedWatches;

    @XmlElement
    private boolean includeMarineAreasInWatches;

    @XmlElementWrapper(name = "durations")
    @XmlElement(name = "duration")
    private int[] durations;

    @XmlElementWrapper(name = "followups")
    @XmlElement(name = "followup")
    private String[] followUps;

    @XmlElementWrapper(name = "maps")
    @XmlElement(name = "map")
    private String[] maps = new String[0];

    private Unit<Length> unitDistance = USCustomary.MILE;

    private Unit<Speed> unitSpeed = USCustomary.MILE_PER_HOUR;

    @XmlElement
    private boolean trackEnabled = true;

    @XmlElement
    private int defaultDuration;

    @XmlElement
    private boolean enableDuration = true;

    @XmlElement
    private boolean enableRestart;

    @XmlElement
    private boolean enablePresetThreat;

    @XmlElement
    private boolean autoLockText;

    @XmlElement
    private String productId;

    @XmlElementWrapper(name = "phensigs")
    @XmlElement(name = "phensig")
    private String[] phensigs;

    @XmlElement(name = "pointSource")
    private PointSourceConfiguration[] pointSources;

    @XmlElement(name = "areaSource")
    private AreaSourceConfiguration[] areaSources;

    @XmlElement
    private String lockedGroupsOnFollowup;

    @XmlElement
    private ExtensionArea extensionArea;

    /**
     * Method used to load a configuration file for a newly selected Warngen
     * template.
     *
     * @param templateName
     *            - the name of the warngen template
     * @param localSite
     *            - the site cave is localized to
     * @param localSite
     *            - the back up site
     * @return the warngen configuration
     * @throws VizException
     */
    public static WarngenConfiguration loadConfig(String templateName,
            String localSite, String backupSite) throws FileNotFoundException,
    IOException, JAXBException {
        WarngenConfiguration config = new WarngenConfiguration();
        // Open the template file
        String xml = WarnFileUtil.convertFileContentsToString(templateName
                + ".xml", localSite, backupSite);

        // Include external files, such as damInfo.txt
        Matcher m = p.matcher(xml);
        String includeFile = null;
        try {
            while (m.find()) {
                includeFile = m.group(1);
                String includeXml = WarnFileUtil.convertFileContentsToString(
                        includeFile, localSite, backupSite);
                xml = xml.replace(m.group(0), includeXml);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred trying to include " + includeFile
                    + " in template " + templateName, e);
        }

        /*
         * TODO: remove in later release
         *
         * Convert damInfoBullets to presetInfoBullets for backward
         * compatibility This can eventually be removed when 18.1.1 is fully
         * deployed and templates have all been updated to use presetInfoBullets
         */
        String xmlOrig = xml;
        xml = xml.replaceAll("damInfoBullet", "presetInfoBullet");
        if (!xmlOrig.equals(xml)) {
            statusHandler.handle(Priority.EVENTB, templateName
                    + " contains deprecated <damInfoBullet> tags. These should be changed to <presetInfoBullet>.");
        }

        xmlOrig = xml;
        xml = xml.replaceAll("enableDamBreakThreat", "enablePresetThreat");
        if (!xmlOrig.equals(xml)) {
            statusHandler.handle(Priority.EVENTB, templateName
                    + " contains deprecated <enableDamBreakThreat> tags. These should be changed to <enablePresetThreat>.");
        }

        xmlOrig = xml;
        m = Pattern.compile("(?i)(DAM)(?: BREAK)?( THREAT AREA BUTTON)")
                .matcher(xml);
        while (m.find()) {
            if ("DAM".equals(m.group(1))) {
                xml = m.replaceFirst("PRESET$2");
            } else if ("dam".equals(m.group(1))) {
                xml = m.replaceFirst("preset$2");
            } else {
                /*
                 * either initial capital or some non-standard capitalization,
                 * just replace with initial capital
                 */
                xml = m.replaceFirst("Preset$2");
            }
        }
        if (!xmlOrig.equals(xml)) {
            statusHandler.handle(Priority.EVENTB, templateName
                    + " contains references to the deprecated Dam Break Threat Area button. These should be changed to Preset Threat Area");
        }


        config = (WarngenConfiguration) config.jaxb.unmarshalFromXml(xml);

        // Sets the lists of bullets and preset bullets
        if (config.getBulletActionGroups() != null) {
            for (BulletActionGroup group : config.getBulletActionGroups()) {
                if (group.getAction() == null
                        || WarningAction.valueOf(group.getAction()) == WarningAction.NEW) {
                    config.setBullets(group.getBullets());

                    /* TODO: remove in later release */
                    config.setPresetInfoBullets(group.getPresetInfoBullets());
                    break;
                }
            }
        }

        PointSourceConfiguration pscs[] = config.getPointSources();

        if (pscs != null) {
            for (PointSourceConfiguration psc : pscs) {
                if (psc.getPointSource() == null) {
                    psc.setPointSource(config.getGeospatialConfig()
                            .getPointSource());
                }
            }
        }

        // AreaConfiguration is deprecated. This is only meant for backwards
        // compatibility while areaConfig is phased out with updated templates
        // from the template team.
        if (config.getAreaConfig() != null) {
            ArrayList<AreaSourceConfiguration> areaSources = null;

            if (config.getAreaSources() == null) {
                areaSources = new ArrayList<>();
            } else {
                areaSources = new ArrayList<>(
                        Arrays.asList(config.getAreaSources()));
            }
            areaSources
            .add(new AreaSourceConfiguration(config.getAreaConfig()));
            config.setAreaSources(areaSources
                    .toArray(new AreaSourceConfiguration[areaSources.size()]));
        }

        for (AreaSourceConfiguration asc : config.getAreaSources()) {
            if (asc.getAreaSource() == null) {
                asc.setAreaSource(config.getGeospatialConfig().getAreaSource());
            }

            if (asc.getType() == AreaType.HATCHING) {
                config.setHatchedAreaSource(asc);
            }
        }

        if (config.getPathcastConfig() != null
                && config.getPathcastConfig().getPointSource() == null) {
            config.getPathcastConfig().setPointSource(
                    config.getGeospatialConfig().getPointSource());
        }

        return config;
    }

    /**
     * Constructor
     *
     * @throws JAXBException
     */
    public WarngenConfiguration() throws JAXBException {
        jaxb = new JAXBManager(WarngenConfiguration.class, Bullet.class,
                PresetInfoBullet.class);
    }

    public String[] getMaps() {
        return maps;
    }

    public void setMaps(String[] maps) {
        this.maps = maps;
    }

    /**
     * This method is depreciated. Please use AreaSourceConfiguration. The type
     * AreaType.HATCHING will determine which area source is for hatching.
     *
     * @return the areaConfig
     */
    @Deprecated
    public AreaConfiguration getAreaConfig() {
        return areaConfig;
    }

    /**
     * @param areaConfig
     *            the areaConfig to set
     */
    @Deprecated
    public void setAreaConfig(AreaConfiguration areaConfig) {
        this.areaConfig = areaConfig;
    }

    /**
     * @return the geospatialConfig
     */
    public GeospatialConfiguration getGeospatialConfig() {
        return geospatialConfig;
    }

    /**
     * @param geospatialConfig
     *            the geospatialConfig to set
     */
    public void setGeospatialConfig(GeospatialConfiguration geospatialConfig) {
        this.geospatialConfig = geospatialConfig;
    }

    /**
     * @return the pathcastConfig
     */
    public PathcastConfiguration getPathcastConfig() {
        return pathcastConfig;
    }

    /**
     * @param pathcastConfig
     *            the pathcastConfig to set
     */
    public void setPathcastConfig(PathcastConfiguration pathcastConfig) {
        this.pathcastConfig = pathcastConfig;
    }

    /**
     * @return the bullets
     */
    public Bullet[] getBullets() {
        return bullets;
    }

    /**
     * @param bullets
     *            the bullets to set
     */
    public void setBullets(Bullet[] bullets) {
        this.bullets = bullets;
    }

    public BulletActionGroup[] getBulletActionGroups() {
        return bulletActionGroups;
    }

    public void setBulletActionGroups(BulletActionGroup[] bulletActionGroups) {
        this.bulletActionGroups = bulletActionGroups;
    }

    /* TODO: remove in later release */
    @Deprecated
    public PresetInfoBullet[] getPresetInfoBullets() {
        return presetInfoBullets;
    }

    /* TODO: remove in later release */
    @Deprecated
    public void setPresetInfoBullets(PresetInfoBullet[] presetInfoBullets) {
        this.presetInfoBullets = presetInfoBullets;
    }

    /**
     * @return the durations
import org.geotools.xs.facets.Length;

     */
    public int[] getDurations() {
        return durations;
    }

    /**
     * @param durations
     *            the durations to set
     */
    public void setDurations(int[] durations) {
        this.durations = durations;
    }

    public int getDefaultDuration() {
        return defaultDuration;
    }

    public void setDefaultDuration(int defaultDuration) {
        this.defaultDuration = defaultDuration;
    }

    public String[] getFollowUps() {
        return followUps;
    }

    public void setFollowUps(String[] followUps) {
        this.followUps = followUps;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @param phensigs
     *            the phensigs to set
     */
    public void setPhensigs(String[] phensigs) {
        this.phensigs = phensigs;
    }

    /**
     * @return the phensigs
     */
    public String[] getPhensigs() {
        return phensigs;
    }

    public void setIncludedWatches(String[] includedWatches) {
        this.includedWatches = includedWatches;
    }

    public String[] getIncludedWatches() {
        return includedWatches;
    }

    public boolean isIncludeMarineAreasInWatches() {
        return includeMarineAreasInWatches;
    }

    public void setIncludeMarineAreasInWatches(
            boolean includeMarineAreasInWatches) {
        this.includeMarineAreasInWatches = includeMarineAreasInWatches;
    }

    public boolean getEnableRestart() {
        return enableRestart;
    }

    public void setEnableRestart(boolean enableRestart) {
        this.enableRestart = enableRestart;
    }

    public boolean getEnablePresetThreat() {
        return enablePresetThreat;
    }

    public void setEnablePresetThreat(boolean enablePresetThreat) {
        this.enablePresetThreat = enablePresetThreat;
    }

    public boolean getAutoLockText() {
        return autoLockText;
    }

    public void setAutoLockText(boolean autoLockText) {
        this.autoLockText = autoLockText;
    }

    public Unit<Length> getUnitDistance() {
        return unitDistance;
    }

    public void setUnitDistance(Unit<Length> unitDistance) {
        this.unitDistance = unitDistance;
    }

    public Unit<Speed> getUnitSpeed() {
        return unitSpeed;
    }

    public void setUnitSpeed(Unit<Speed> unitSpeed) {
        this.unitSpeed = unitSpeed;
    }

    public String setUnitDistanceString() {
        return SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(this.unitDistance);
    }

    @XmlElement(name = "unitDistance")
    public void setUnitDistanceString(String unitDistance) {
        try {
            this.unitDistance = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).parseProductUnit(
                    unitDistance, new ParsePosition(0)).asType(Length.class);
        } catch (ParserException e) {
            throw new RuntimeException("Error setting unit speed: "
                    + e.getLocalizedMessage(), e);
        }
    }

    public String getUnitSpeedString() {
        return SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(this.unitSpeed);
    }

    @XmlElement(name = "unitSpeed")
    public void setUnitSpeedString(String unitSpeed) {
        try {
            this.unitSpeed = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).parseProductUnit(
                    unitSpeed, new ParsePosition(0)).asType(Speed.class);
        } catch (ParserException e) {
            throw new RuntimeException("Error setting unit speed: "
                    + e.getLocalizedMessage(), e);
        }
    }

    public PointSourceConfiguration[] getPointSources() {
        return pointSources;
    }

    public void setPointSources(PointSourceConfiguration[] pointSources) {
        this.pointSources = pointSources;
    }

    public AreaSourceConfiguration[] getAreaSources() {
        return areaSources;
    }

    public void setAreaSources(AreaSourceConfiguration[] areaSources) {
        this.areaSources = areaSources;
    }

    public boolean isTrackEnabled() {
        return trackEnabled;
    }

    public void setTrackEnabled(boolean trackEnabled) {
        this.trackEnabled = trackEnabled;
    }

    public String getLockedGroupsOnFollowup() {
        return lockedGroupsOnFollowup;
    }

    public void setLockedGroupsOnFollowup(String lockedGroupsOnFollowup) {
        this.lockedGroupsOnFollowup = lockedGroupsOnFollowup;
    }

    public boolean isEnableDuration() {
        return enableDuration;
    }

    public void setEnableDuration(boolean enableDuration) {
        this.enableDuration = enableDuration;
    }

    public AreaSourceConfiguration getHatchedAreaSource() {
        return hatchedAreaSource;
    }

    public void setHatchedAreaSource(AreaSourceConfiguration hatchedAreaSource) {
        this.hatchedAreaSource = hatchedAreaSource;
    }

    public ExtensionArea getExtensionArea() {
        return extensionArea;
    }

    public void setExtensionArea(ExtensionArea extensionArea) {
        this.extensionArea = extensionArea;
    }

}
