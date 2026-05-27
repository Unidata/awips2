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
package com.raytheon.uf.common.dataplugin.modelsounding;

import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import jakarta.persistence.Access;
import jakarta.persistence.AccessType;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.Index;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.UniqueConstraint;

import org.locationtech.jts.geom.Geometry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * The SoundingSite class encapsulates the location and time information for a
 * model sounding forecast as well as providing a container for the vertical
 * level data above the location.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Apr 04, 2013  1846     bkowal      Added an index on refTime and
 *                                    forecastTime
 * Apr 12, 2013  1857     bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen    Remove dataURI column from
 *                                    PluginDataObject.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Dec 02, 2013  2537     bsteffen    Move to common, remove IDecoderGettable,
 *                                    remove unnecessary fields.
 * Jul 27, 2015  4360     rferrel     Named unique constraint. Made reportType non-nullable.
 * Aug 08, 2022  8892     tjensen     Update indexes for Hibernate 5
 *
 * </pre>
 *
 * @author jkorman
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "modelsoundingseq")
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@Table(name = "modelsounding", uniqueConstraints = {
        @UniqueConstraint(name = "uk_modelsounding_datauri_fields", columnNames = {
                "dataURI" }) }, indexes = {
                        @Index(name = "%TABLE%_refTimeIndex", columnList = "refTime, forecastTime"),
                        @Index(name = "%TABLE%_stationIndex", columnList = "stationId") })

@DynamicSerialize
public class SoundingSite extends PersistablePluginDataObject
        implements ISpatialEnabled, IPointData, IPersistable {

    public static final String PLUGIN_ID = "modelsounding";

    private static final long serialVersionUID = 1L;

    // These site ids are not strictly ICAO ids!
    @Column
    @DynamicSerializeElement
    private String siteId;

    @DataURI(position = 1)
    @NullString
    @Column(nullable = false)
    @DynamicSerializeElement
    private String reportType;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @Column
    @DynamicSerializeElement
    private String wmoHeader;

    // the level data
    @Transient
    private Set<SoundingLevel> levels;

    /**
     * Create an empty ProfilerObs object.
     */
    public SoundingSite() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     *
     * @param uri
     *            A data uri applicable to this class.
     * @param tableDef
     *            The table definitions for this class.
     */
    public SoundingSite(String uri) {
        super(uri);
    }

    /**
     * Get the observation time for this data.
     *
     * @return The data observation time.
     */
    public Calendar getTimeObs() {
        return dataTime.getRefTimeAsCalendar();
    }

    /**
     * @return the fcstSeconds
     */
    public Long getFcstSeconds() {
        return (long) dataTime.getFcstTime();
    }

    /**
     * Get this observation's geometry.
     *
     * @return The geometry for this observation.
     */
    public Geometry getGeometry() {
        return location.getGeometry();
    }

    /**
     * Get the geometry latitude.
     *
     * @return The geometry latitude.
     */
    public double getLatitude() {
        return location.getLatitude();
    }

    /**
     * Get the geometry longitude.
     *
     * @return The geometry longitude.
     */
    public double getLongitude() {
        return location.getLongitude();
    }

    /**
     * Get the station identifier for this observation.
     *
     * @return the stationId
     */
    public String getStationId() {
        return location.getStationId();
    }

    /**
     * Get the elevation, in meters, of the observing platform or location.
     *
     * @return The observation elevation, in meters.
     */
    public Integer getElevation() {
        return location.getElevation();
    }

    /**
     * Was this location defined from the station catalog? False if not.
     *
     * @return Was this location defined from the station catalog?
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
    }

    /**
     * Set the WMOHeader of the file that contained this data.
     *
     * @return The wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Get the WMOHeader of the file that contained this data.
     *
     * @param wmoHeader
     *            The WMOHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    private void populateLevels() {
        if (levels == null) {
            int count = pointDataView
                    .getInt(ModelSoundingParameters.NUM_LEVELS);
            if (count < 0) {
                count = 0;
            }
            levels = new HashSet<>(count, 1.0f);
            for (int i = 0; i < count; i += 1) {
                levels.add(new SoundingLevel(pointDataView, i));
            }
        }
    }

    public SoundingLevel addLevel() {
        populateLevels();
        SoundingLevel level = new SoundingLevel(pointDataView, levels.size());
        levels.add(level);
        pointDataView.setInt(ModelSoundingParameters.NUM_LEVELS, levels.size());
        return level;
    }

    /**
     * Get all levels contained by this object.
     *
     * @return the levels
     */
    public Set<SoundingLevel> getLevels() {
        populateLevels();
        return Collections.unmodifiableSet(levels);
    }

    /**
     * @return the siteId
     */
    public String getSiteId() {
        return siteId;
    }

    /**
     * @param siteId
     *            the siteId to set
     */
    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    /**
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /** @return the pressSLP */
    public float getPressSLP() {
        return pointDataView.getFloat(ModelSoundingParameters.SEA_LEVEL_PRESS);
    }

    /**
     * @param pressSLP
     *            the pressSLP to set
     */
    public void setPressSLP(float pressSLP) {
        pointDataView.setFloat(ModelSoundingParameters.SEA_LEVEL_PRESS,
                pressSLP);
    }

    /** @return the pressSfc */
    public float getPressSfc() {
        return pointDataView.getFloat(ModelSoundingParameters.SFC_PRESS);
    }

    /**
     * @param pressSfc
     *            the pressSfc to set
     */
    public void setPressSfc(float pressSfc) {
        pointDataView.setFloat(ModelSoundingParameters.SFC_PRESS, pressSfc);
    }

    /** @return the cldAmtLo */
    public float getCldAmtLo() {
        return pointDataView.getFloat(ModelSoundingParameters.LOW_CLOUD);
    }

    /**
     * @param cldAmtLo
     *            the cldAmtLo to set
     */
    public void setCldAmtLo(float cldAmtLo) {
        pointDataView.setFloat(ModelSoundingParameters.LOW_CLOUD, cldAmtLo);
    }

    /** @return the cldAmtMd */
    public float getCldAmtMd() {
        return pointDataView.getFloat(ModelSoundingParameters.MID_CLOUD);
    }

    /**
     * @param cldAmtMd
     *            the cldAmtMd to set
     */
    public void setCldAmtMd(float cldAmtMd) {
        pointDataView.setFloat(ModelSoundingParameters.MID_CLOUD, cldAmtMd);
    }

    /** @return the cldAmtHi */
    public float getCldAmtHi() {
        return pointDataView.getFloat(ModelSoundingParameters.HIGH_CLOUD);
    }

    /**
     * @param cldAmtHi
     *            the cldAmtHi to set
     */
    public void setCldAmtHi(float cldAmtHi) {
        pointDataView.setFloat(ModelSoundingParameters.HIGH_CLOUD, cldAmtHi);
    }

    /** @return the pressCldBase */
    public float getPressCldBase() {
        return pointDataView.getFloat(ModelSoundingParameters.CLOUD_PRESS);
    }

    /**
     * @param pressCldBase
     *            the pressCldBase to set
     */
    public void setPressCldBase(float pressCldBase) {
        pointDataView.setFloat(ModelSoundingParameters.CLOUD_PRESS,
                pressCldBase);
    }

    /** @return the uc10Meter */
    public float getUc10M() {
        return pointDataView.getFloat(ModelSoundingParameters.U_COMP_10M);
    }

    /**
     * @param uc10Meter
     *            the uc10Meter to set
     */
    public void setUc10M(float uc10M) {
        pointDataView.setFloat(ModelSoundingParameters.U_COMP_10M, uc10M);
    }

    /** @return the vc10M */
    public float getVc10M() {
        return pointDataView.getFloat(ModelSoundingParameters.V_COMP_10M);
    }

    /**
     * @param vc10M
     *            the vc10M to set
     */
    public void setVc10M(float vc10M) {
        pointDataView.setFloat(ModelSoundingParameters.V_COMP_10M, vc10M);
    }

    /** @return the sensHeat */
    public float getSensHeat() {
        return pointDataView.getFloat(ModelSoundingParameters.SENS_HEAT);
    }

    /**
     * @param sensHeat
     *            the sensHeat to set
     */
    public void setSensHeat(float sensHeat) {
        pointDataView.setFloat(ModelSoundingParameters.SENS_HEAT, sensHeat);
    }

    /** @return the subSfcHeat */
    public float getSubSfcHeat() {
        return pointDataView.getFloat(ModelSoundingParameters.SUB_SFC_HEAT);
    }

    /**
     * @param subSfcHeat
     *            the subSfcHeat to set
     */
    public void setSubSfcHeat(float subSfcHeat) {
        pointDataView.setFloat(ModelSoundingParameters.SUB_SFC_HEAT,
                subSfcHeat);
    }

    /** @return the minTemp */
    public float getMinTemp() {
        return pointDataView.getFloat(ModelSoundingParameters.MIN_TEMP);
    }

    /**
     * @param minTemp
     *            the minTemp to set
     */
    public void setMinTemp(float minTemp) {
        pointDataView.setFloat(ModelSoundingParameters.MIN_TEMP, minTemp);
    }

    /** @return the maxTemp */
    public float getMaxTemp() {
        return pointDataView.getFloat(ModelSoundingParameters.MAX_TEMP);
    }

    /**
     * @param maxTemp
     *            the maxTemp to set
     */
    public void setMaxTemp(float maxTemp) {
        pointDataView.setFloat(ModelSoundingParameters.MAX_TEMP, maxTemp);
    }

    /** @return the skinTemp */
    public float getSkinTemp() {
        return pointDataView.getFloat(ModelSoundingParameters.SKIN_TEMP);
    }

    /**
     * @param skinTemp
     *            the skinTemp to set
     */
    public void setSkinTemp(float skinTemp) {
        pointDataView.setFloat(ModelSoundingParameters.SKIN_TEMP, skinTemp);
    }

    /** @return the temp2M */
    public float getTemp2M() {
        return pointDataView.getFloat(ModelSoundingParameters.TEMP_2M);
    }

    /**
     * @param temp2M
     *            the temp2M to set
     */
    public void setTemp2M(float temp2M) {
        pointDataView.setFloat(ModelSoundingParameters.TEMP_2M, temp2M);
    }

    /** @return the specHum2M */
    public float getSpecHum2M() {
        return pointDataView.getFloat(ModelSoundingParameters.SPEC_HUM_2M);
    }

    /**
     * @param specHum2M
     *            the specHum2M to set
     */
    public void setSpecHum2M(float specHum2M) {
        pointDataView.setFloat(ModelSoundingParameters.SPEC_HUM_2M, specHum2M);
    }

    /** @return the specHum10M */
    public float getSpecHum10M() {
        return pointDataView.getFloat(ModelSoundingParameters.SPEC_HUM_10M);
    }

    /**
     * @param specHum10M
     *            the specHum10M to set
     */
    public void setSpecHum10M(float specHum10M) {
        pointDataView.setFloat(ModelSoundingParameters.SPEC_HUM_10M,
                specHum10M);
    }

    /** @return the theta10M */
    public float getTheta10M() {
        return pointDataView.getFloat(ModelSoundingParameters.THETA_10M);
    }

    /**
     * @param theta10M
     *            the theta10M to set
     */
    public void setTheta10M(float theta10M) {
        pointDataView.setFloat(ModelSoundingParameters.THETA_10M, theta10M);
    }

    /** @return the snowType */
    public int getSnowType() {
        return pointDataView.getInt(ModelSoundingParameters.SNOW_TYPE);
    }

    /**
     * @param snowType
     *            the snowType to set
     */
    public void setSnowType(int snowType) {
        pointDataView.setInt(ModelSoundingParameters.SNOW_TYPE, snowType);
    }

    /** @return the iceType */
    public int getIceType() {
        return pointDataView.getInt(ModelSoundingParameters.ICE_TYPE);
    }

    /**
     * @param iceType
     *            the iceType to set
     */
    public void setIceType(int iceType) {
        pointDataView.setInt(ModelSoundingParameters.ICE_TYPE, iceType);
    }

    /** @return the fzRainType */
    public int getFzRainType() {
        return pointDataView.getInt(ModelSoundingParameters.FREEZING_RAIN_TYPE);
    }

    /**
     * @param fzRainType
     *            the fzRainType to set
     */
    public void setFzRainType(int fzRainType) {
        pointDataView.setInt(ModelSoundingParameters.FREEZING_RAIN_TYPE,
                fzRainType);
    }

    /** @return the rainType */
    public int getRainType() {
        return pointDataView.getInt(ModelSoundingParameters.RAIN_TYPE);
    }

    /**
     * @param rainType
     *            the rainType to set
     */
    public void setRainType(int rainType) {
        pointDataView.setInt(ModelSoundingParameters.RAIN_TYPE, rainType);
    }

    /** @return the horzVis */
    public float getHorzVis() {
        return pointDataView.getFloat(ModelSoundingParameters.VISIBILITY);
    }

    /**
     * @param horzVis
     *            the horzVis to set
     */
    public void setHorzVis(float horzVis) {
        pointDataView.setFloat(ModelSoundingParameters.VISIBILITY, horzVis);
    }

    /** @return the stormUComp */
    public float getStormUComp() {
        return pointDataView.getFloat(ModelSoundingParameters.U_STORM);
    }

    /**
     * @param stormUComp
     *            the stormUComp to set
     */
    public void setStormUComp(float stormUComp) {
        pointDataView.setFloat(ModelSoundingParameters.U_STORM, stormUComp);
    }

    /** @return the stormVComp */
    public float getStormVComp() {
        return pointDataView.getFloat(ModelSoundingParameters.V_STORM);
    }

    /**
     * @param stormVComp
     *            the stormVComp to set
     */
    public void setStormVComp(float stormVComp) {
        pointDataView.setFloat(ModelSoundingParameters.V_STORM, stormVComp);
    }

    /** @return the stormRelHeli */
    public float getStormRelHeli() {
        return pointDataView.getFloat(ModelSoundingParameters.STORM_REL_HELI);
    }

    /**
     * @param stormRelHeli
     *            the stormRelHeli to set
     */
    public void setStormRelHeli(float stormRelHeli) {
        pointDataView.setFloat(ModelSoundingParameters.STORM_REL_HELI,
                stormRelHeli);
    }

    /** @return the totPrecip */
    public float getTotPrecip() {
        return pointDataView.getFloat(ModelSoundingParameters.TOTAL_PRECIP);
    }

    /**
     * @param totPrecip
     *            the totPrecip to set
     */
    public void setTotPrecip(float totPrecip) {
        pointDataView.setFloat(ModelSoundingParameters.TOTAL_PRECIP, totPrecip);
    }

    /** @return the precipConv */
    public float getPrecipConv() {
        return pointDataView.getFloat(ModelSoundingParameters.CONV_PRECIP);
    }

    /**
     * @param precipConv
     *            the precipConv to set
     */
    public void setPrecipConv(float precipConv) {
        pointDataView.setFloat(ModelSoundingParameters.CONV_PRECIP, precipConv);
    }

    /** @return the snowWaterEquiv */
    public float getSnowWaterEquiv() {
        return pointDataView.getFloat(ModelSoundingParameters.SNOW_WATER);
    }

    /**
     * @param snowWaterEquiv
     *            the snowWaterEquiv to set
     */
    public void setSnowWaterEquiv(float snowWaterEquiv) {
        pointDataView.setFloat(ModelSoundingParameters.SNOW_WATER,
                snowWaterEquiv);
    }

    /** @return the snowFall */
    public float getSnowFall() {
        return pointDataView.getFloat(ModelSoundingParameters.SNOW_FALL);
    }

    /**
     * @param snowFall
     *            the snowFall to set
     */
    public void setSnowFall(float snowFall) {
        pointDataView.setFloat(ModelSoundingParameters.SNOW_FALL, snowFall);
    }

    /**
     * @param snowMelt
     *            the snowMelt to set
     */
    public void setSnowMelt(float snowMelt) {
        pointDataView.setFloat(ModelSoundingParameters.SNOW_MELT, snowMelt);
    }

    /** @return the snowFlux */
    public float getSnowMFlux() {
        return pointDataView.getFloat(ModelSoundingParameters.SNOW_FLUX);
    }

    /**
     * @param snowFlux
     *            the snowFlux to set
     */
    public void setSnowFlux(float snowFlux) {
        pointDataView.setFloat(ModelSoundingParameters.SNOW_FLUX, snowFlux);
    }

    /** @return the snowMelt */
    public float getSnowMelt() {
        return pointDataView.getFloat(ModelSoundingParameters.SNOW_MELT);
    }

    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    public SurfaceObsLocation getLocation() {
        return location;
    }

    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    @Override
    public PointDataView getPointDataView() {
        return this.pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return PLUGIN_ID;
    }
}
