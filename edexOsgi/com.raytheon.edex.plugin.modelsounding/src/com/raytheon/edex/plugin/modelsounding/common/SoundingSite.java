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
package com.raytheon.edex.plugin.modelsounding.common;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * The SoundingSite class encapsulates the location and time information for a
 * model sounding forecast as well as providing a container for the vertical
 * level data above the location.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  1026     jkorman     Initial implementation.
 * Apr 04, 2013  1846     bkowal      Added an index on refTime and
 *                                    forecastTime
 * Apr 12, 2013  1857     bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013  1869     bsteffen    Remove dataURI column from
 *                                    PluginDataObject.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Dec 02, 2013  2537     bsteffen    Remove IDecoderGettable
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "modelsoundingseq")
@Table(name = "modelsounding", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "modelsounding", indexes = { @Index(name = "modelsounding_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class SoundingSite extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    // The profiler observation time.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Calendar timeObs;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private Long fcstSeconds;

    // These site ids are not strictly ICAO ids!
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String siteId;

    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String reportType;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @Column
    @DynamicSerializeElement
    @XmlElement
    private String wmoHeader;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer pressSLP;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer pressSfc;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double precipTot3Hr;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double precipConv3Hr;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer cldAmtLo;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer cldAmtMd;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer cldAmtHi;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer pressCldBase;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double uc10M;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double vc10M;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double temp2M;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double specHum2M;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer snowType;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer iceType;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer fzRainType;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Integer rainType;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double horzVis;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double stormUComp;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double stormVComp;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double stormRelHeli;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double totPrecip1Hr;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double precipConv1Hr;

    @DynamicSerializeElement
    @XmlElement
    @Transient
    private Double snowWaterEquiv;

    // the level data
    @Transient
    private Set<SoundingLevel> soundingLevels;

    // the level data
    @Transient
    private Set<SoilLevel> soilLevels;

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
        return timeObs;
    }

    /**
     * Set the observation time for this data.
     * 
     * @param timeObs
     *            The data observation time.
     */
    public void setTimeObs(Calendar timeObs) {
        this.timeObs = timeObs;
    }

    /**
     * @return the fcstSeconds
     */
    public Long getFcstSeconds() {
        return fcstSeconds;
    }

    /**
     * @param fcstSeconds
     *            the fcstSeconds to set
     */
    public void setFcstSeconds(Long fcstSeconds) {
        this.fcstSeconds = fcstSeconds;
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

    /**
     * 
     * @param level
     *            A sounding data level to add.
     */
    public void addLevel(SoundingLevel level) {
        if (soundingLevels == null) {
            soundingLevels = new HashSet<SoundingLevel>();
        }
        soundingLevels.add(level);
    }

    /**
     * Get all levels contained by this object.
     * 
     * @return the levels
     */
    public Set<SoundingLevel> getLevels() {
        return soundingLevels;
    }

    /**
     * Set the level data into this object.
     * 
     * @param levels
     *            the levels to set
     */
    public void setLevels(Set<SoundingLevel> levels) {
        soundingLevels = levels;
    }

    /**
     * 
     * @param level
     *            A soil data level to add.
     */
    public void addSoilLevel(SoilLevel level) {
        if (soilLevels == null) {
            soilLevels = new HashSet<SoilLevel>();
        }
        soilLevels.add(level);
    }

    /**
     * @return the soilLevels
     */
    public Set<SoilLevel> getSoilLevels() {
        return soilLevels;
    }

    /**
     * @param soilLevels
     *            the soilLevels to set
     */
    public void setSoilLevels(Set<SoilLevel> levels) {
        soilLevels = levels;
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

    /**
     * @return the soundingLevels
     */
    public Set<SoundingLevel> getSoundingLevels() {
        return soundingLevels;
    }

    /**
     * @param soundingLevels
     *            the soundingLevels to set
     */
    public void setSoundingLevels(Set<SoundingLevel> soundingLevels) {
        this.soundingLevels = soundingLevels;
    }

    /**
     * @return the pressSLP
     */
    public Integer getPressSLP() {
        return pressSLP;
    }

    /**
     * @param pressSLP
     *            the pressSLP to set
     */
    public void setPressSLP(Integer pressSLP) {
        this.pressSLP = pressSLP;
    }

    /**
     * @return the pressSfc
     */
    public Integer getPressSfc() {
        return pressSfc;
    }

    /**
     * @param pressSfc
     *            the pressSfc to set
     */
    public void setPressSfc(Integer pressSfc) {
        this.pressSfc = pressSfc;
    }

    /**
     * @return the precipTot3Hr
     */
    public Double getPrecipTot3Hr() {
        return precipTot3Hr;
    }

    /**
     * @param precipTot3Hr
     *            the precipTot3Hr to set
     */
    public void setPrecipTot3Hr(Double precipTot3Hr) {
        this.precipTot3Hr = precipTot3Hr;
    }

    /**
     * @return the precipConv3Hr
     */
    public Double getPrecipConv3Hr() {
        return precipConv3Hr;
    }

    /**
     * @param precipConv3Hr
     *            the precipConv3Hr to set
     */
    public void setPrecipConv3Hr(Double precipConv3Hr) {
        this.precipConv3Hr = precipConv3Hr;
    }

    /**
     * @return the cldAmtLo
     */
    public Integer getCldAmtLo() {
        return cldAmtLo;
    }

    /**
     * @param cldAmtLo
     *            the cldAmtLo to set
     */
    public void setCldAmtLo(Integer cldAmtLo) {
        this.cldAmtLo = cldAmtLo;
    }

    /**
     * @return the cldAmtMd
     */
    public Integer getCldAmtMd() {
        return cldAmtMd;
    }

    /**
     * @param cldAmtMd
     *            the cldAmtMd to set
     */
    public void setCldAmtMd(Integer cldAmtMd) {
        this.cldAmtMd = cldAmtMd;
    }

    /**
     * @return the cldAmtHi
     */
    public Integer getCldAmtHi() {
        return cldAmtHi;
    }

    /**
     * @param cldAmtHi
     *            the cldAmtHi to set
     */
    public void setCldAmtHi(Integer cldAmtHi) {
        this.cldAmtHi = cldAmtHi;
    }

    /**
     * @return the pressCldBase
     */
    public Integer getPressCldBase() {
        return pressCldBase;
    }

    /**
     * @param pressCldBase
     *            the pressCldBase to set
     */
    public void setPressCldBase(Integer pressCldBase) {
        this.pressCldBase = pressCldBase;
    }

    /**
     * @return the uc10Meter
     */
    public Double getUc10M() {
        return uc10M;
    }

    /**
     * @param uc10Meter
     *            the uc10Meter to set
     */
    public void setUc10M(Double uc10Meter) {
        this.uc10M = uc10Meter;
    }

    /**
     * @return the vc10M
     */
    public Double getVc10M() {
        return vc10M;
    }

    /**
     * @param vc10M
     *            the vc10M to set
     */
    public void setVc10M(Double vc10M) {
        this.vc10M = vc10M;
    }

    /**
     * @return the temp2M
     */
    public Double getTemp2M() {
        return temp2M;
    }

    /**
     * @param temp2M
     *            the temp2M to set
     */
    public void setTemp2M(Double temp2M) {
        this.temp2M = temp2M;
    }

    /**
     * @return the specHum2M
     */
    public Double getSpecHum2M() {
        return specHum2M;
    }

    /**
     * @param specHum2M
     *            the specHum2M to set
     */
    public void setSpecHum2M(Double specHum2M) {
        this.specHum2M = specHum2M;
    }

    /**
     * @return the snowType
     */
    public Integer getSnowType() {
        return snowType;
    }

    /**
     * @param snowType
     *            the snowType to set
     */
    public void setSnowType(Integer snowType) {
        this.snowType = snowType;
    }

    /**
     * @return the iceType
     */
    public Integer getIceType() {
        return iceType;
    }

    /**
     * @param iceType
     *            the iceType to set
     */
    public void setIceType(Integer iceType) {
        this.iceType = iceType;
    }

    /**
     * @return the fzRainType
     */
    public Integer getFzRainType() {
        return fzRainType;
    }

    /**
     * @param fzRainType
     *            the fzRainType to set
     */
    public void setFzRainType(Integer fzRainType) {
        this.fzRainType = fzRainType;
    }

    /**
     * @return the rainType
     */
    public Integer getRainType() {
        return rainType;
    }

    /**
     * @param rainType
     *            the rainType to set
     */
    public void setRainType(Integer rainType) {
        this.rainType = rainType;
    }

    /**
     * @return the horzVis
     */
    public Double getHorzVis() {
        return horzVis;
    }

    /**
     * @param horzVis
     *            the horzVis to set
     */
    public void setHorzVis(Double horzVis) {
        this.horzVis = horzVis;
    }

    /**
     * @return the stormUComp
     */
    public Double getStormUComp() {
        return stormUComp;
    }

    /**
     * @param stormUComp
     *            the stormUComp to set
     */
    public void setStormUComp(Double stormUComp) {
        this.stormUComp = stormUComp;
    }

    /**
     * @return the stormVComp
     */
    public Double getStormVComp() {
        return stormVComp;
    }

    /**
     * @param stormVComp
     *            the stormVComp to set
     */
    public void setStormVComp(Double stormVComp) {
        this.stormVComp = stormVComp;
    }

    /**
     * @return the stormRelHeli
     */
    public Double getStormRelHeli() {
        return stormRelHeli;
    }

    /**
     * @param stormRelHeli
     *            the stormRelHeli to set
     */
    public void setStormRelHeli(Double stormRelHeli) {
        this.stormRelHeli = stormRelHeli;
    }

    /**
     * @return the totPrecip1Hr
     */
    public Double getTotPrecip1Hr() {
        return totPrecip1Hr;
    }

    /**
     * @param totPrecip1Hr
     *            the totPrecip1Hr to set
     */
    public void setTotPrecip1Hr(Double totPrecip1Hr) {
        this.totPrecip1Hr = totPrecip1Hr;
    }

    /**
     * @return the precipConv1Hr
     */
    public Double getPrecipConv1Hr() {
        return precipConv1Hr;
    }

    /**
     * @param precipConv1Hr
     *            the precipConv1Hr to set
     */
    public void setPrecipConv1Hr(Double precipConv1Hr) {
        this.precipConv1Hr = precipConv1Hr;
    }

    /**
     * @return the snowWaterEquiv
     */
    public Double getSnowWaterEquiv() {
        return snowWaterEquiv;
    }

    /**
     * @param snowWaterEquiv
     *            the snowWaterEquiv to set
     */
    public void setSnowWaterEquiv(Double snowWaterEquiv) {
        this.snowWaterEquiv = snowWaterEquiv;
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
        return "modelsounding";
    }
}
