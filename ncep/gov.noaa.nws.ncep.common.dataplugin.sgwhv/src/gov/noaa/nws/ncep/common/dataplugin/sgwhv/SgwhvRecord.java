package gov.noaa.nws.ncep.common.dataplugin.sgwhv;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * SgwhvRecord This java class performs the mapping to the database for BUFR
 * Sgwhv.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------- ----------- --------------------------
 * Aug23 2011               Chin Chen   Initial Coding (Following
 *                                      BufrsgwhvRecord to refactor for  saving
 *                                      data to HDF5)
 * Apr 04, 2013 1846        bkowal      Added an index on refTime and
 *                                      forecastTime
 * Apr 08, 2013 1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract.
 * Dec 03, 2013 2551        rjpeter     Extend PersistablePluginDataObject.
 * </pre>
 * 
 * @author chin chen
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "sgwhvseq")
@Table(name = "sgwhv", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "sgwhv", indexes = { @Index(name = "sgwhv_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class SgwhvRecord extends PersistablePluginDataObject implements
        IPointData {
    private static final long serialVersionUID = 1L;

    private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;

    private static final Integer IMISSD = IDecoderConstantsN.INTEGER_MISSING;

    /** Satellite Identification */
    @Column
    /*
     * Height of waves are from blended TOPEX/Poseidon, Jason-1, ERS 1, ERS 2m
     * GPO and Envisat ocean altimetry satellites, therefore satelliteId should
     * be a primary key. Data is from Navoceano. SGWH is from Jason-1 and has
     * satelliteID of 260. SGWH2 is from Jason-2 and has satelliteId of 261.
     * SGWHE is from Envisat and has satelliteId of 60.
     */
    @DataURI(position = 4)
    @DynamicSerializeElement
    private Long satelliteId;

    /** Observation time */
    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    private Calendar obsTime;

    /** Latitude */
    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    private Double lat;

    /** Longitude */
    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Double lon;

    /** Wind speed at 10 m (m/s) */
    @Transient
    @DynamicSerializeElement
    private Double wspd10;

    /** Height of waves (m) */
    @Transient
    @DynamicSerializeElement
    private Double htwaves;

    /** Standard Deviation of significant wave height (m) */
    @Transient
    @DynamicSerializeElement
    private Double sgwhSd;

    /** Altitude (Platform to ellipsoid) (m) */
    @Transient
    @DynamicSerializeElement
    private Double altitude;

    /** Number of valid points per second used to derive previous parameters */
    @Transient
    @DynamicSerializeElement
    private Long peak;

    /** Altitude correction (ionosphere) */
    @Transient
    @DynamicSerializeElement
    private Double altCorrI;

    /** Altitude correction (dry troposphere) */
    @Transient
    @DynamicSerializeElement
    private Double altCorrD;

    /** Altitude correction (wet troposphere) */
    @Transient
    @DynamicSerializeElement
    private Double altCorrW;

    /** Open loop correction (auto gain control) in decibels for group 1 rep 2 */
    @Transient
    @DynamicSerializeElement
    private Double loopCorr;

    /** Backscatter in decibels */
    @Transient
    @DynamicSerializeElement
    private Double bkst;

    /** Report type */
    // @Column(length=8)
    @Transient
    @DynamicSerializeElement
    @DataURI(position = 5)
    private String reportType;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Default constructor.
     */
    public SgwhvRecord() {
        Double doubleRmissd = Double.parseDouble(String.valueOf(RMISSD));
        Long longImissd = Long.parseLong(String.valueOf(IMISSD));
        this.reportType = "BUFRSGWHV";
        this.satelliteId = longImissd;
        this.lat = doubleRmissd;
        this.lon = doubleRmissd;
        this.altCorrD = doubleRmissd;
        this.wspd10 = doubleRmissd;
        this.htwaves = doubleRmissd;
        this.sgwhSd = doubleRmissd;
        this.altitude = doubleRmissd;
        this.altCorrI = doubleRmissd;
        this.altCorrD = doubleRmissd;
        this.altCorrW = doubleRmissd;
        this.loopCorr = doubleRmissd;
        this.bkst = doubleRmissd;
        this.peak = longImissd;
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public SgwhvRecord(String uri) {
        super(uri);
    }

    /**
     * Get the observation report type.
     * 
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    /**
     * Set the observation report type.
     * 
     * @param reportType
     *            the reportType to set
     */
    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    /**
     * Get the Satellite Identifier.
     * 
     * @return the Satellite ID
     */
    public Long getSatelliteId() {
        return satelliteId;
    }

    /**
     * @param satelliteId
     *            the Satellite Identifier to set
     */
    public void setSatelliteId(Long satelliteId) {
        this.satelliteId = satelliteId;
    }

    /**
     * @return the lat
     */
    public Double getLat() {
        return lat;
    }

    /**
     * @param lat
     *            the latitude (coarse) to set
     */
    public void setLat(Double lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public Double getLon() {
        return lon;
    }

    /**
     * @param lon
     *            the longitude (coarse) to set
     */
    public void setLon(Double lon) {
        this.lon = lon;
    }

    /**
     * @param wspd10
     *            wind speed at 10 m to return
     */
    public Double getWspd10() {
        return wspd10;
    }

    /**
     * @param wspd10
     *            wind speed at 10 m to set
     */
    public void setWspd10(Double wspd10) {
        this.wspd10 = wspd10;
    }

    /**
     * @param htwaves
     *            height of waves to return
     */
    public Double getHtwaves() {
        return htwaves;
    }

    /**
     * @param htwaves
     *            height of waves to set
     */
    public void setHtwaves(Double htwaves) {
        this.htwaves = htwaves;
    }

    /**
     * @param SgwhSd
     *            SGWH standard deviation to return
     */
    /**
     * @return the SgwhSd
     */
    public Double getSgwhSd() {
        return sgwhSd;
    }

    /**
     * @param SgwhSd
     *            SGWH standard deviation to set
     */
    public void setSgwhSd(Double sgwhSd) {
        this.sgwhSd = sgwhSd;
    }

    /**
     * @param altitude
     *            altitude (platform to ellipsoid) m
     */
    public Double getAltitude() {
        return altitude;
    }

    /**
     * @param altitude
     *            altitude to set
     */
    public void setAltitude(Double altitude) {
        this.altitude = altitude;
    }

    /**
     * @param peak
     *            peakiness to return
     */
    /**
     * @return the peak
     */
    public Long getPeak() {
        return peak;
    }

    /**
     * @param peak
     *            peakiness to set
     */
    public void setPeak(Long peak) {
        this.peak = peak;
    }

    /**
     * @param altCorrI
     *            altitude correction (ionosphere) to return
     */
    /**
     * @return the altCorrI
     */
    public Double getAltCorrI() {
        return altCorrI;
    }

    /**
     * @param altCorrI
     *            altitude correction (ionosphere) (m) to set
     */
    public void setAltCorrI(Double altCorrI) {
        this.altCorrI = altCorrI;
    }

    /**
     * @param altCorrD
     *            altitude correction (dry troposphere) to return
     */
    /**
     * @return the altCorrD
     */
    public Double getAltCorrD() {
        return altCorrD;
    }

    /**
     * @param altCorrD
     *            altitude correction (dry troposphere) (m) to set
     */
    public void setAltCorrD(Double altCorrD) {
        this.altCorrD = altCorrD;
    }

    /**
     * @param altCorrW
     *            altitude correction (wet troposphere) to return
     */
    /**
     * @return the altCorrW
     */
    public Double getAltCorrW() {
        return altCorrW;
    }

    /**
     * @param altCorrI
     *            altitude correction (wet troposphere) (m) to set
     */
    public void setAltCorrW(Double altCorrW) {
        this.altCorrW = altCorrW;
    }

    /**
     * @param loopCorr
     *            open loop correction (auto gain control) (db) to return
     */
    /**
     * @return the loopCorr
     */
    public Double getLoopCorr() {
        return loopCorr;
    }

    /**
     * @param loopCorr
     *            open loop correction (auto gain control) (db) to set
     */
    public void setLoopCorr(Double loopCorr) {
        this.loopCorr = loopCorr;
    }

    /**
     * @param bkst
     *            backscatter to return
     */
    /**
     * @return the bkst
     */
    public Double getBkst() {
        return bkst;
    }

    /**
     * @param bkst
     *            backscatter to set
     */
    public void setBkst(Double bkst) {
        this.bkst = bkst;
    }

    /**
     * @return the obsTime
     */
    public Calendar getObsTime() {
        return obsTime;
    }

    /**
     * @param obsTime
     *            the obsTime to set
     */
    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    @Override
    public void setDataURI(String dataURI) {
        identifier = dataURI;
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
        return "sgwhv";
    }
}
