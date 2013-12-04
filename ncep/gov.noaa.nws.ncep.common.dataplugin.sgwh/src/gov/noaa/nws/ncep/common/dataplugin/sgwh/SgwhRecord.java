package gov.noaa.nws.ncep.common.dataplugin.sgwh;

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
 * SgwhRecord This java class performs the mapping to the database for BUFR
 * Sgwh.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------- ----------- --------------------------
 * Aug17 2011               Chin Chen   Initial Coding (Following BufrsgwhRecord
 *                                      to refactor for  saving data to HDF5)
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
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "sgwhseq")
@Table(name = "sgwh", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "sgwh", indexes = { @Index(name = "sgwh_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class SgwhRecord extends PersistablePluginDataObject implements
        IPointData {
    private static final long serialVersionUID = 1L;

    /** Satellite Identification */
    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    private Long said;

    /** Software Identification */
    @Column
    @DynamicSerializeElement
    private Long swid;

    /** Identification of the originating/generating center */
    @Column
    @DynamicSerializeElement
    private Long ogce;

    /** Satellite sensor indicator, first occurrence */
    @Column
    @DynamicSerializeElement
    private Long ssin1;

    /** Satellite sensor indicator, second occurrence */
    @Column
    @DynamicSerializeElement
    private Long ssin2;

    /** Orbit Number */
    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    private Long orbn;

    /** Height of station in meters */
    @Column
    @DynamicSerializeElement
    private Double selv = IDecoderConstantsN.DOUBLE_MISSING;

    /** Height increment in meters */
    @Column
    @DynamicSerializeElement
    private Double hinc = IDecoderConstantsN.DOUBLE_MISSING;

    /** Observation time */
    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Calendar obsTime;

    /** Latitude */
    @Column
    @DataURI(position = 4)
    @DynamicSerializeElement
    private Double clath = IDecoderConstantsN.DOUBLE_MISSING;

    /** Longitude */
    @Column
    @DataURI(position = 5)
    @DynamicSerializeElement
    private Double clonh = IDecoderConstantsN.DOUBLE_MISSING;

    /** Remotely sensed surface type */
    @Transient
    @DynamicSerializeElement
    private Long rsst;

    /** Altimeter echo type */
    @Transient
    @DynamicSerializeElement
    private Long aetp;

    /** Land/Sea qualifier */
    @Transient
    @DynamicSerializeElement
    private Long lsql;

    /** Altimeter state flag */
    @Transient
    @DynamicSerializeElement
    private Long asfl;

    /** Radiometer state flag */
    @Transient
    @DynamicSerializeElement
    private Long rsfl;

    /** 3D error estimate of the navigator orbit */
    @Transient
    @DynamicSerializeElement
    private Long eeno;

    /** Associated field significance for sgwh */
    @Transient
    @DynamicSerializeElement
    private Long afssgwh;

    /** Significant wave height in meters */
    @Transient
    @DynamicSerializeElement
    private Double sgwh = IDecoderConstantsN.DOUBLE_MISSING;

    /** First order statistics */
    @Transient
    @DynamicSerializeElement
    private Long fostsgwh;

    /** Significant wave height (standard deviation) in meters */
    @Transient
    @DynamicSerializeElement
    private Double sgwhstd = IDecoderConstantsN.DOUBLE_MISSING;

    /** Number of valid points per second used to derive previous parameters */
    @Transient
    @DynamicSerializeElement
    private Long nvpp;

    /** Type of Band for 1st replication */
    @Transient
    @DynamicSerializeElement
    private Long tobdg1r1;

    /** Type of Band for 2nd replication */
    @Transient
    @DynamicSerializeElement
    private Long tobdg1r2;

    /**
     * Associated field significance for group 1, replication 1, first
     * occurrence
     */
    @Transient
    @DynamicSerializeElement
    private Long afsbkstg1r1;

    /**
     * Associated field significance for group 1, replication 2, first
     * occurrence
     */
    @Transient
    @DynamicSerializeElement
    private Long afsbkstg1r2;

    /** Backscatter in decibels for group 1 rep 1 */
    @Transient
    @DynamicSerializeElement
    private Double bkstg1r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Backscatter in decibels for group 1 rep 2 */
    @Transient
    @DynamicSerializeElement
    private Double bkstg1r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** fost for bkst for group 1, replication 1, first fost occurrence */
    @Transient
    @DynamicSerializeElement
    private Long fostbkstg1r1;

    /** fost for bkst for group 1, replication 2, first fost occurrence */
    @Transient
    @DynamicSerializeElement
    private Long fostbkstg1r2;

    /** Backscatter Standard Deviation in decibels for group 1 rep 1 */
    @Transient
    @DynamicSerializeElement
    private Double bkststdg1r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Backscatter Standard Deviation in decibels for group 1 rep 2 */
    @Transient
    @DynamicSerializeElement
    private Double bkststdg1r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Square of the off-nadir angle in degrees squared */
    @Transient
    @DynamicSerializeElement
    private Double sona;

    /**
     * Associated Field Significance for Square of the off-nadir angle in
     * degrees squared
     */
    @Transient
    @DynamicSerializeElement
    private Long afssona;

    /** Radiometer water vapor content in kg per square meter */
    @Transient
    @DynamicSerializeElement
    private Double rwvc = IDecoderConstantsN.DOUBLE_MISSING;

    /** Radiometer liquid content in kg per square meter */
    @Transient
    @DynamicSerializeElement
    private Double rlqc = IDecoderConstantsN.DOUBLE_MISSING;

    /**
     * Associated field significance for group 1, replication 1, second
     * occurrence
     */
    @Transient
    @DynamicSerializeElement
    private Long afsselvg1r1;

    /**
     * Associated field significance for group 1, replication 2, second
     * occurrence
     */
    @Transient
    @DynamicSerializeElement
    private Long afsselvg1r2;

    /** Height of station for 1st replication */
    @Transient
    @DynamicSerializeElement
    private Double selvg1r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Height of station for 2nd replication */
    @Transient
    @DynamicSerializeElement
    private Double selvg1r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Height increment in meters for 1st replication */
    @Transient
    @DynamicSerializeElement
    private Double hincg1r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Height increment in meters for snd replication */
    @Transient
    @DynamicSerializeElement
    private Double hincg1r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** fost for selv for group 1, replication 1, second fost occurrence */
    @Transient
    @DynamicSerializeElement
    private Long fostselvg1r1;

    /** fost for selv for group 1, replication 2, second fost occurrence */
    @Transient
    @DynamicSerializeElement
    private Long fostselvg1r2;

    /** Std of Height of station for 1st replication */
    @Transient
    @DynamicSerializeElement
    private Double selvstdg1r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Std of Height of station for 2nd replication */
    @Transient
    @DynamicSerializeElement
    private Double selvstdg1r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /**
     * Number of valid points per second used to derive previous parameters for
     * 1st replication
     */
    @Transient
    @DynamicSerializeElement
    private Long nvppg1r1;

    /**
     * Number of valid points per second used to derive previous parameters for
     * 2nd replication
     */
    @Transient
    @DynamicSerializeElement
    private Long nvppg1r2;

    /** MEFR Mean Frequency for group 2 for 1st replication */
    @Transient
    @DynamicSerializeElement
    private Double mefrg2r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** mefr Mean Frequency for group 2 for 2nd replication */
    @Transient
    @DynamicSerializeElement
    private Double mefrg2r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** mefr Mean Frequency for group 2 for 3rd replication */
    @Transient
    @DynamicSerializeElement
    private Double mefrg2r3 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Associated field significance for tmbr for group 2, replication 1 */
    @Transient
    @DynamicSerializeElement
    private Long afstmbrg2r1;

    /** Associated field significance for tmbr for group 2, replication 2 */
    @Transient
    @DynamicSerializeElement
    private Long afstmbrg2r2;

    /** Associated field significance for tmbr for group 2, replication 3 */
    @Transient
    @DynamicSerializeElement
    private Long afstmbrg2r3;

    /** Brightness temperature tmbrg2r1 in K for group 2, rep 1 */
    @Transient
    @DynamicSerializeElement
    private Double tmbrg2r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Brightness temperature tmbrg2r2 in K for group 2 rep 2 */
    @Transient
    @DynamicSerializeElement
    private Double tmbrg2r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Brightness temperature tmbrg2r3 in K for group 2 rep 3 */
    @Transient
    @DynamicSerializeElement
    private Double tmbrg2r3 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Satellite-derived wind computation method swcmg3r1 for group 3 rep 1 */
    @Transient
    @DynamicSerializeElement
    private Long swcmg3r1;

    /** Satellite-derived wind computation method swcmg3r1 for group 3 rep 2 */
    @Transient
    @DynamicSerializeElement
    private Long swcmg3r2;

    /** Wind speed at 10 m; in meters per second; for group 3 rep 1 */
    @Transient
    @DynamicSerializeElement
    private Double ws10g3r1 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Wind speed at 10 m; in meters per second; for group 3 rep 2 */
    @Transient
    @DynamicSerializeElement
    private Double ws10g3r2 = IDecoderConstantsN.DOUBLE_MISSING;

    /** Report type */
    // @Column(length=8)
    @Transient
    @DynamicSerializeElement
    @DataURI(position = 6)
    private String reportType;

    /** Text of the WMO header */
    @Column(length = 32)
    @DynamicSerializeElement
    private String wmoHeader;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Empty constructor.
     */
    public SgwhRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public SgwhRecord(String uri) {
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
    public Long getSaid() {
        return said;
    }

    /**
     * Set the Satellite Identifier.
     * 
     * @param said
     *            the Satellite ID to set
     */
    public void setSaid(Long said) {
        this.said = said;
    }

    /**
     * @param swid
     *            the Software Identification to get
     * 
     * @return the swid
     */
    public Long getSwid() {
        return swid;
    }

    /**
     * @param swid
     *            the Software Identification to set
     */
    public void setSwid(Long swid) {
        this.swid = swid;
    }

    /**
     * @param ogce
     *            the Identification of originating/generating center to return
     * 
     * @return the ogce
     */
    public Long getOgce() {
        return ogce;
    }

    /**
     * @param ogce
     *            the Identification of originating/generating center to set
     */
    public void setOgce(Long ogce) {
        this.ogce = ogce;
    }

    /**
     * @param ssin1
     *            the Satellite Sensor Indicator 1 to return
     * 
     * @return the ssin1
     */
    public Long getSsin1() {
        return ssin1;
    }

    /**
     * @param ssin
     *            the Satellite Sensor Indicator 1 to set
     */
    public void setSsin1(Long ssin1) {
        this.ssin1 = ssin1;
    }

    /**
     * @param ssin
     *            the Satellite Sensor Indicator 2 to return
     * 
     * @return the ssin2
     */
    public Long getSsin2() {
        return ssin2;
    }

    /**
     * @param ssin2
     *            the Satellite Sensor Indicator 2 to set
     */
    public void setSsin2(Long ssin2) {
        this.ssin2 = ssin2;
    }

    /**
     * @param orbn
     *            the Orbit number to return
     * 
     * @return the orbn
     */
    public Long getOrbn() {
        return orbn;
    }

    /**
     * @param orbn
     *            the Orbit number to set
     */
    public void setOrbn(Long orbn) {
        this.orbn = orbn;
    }

    /**
     * @param selv
     *            height of station to return
     * 
     * @return the selv
     */
    public Double getSelv() {
        return selv;
    }

    /**
     * @param selv
     *            the height of elevation to set
     */
    public void setSelv(Double selv) {
        this.selv = selv;
    }

    /**
     * @param hinc
     *            height increment to return
     * 
     * @return the hinc
     */
    public Double getHinc() {
        return hinc;
    }

    /**
     * @param hinc
     *            the height increment to set
     */
    public void setHinc(Double hinc) {
        this.hinc = hinc;
    }

    /**
     * @return the clath
     */
    public Double getClath() {
        return clath;
    }

    /**
     * @param clath
     *            the clath to set
     */
    public void setClath(Double clath) {
        this.clath = clath;
    }

    /**
     * @return the clonh
     */
    public Double getClonh() {
        return clonh;
    }

    /**
     * @param clonh
     *            the clonh to set
     */
    public void setClonh(Double clonh) {
        this.clonh = clonh;
    }

    /**
     * @param rsst
     *            remotely sensed surface type to return
     * 
     * @return the rsst
     */
    public Long getRsst() {
        return rsst;
    }

    /**
     * @param rsst
     *            the remotely sensed surface type to set
     */
    public void setRsst(Long rsst) {
        this.rsst = rsst;
    }

    /**
     * @param aetp
     *            remotely sensed surface type to return
     * 
     * @return the aetp
     */
    public Long getAetp() {
        return aetp;
    }

    /**
     * @param aetp
     *            the remotely sensed surface type to set
     */
    public void setAetp(Long aetp) {
        this.aetp = aetp;
    }

    /**
     * @param lsql
     *            land/sea qualifier to return
     * 
     * @return the lsql
     */
    public Long getLsql() {
        return lsql;
    }

    /**
     * @param lsql
     *            land/sea qualifer to set
     */
    public void setLsql(Long lsql) {
        this.lsql = lsql;
    }

    /**
     * @param asfl
     *            altimeter state flag to return
     * 
     * @return the asfl
     */
    public Long getAsfl() {
        return asfl;
    }

    /**
     * @param asfl
     *            the altimeter state flag to set
     */
    public void setAsfl(Long asfl) {
        this.asfl = asfl;
    }

    /**
     * @param rsfl
     *            radiometer state flag to return
     * 
     * @return the rsfl
     */
    public Long getRsfl() {
        return rsfl;
    }

    /**
     * @param rsfl
     *            the radiometer state flag to set
     */
    public void setRsfl(Long rsfl) {
        this.rsfl = rsfl;
    }

    /**
     * @param eeno
     *            three dimensional error estimate of the navigator orbit to
     *            return
     * 
     * @return the eeno
     */
    public Long getEeno() {
        return eeno;
    }

    /**
     * @param eeno
     *            three dimensional error estimate of the navigator orbit to set
     */
    public void setEeno(Long eeno) {
        this.eeno = eeno;
    }

    /**
     * @param afssgwh
     *            associated field significance for sgwh to return
     * 
     * @return the afssgwh
     */
    public Long getAfssgwh() {
        return afssgwh;
    }

    /**
     * @param afssgwh
     *            associated field significance for sgwh to set
     */
    public void setAfssgwh(Long afssgwh) {
        this.afssgwh = afssgwh;
    }

    /**
     * @param sgwh
     *            significant wave height to return
     * 
     * @return the sgwh
     */
    public Double getSgwh() {
        return sgwh;
    }

    /**
     * @param sgwh
     *            significant wave height to set
     */
    public void setSgwh(Double sgwh) {
        this.sgwh = sgwh;
    }

    /**
     * @param sgwh
     *            significant wave height (standard deviation) to return
     * 
     * @return the sgwhStd
     */
    public Double getSgwhstd() {
        return sgwhstd;
    }

    /**
     * @param sgwh
     *            significant wave height to set
     */
    public void setSgwhstd(Double sgwhstd) {
        this.sgwhstd = sgwhstd;
    }

    /**
     * @param fostsgwh
     *            first order statistics for sgwh to return
     * 
     * @return the fostsgwh
     */
    public Long getFostsgwh() {
        return fostsgwh;
    }

    /**
     * @param fostsgwh
     *            first order statistics for sgwh to set
     */
    public void setFostsgwh(Long fostsgwh) {
        this.fostsgwh = fostsgwh;
    }

    /**
     * @param nvpp
     *            number of valid points per sec used to derive previous
     *            parameters to return
     * 
     * @return the nvpp
     */
    public Long getNvpp() {
        return nvpp;
    }

    /**
     * @param nvpp
     *            number of valid points per sec used to derive previous
     *            parameters to set
     */
    public void setNvpp(Long nvpp) {
        this.nvpp = nvpp;
    }

    /**
     * @param tobdg1r1
     *            type of band for 1st replication to return
     * 
     * @return the tobdg1r1
     */
    public Long getTobdg1r1() {
        return tobdg1r1;
    }

    /**
     * @param tbnd
     *            type of band for 1st replication to set
     */
    public void setTobdg1r1(Long tobdg1r1) {
        this.tobdg1r1 = tobdg1r1;
    }

    /**
     * @param tobdg1r2
     *            type of band for 2nd replication to return
     * 
     * @return the tobdg1r2
     */
    public Long getTobdg1r2() {
        return tobdg1r2;
    }

    /**
     * @param tobdg1r2
     *            type of band for 2nd replication to set
     */
    public void setTobdg1r2(Long tobdg1r2) {
        this.tobdg1r2 = tobdg1r2;
    }

    /**
     * @param afsbkstg1r1
     *            associated field sig. bkst in group1, 1st replication to
     *            return
     * 
     * @return the afsbkstg1r1
     */
    public Long getAfsbkstg1r1() {
        return afsbkstg1r1;
    }

    /**
     * @param afsbkstg1r1
     *            associated field sig. for bkst in group 1, 1st replication to
     *            set
     */
    public void setAfsbkstg1r1(Long afsbkstg1r1) {
        this.afsbkstg1r1 = afsbkstg1r1;
    }

    /**
     * @param afsbkstg1r2
     *            associated field sig for bkst in group 1, 2nd replication to
     *            return
     * 
     * @return the afsbkstg1r2
     */
    public Long getAfsbkstg1r2() {
        return afsbkstg1r2;
    }

    /**
     * @param afsbkstg1r2
     *            associated field sig for bkst in group 1, 2nd replication to
     *            set
     */
    public void setAfsbkstg1r2(Long afsbkstg1r2) {
        this.afsbkstg1r2 = afsbkstg1r2;
    }

    /**
     * @param afsselvg1r1
     *            associated field sig for selv in group 1, 1st replication to
     *            return
     * 
     * @return the afsselvg1r1
     */
    public Long getAfsselvg1r1() {
        return afsselvg1r1;
    }

    /**
     * @param afsselvg1r1
     *            associated field sig for selv in group 1, 1st replication to
     *            set
     */
    public void setAfsselvg1r1(Long afsselvg1r1) {
        this.afsselvg1r1 = afsselvg1r1;
    }

    /**
     * @param afsselvg1r2b
     *            associated field sig for selv in group 1, 2nd replication to
     *            return
     * 
     * @return the afsselvg1r2
     */
    public Long getAfsselvg1r2() {
        return afsselvg1r2;
    }

    /**
     * @param afsselvg1r2b
     *            associated field sig for selv in group 1 for 2nd replication
     *            to set
     */
    public void setAfsselvg1r2(Long afsselvg1r2) {
        this.afsselvg1r2 = afsselvg1r2;
    }

    /**
     * @param fostbkstg1r1
     *            fost for bkst for group 1, 1st replication to return
     * 
     * @return the fostbkstg1r1
     */
    public Long getFostbkstg1r1() {
        return fostbkstg1r1;
    }

    /**
     * @param fostbkstg1r1
     *            fost for bkst for group 1, 1st replication to set
     */
    public void setFostbkstg1r1(Long fostbkstg1r1) {
        this.fostbkstg1r1 = fostbkstg1r1;
    }

    /**
     * @param fostbkstg1r2
     *            fost for bkst for group 1, 2nd replication to return
     * 
     * @return the fostbkstg1r2 fost for bkst for group 1, 2nd replication to
     *         set
     */
    public Long getFostbkstg1r2() {
        return fostbkstg1r2;
    }

    /**
     * @param afsg1r2
     *            associated field sig for 1st occurrence in group 1, 2nd
     *            replication to set
     */
    public void setFostbkstg1r2(Long fostbkstg1r2) {
        this.fostbkstg1r2 = fostbkstg1r2;
    }

    /**
     * @param bkstg1r1
     *            backscatter to return
     * 
     * @return the bkstg1r1
     */
    public Double getBkstg1r1() {
        return bkstg1r1;
    }

    /**
     * @param bkst
     *            backscatter to set
     */
    public void setBkstg1r1(Double bkstg1r1) {
        this.bkstg1r1 = bkstg1r1;
    }

    /**
     * @param bkststdg1r1
     *            backscatter standard deviation to return
     * 
     * @return the bkststdg1r1
     */
    public Double getBkststdg1r1() {
        return bkststdg1r1;
    }

    /**
     * @param bkststdg1r1
     *            backscatter standard deviation to set
     */
    public void setBkststdg1r1(Double bkststdg1r1) {
        this.bkststdg1r1 = bkststdg1r1;
    }

    /**
     * @param bkstg1r2
     *            backscatter to return
     * 
     * @return the bkstg1r2
     */
    public Double getBkstg1r2() {
        return bkstg1r2;
    }

    /**
     * @param bkstg1r2
     *            backscatter to set
     */
    public void setBkstg1r2(Double bkstg1r2) {
        this.bkstg1r2 = bkstg1r2;
    }

    /**
     * @param bkststdg1r2
     *            backscatter standard deviation to return
     * 
     * @return the bkststdg1r2
     */
    public Double getBkststdg1r2() {
        return bkststdg1r2;
    }

    /**
     * @param bkststdg1r
     *            backscatter standard deviation to set
     */
    public void setBkststdg1r2(Double bkststdg1r2) {
        this.bkststdg1r2 = bkststdg1r2;
    }

    /**
     * @param fostselvg1r1
     *            fost for selv for group 1, 1st replication to return
     * 
     * @return the fostselvg1r1
     */
    public Long getFostselvg1r1() {
        return fostselvg1r1;
    }

    /**
     * @param fostselvg1r1
     *            fost for selv for group 1, 1st replication to set
     */
    public void setFostselvg1r1(Long fostselvg1r1) {
        this.fostselvg1r1 = fostselvg1r1;
    }

    /**
     * @param fostselvg1r2
     *            fost for selv for group 1, 2nd replication to return
     * 
     * @return the fostselvg1r2
     */
    public Long getFostselvg1r2() {
        return fostselvg1r2;
    }

    /**
     * @param fostselvg1r2
     *            fost for selv for 1st occurrence in group 1, 2nd replication
     *            to set
     */
    public void setFostselvg1r2(Long fostselvg1r2) {
        this.fostselvg1r2 = fostselvg1r2;
    }

    /**
     * @param selvg1r1
     *            elevation of satellite in group 1 replication 1 to return
     * 
     * @return the selvg1r1
     */
    public Double getSelvg1r1() {
        return selvg1r1;
    }

    /**
     * @param selv
     *            elevation of satellite in group 1 replication 1 to set
     */
    public void setSelvg1r1(Double selvg1r1) {
        this.selvg1r1 = selvg1r1;
    }

    /**
     * @param selvg1r2
     *            elevation of satellite in group 1 replication 2 to return
     * 
     * @return the selvg1r2
     */
    public Double getSelvg1r2() {
        return selvg1r2;
    }

    /**
     * @param selvg1r2
     *            elevation of satellite in group 1 replication 2 to set
     */
    public void setSelvg1r2(Double selvg1r2) {
        this.selvg1r2 = selvg1r2;
    }

    /**
     * @param hincg1r1
     *            height increment in group 1 replication 1 to return
     * 
     * @return the hincg1r1
     */
    public Double getHincg1r1() {
        return hincg1r1;
    }

    /**
     * @param hincg1r1
     *            height increment in group 1 replication 1 to set
     */
    public void setHincg1r1(Double hincg1r1) {
        this.hincg1r1 = hincg1r1;
    }

    /**
     * @param hincg1r2
     *            height increment in group 1 replication 2 to return
     * 
     * @return the hincg1r2
     */
    public Double getHincg1r2() {
        return hincg1r2;
    }

    /**
     * @param hincg1r1
     *            height increment in group 1 replication 2 to set
     */
    public void setHincg1r2(Double hincg1r2) {
        this.hincg1r2 = hincg1r2;
    }

    /**
     * @param selvstdg1r1
     *            std of elevation of satellite in group 1 replication 1 to
     *            return
     * 
     * @return the selvstdg1r1
     */
    public Double getSelvstdg1r1() {
        return selvstdg1r1;
    }

    /**
     * @param selvstdg1r1
     *            elevation of satellite in group 1 replication 2 to set
     */
    public void setSelvstdg1r1(Double selvstdg1r1) {
        this.selvstdg1r1 = selvstdg1r1;
    }

    /**
     * @param selvstdg1r2
     *            std of elevation of satellite in group 1 replication 1 to
     *            return
     * 
     * @return the selvstdg1r2
     */
    public Double getSelvstdg1r2() {
        return selvstdg1r2;
    }

    /**
     * @param selvstdg1r2
     *            elevation of satellite in group 1 replication 2 to set
     */
    public void setSelvstdg1r2(Double selvstdg1r2) {
        this.selvstdg1r2 = selvstdg1r2;
    }

    /**
     * @param nvppg1r1
     *            number of valid points per sec used to derive previous
     *            parameters in group 1 replication 1 to return
     * 
     * @return the nvppg1r1
     */
    public Long getNvppg1r1() {
        return nvppg1r1;
    }

    /**
     * @param nvppg1r1
     *            number of valid points per sec used to derive previous
     *            parameters in group 1 replication 1 to set
     */
    public void setNvppg1r1(Long nvppg1r1) {
        this.nvppg1r1 = nvppg1r1;
    }

    /**
     * @param nvppg1r2
     *            number of valid points per sec used to derive previous
     *            parameters in group 1 replication 2 to return
     * 
     * @return the nvppg1r1
     */
    public Long getNvppg1r2() {
        return nvppg1r2;
    }

    /**
     * @param nvppg1r2
     *            number of valid points per sec used to derive previous
     *            parameters in group 1 replication 2 to set
     */
    public void setNvppg1r2(Long nvppg1r2) {
        this.nvppg1r2 = nvppg1r2;
    }

    /**
     * @param afssona
     *            Associated Field Significance of Square of the off-nadir angle
     *            to return
     * 
     * @return the afssona
     */
    public Long getAfssona() {
        return afssona;
    }

    /**
     * @param afssona
     *            Assocaited Field Significance of sona to set
     */
    public void setAfssona(Long afssona) {
        this.afssona = afssona;
    }

    /**
     * @param sona
     *            Square of the off-nadir angle to return
     * 
     * @return the sona
     */
    public Double getSona() {
        return sona;
    }

    /**
     * @param sona
     *            Square of the off-nadir angle to set
     */
    public void setSona(Double sona) {
        this.sona = sona;
    }

    /**
     * @param mefrg2r1
     *            mean frequency in hz for group 2 rep 1 to return
     * 
     * @return the mefrg2r1
     */
    public Double getMefrg2r1() {
        return mefrg2r1;
    }

    /**
     * @param mefrg2r1
     *            mean frequency for group 2 rep 1 to set
     */
    public void setMefrg2r1(Double mefrg2r1) {
        this.mefrg2r1 = mefrg2r1;
    }

    /**
     * @param mefrg2r2
     *            mean frequency in hz for group 2 rep 2 to return
     * 
     * @return the mefrg2r2
     */
    public Double getMefrg2r2() {
        return mefrg2r2;
    }

    /**
     * @param mefrg2r2
     *            mean frequency for group 2 rep 2 to set
     */
    public void setMefrg2r2(Double mefrg2r2) {
        this.mefrg2r2 = mefrg2r2;
    }

    /**
     * @param mefrg2r3
     *            mean frequency in hz for group 2 rep 3 to return
     * 
     * @return the mefrg2r3
     */
    public Double getMefrg2r3() {
        return mefrg2r3;
    }

    /**
     * @param mefrg2r3
     *            mean frequency for group 2 rep 3 to set
     */
    public void setMefrg2r3(Double mefrg2r3) {
        this.mefrg2r3 = mefrg2r3;
    }

    /**
     * @param afstmbrg2r1
     *            associated field sig for tmbr in group 2, 1st replication to
     *            return
     * 
     * @return the afstmbrg2r1
     */
    public Long getAfstmbrg2r1() {
        return afstmbrg2r1;
    }

    /**
     * @param afstmbrg2r1
     *            associated field sig for tmbr in group 2, 1st replication to
     *            set
     */
    public void setAfstmbrg2r1(Long afstmbrg2r1) {
        this.afstmbrg2r1 = afstmbrg2r1;
    }

    /**
     * @param afstmbrg2r2b
     *            associated field sig for tmbr in group 2, 2nd replication to
     *            return
     * 
     * @return the afstmbrg2r2
     */
    public Long getAfstmbrg2r2() {
        return afstmbrg2r2;
    }

    /**
     * @param afstmbrg2r2b
     *            associated field sig for tmbr in group 2 for 2nd replication
     *            to set
     */
    public void setAfstmbrg2r2(Long afstmbrg2r2) {
        this.afstmbrg2r2 = afstmbrg2r2;
    }

    /**
     * @param afstmbrg2r3
     *            associated field sig for tmbr in group 2, 3rd replication to
     *            return
     * 
     * @return the afstmbrg2r3
     */
    public Long getAfstmbrg2r3() {
        return afstmbrg2r3;
    }

    /**
     * @param afstmbrg2r3
     *            associated field sig for tmbr in group 2, 3rd replication to
     *            set
     */
    public void setAfstmbrg2r3(Long afstmbrg2r3) {
        this.afstmbrg2r3 = afstmbrg2r3;
    }

    /**
     * @param tmbrg2r1
     *            brightness temperature in K for group 2 rep 1 to return
     * 
     * @return the tmbrg2r1
     */
    public Double getTmbrg2r1() {
        return tmbrg2r1;
    }

    /**
     * @param tmbrg2r1
     *            brightness temperaturefor group 2 rep 1 to set
     */
    public void setTmbrg2r1(Double tmbrg2r1) {
        this.tmbrg2r1 = tmbrg2r1;
    }

    /**
     * @param tmbrg2r2
     *            brightness temperature in K for group 2 rep 2 to return
     * 
     * @return the tmbrg2r2
     */
    public Double getTmbrg2r2() {
        return tmbrg2r2;
    }

    /**
     * @param tmbrg2r2
     *            brightness temperaturefor group 2 rep 2 to set
     */
    public void setTmbrg2r2(Double tmbrg2r2) {
        this.tmbrg2r2 = tmbrg2r2;
    }

    /**
     * @param tmbrg2r3
     *            brightness temperature in K for group 2 rep 3 to return
     * 
     * @return the tmbrg2r3
     */
    public Double getTmbrg2r3() {
        return tmbrg2r3;
    }

    /**
     * @param tmbrg2r3
     *            mean frequency for group 2 rep 3 to set
     */
    public void setTmbrg2r3(Double tmbrg2r3) {
        this.tmbrg2r3 = tmbrg2r3;
    }

    /**
     * @param swcmg3r1
     *            satellite-derived wind computation method for group 3 rep 1 to
     *            return
     * 
     * @return the swcmg3r1
     */
    public Long getSwcmg3r1() {
        return swcmg3r1;
    }

    /**
     * @param swcmg3r1
     *            satellite-derived wind computation method for group 3 rep 1 to
     *            set
     */
    public void setSwcmg3r1(Long swcmg3r1) {
        this.swcmg3r1 = swcmg3r1;
    }

    /**
     * @param swcmg3r2
     *            satellite-derived wind computation method for group 3 rep 2 to
     *            return
     * 
     * @return the swcmg3r2
     */
    public Long getSwcmg3r2() {
        return swcmg3r2;
    }

    /**
     * @param swcmg3r2
     *            satellite-derived wind computation method for group 3 rep 2 to
     *            set
     */
    public void setSwcmg3r2(Long swcmg3r2) {
        this.swcmg3r2 = swcmg3r2;
    }

    /**
     * @param ws10g3r1
     *            wind speed at 10 m for group 3 rep 1 to return
     * 
     * @return the ws10g3r1
     */
    public Double getWs10g3r1() {
        return ws10g3r1;
    }

    /**
     * @param ws10g3r1
     *            wind speed at 10 m to set
     */
    public void setWs10g3r1(Double ws10g3r1) {
        this.ws10g3r1 = ws10g3r1;
    }

    /**
     * @param ws10g3r1
     *            wind speed at 10 m for group 3 rep 2 to return
     * 
     * @return the ws10g3r2
     */
    public Double getWs10g3r2() {
        return ws10g3r2;
    }

    /**
     * @param ws10g3r1
     *            wind speed at 10 m to set
     */
    public void setWs10g3r2(Double ws10g3r2) {
        this.ws10g3r2 = ws10g3r2;
    }

    /**
     * @param rwvc
     *            radiometer liquid content to return
     * 
     * @return the rwvc
     */
    public Double getRwvc() {
        return rwvc;
    }

    /**
     * @param rwvc
     *            radiometer liquid content to set
     */
    public void setRwvc(Double rwvc) {
        this.rwvc = rwvc;
    }

    /**
     * @param rlqc
     *            radiometer liquid content to return
     * 
     * @return the rlqc
     */
    public Double getRlqc() {
        return rlqc;
    }

    /**
     * @param rlqc
     *            radiometer liquid content to set
     */
    public void setRlqc(Double rlqc) {
        this.rlqc = rlqc;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
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
        return "sgwh";
    }
}
