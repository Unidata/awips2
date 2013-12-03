package gov.noaa.nws.ncep.common.dataplugin.ssha;

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
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * SshaRecord
 * 
 * This java class performs the mapping to the database for Ssha.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ --------    --------    ----------------------------------------------
 * Sep 2011                 Chin Chen   Initial Coding (Following BufrsshaRecord
 *                                      to refactor for  saving data to HDF5)
 * Apr 04, 2013 1846        bkowal      Added an index on refTime and
 *                                      forecastTime
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Dec 03, 2013 2551        rjpeter     Removed get/setPersistenceTime override.
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 * 
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "sshaseq")
@Table(name = "ssha", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ssha", indexes = { @Index(name = "ssha_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class SshaRecord extends PersistablePluginDataObject implements
        IPointData, IPersistable {
    private static final long serialVersionUID = 1L;

    private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;

    private static final Integer IMISSD = IDecoderConstantsN.INTEGER_MISSING;

    /** Satellite Identification */
    @Column
    /*
     * Sea surface height anomalies are from blended TOPEX/Poseidon, Jason-1,
     * ERS 1, ERS 2m GPO and Envisat ocean altimetry satellites, therefore said
     * should be a primary key.
     */
    @DataURI(position = 4)
    @DynamicSerializeElement
    private Long said;

    /** Satellite Instruments */
    @Column
    @DynamicSerializeElement
    private Long siid;

    /** Station Acquisition */
    @Column(length = 20)
    @DynamicSerializeElement
    private String staq;

    /** Software Identification and Version Number */
    @Column(length = 12)
    @DynamicSerializeElement
    private String softv;

    /** Satellite Cycle Number */
    @Column
    @DynamicSerializeElement
    private Long sacyln;

    /** Orbit Number */
    @Column
    @DataURI(position = 5)
    @DynamicSerializeElement
    private Long orbn;

    /** Numerical Model Identifier */
    @Column(length = 16)
    @DynamicSerializeElement
    private String numid;

    /** Observation time */
    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    private Calendar obsTime;

    /** Latitude */
    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    private Double clath;

    /** Longitude */
    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    private Double clonh;

    /** Remotely Sensed Surface Type */
    @Transient
    @DynamicSerializeElement
    private Long rsst;

    /** Altimeter Echo Type */
    @Transient
    @DynamicSerializeElement
    private Long aetp;

    /** Radiometer Sensed Surface Type */
    @Transient
    @DynamicSerializeElement
    private Long dsst;

    /** Interpolation Flag */
    @Transient
    @DynamicSerializeElement
    private Long intf;

    /** Three-dimensional Error Estimate of the Navigator Orbit */
    @Transient
    @DynamicSerializeElement
    private Long eeno;

    /** Altimeter State Flag */
    @Transient
    @DynamicSerializeElement
    private Long asfl;

    /** Altimeter Data Quality Flag */
    @Transient
    @DynamicSerializeElement
    private Long adqf;

    /** Altimeter Correction Quality Flag */
    @Transient
    @DynamicSerializeElement
    private Long arqf;

    /** Altimeter Rain Flag */
    @Transient
    @DynamicSerializeElement
    private Long alrf;

    /** Radioimeter State Flag */
    @Transient
    @DynamicSerializeElement
    private Long rsfl;

    /** Radioimeter Data Quality Flag */
    @Transient
    @DynamicSerializeElement
    private Long rdqf;

    /** Radioimeter Brightness Temperature Interpretation */
    @Transient
    @DynamicSerializeElement
    private Long rbif;

    /** Ice Presence Indicator */
    @Transient
    @DynamicSerializeElement
    private Long ipin;

    /** Auxilliary Altimeter State Flags */
    @Transient
    @DynamicSerializeElement
    private Long aasf;

    /** Meteorological Map Availability */
    @Transient
    @DynamicSerializeElement
    private Long mmap;

    /** Interpolation Flag For Mean Diurnal Tide */
    @Transient
    @DynamicSerializeElement
    private Long ifdt;

    /** Ku Band Ocean Range (m) */
    @Transient
    @DynamicSerializeElement
    private Double kbor;

    /** RMS of 20 Hz Ku Band Ocean Range (m) */
    @Transient
    @DynamicSerializeElement
    private Double rkbor;

    /** Number of 20 Hz valid points for Ku Band */
    @Transient
    @DynamicSerializeElement
    private Long nvpk2;

    /** Ku Band net instrumental Correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double kbic;

    /** Sea State Bias Correction On Ku Band (m) */
    @Transient
    @DynamicSerializeElement
    private Double sbck;

    /** Ku Band Significant Wave Height (m) */
    @Transient
    @DynamicSerializeElement
    private Double kbsw;

    /** RMS 20 Hz Ku Band Significant Wave Height (m) */
    @Transient
    @DynamicSerializeElement
    private Double rksw;

    /** Number of 20 Hz valid points for Ku Band Significant Wave Height */
    @Transient
    @DynamicSerializeElement
    private Long nvksw;

    /**
     * 20 Hz RMS Ku Band Net Instrumental Correction For Significant Wave Height
     * (m)
     */
    @Transient
    @DynamicSerializeElement
    private Double kncs;

    /** Ku Band Corrected Ocean Backscatter coefficient (kb) */
    @Transient
    @DynamicSerializeElement
    private Double kobc;

    /** STD Ku Band Corrected Ocean Backscatter coefficient (kb) */
    @Transient
    @DynamicSerializeElement
    private Double skobc;

    /** Number of valid points for Ku Band Backscatter */
    @Transient
    @DynamicSerializeElement
    private Long nvpkb;

    /** Ku band net instrumental correction for AGC (db) */
    @Transient
    @DynamicSerializeElement
    private Double knic;

    /** Attenuation Correction 1 On Sigma-0 (db) */
    @Transient
    @DynamicSerializeElement
    private Double acrs1;

    /** Attenuation Correction 2 On Sigma-0 (db) */
    @Transient
    @DynamicSerializeElement
    private Double acrs2;

    /** Ku Band Automatic Gain Control (db) */
    @Transient
    @DynamicSerializeElement
    private Double kagc;

    /** RMS Ku Band Automatic Gain Control (db) */
    @Transient
    @DynamicSerializeElement
    private Double rkagc;

    /** Number of valid points for Ku Band Automatic Gain Control */
    @Transient
    @DynamicSerializeElement
    private Long nvkg;

    /** C Band Ocean Range (m) */
    @Transient
    @DynamicSerializeElement
    private Double cbor;

    /** RMS of C Band Ocean Range (m) */
    @Transient
    @DynamicSerializeElement
    private Double rcbor;

    /** Number of 20 Hz valid points for C band */
    @Transient
    @DynamicSerializeElement
    private Long nvpc;

    /** C band net instrumental correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double cbic;

    /** Sea state bias correction on C band (m) */
    @Transient
    @DynamicSerializeElement
    private Double sbcc;

    /** C band significant wave height (m) */
    @Transient
    @DynamicSerializeElement
    private Double cbsw;

    /** RMS 20 Hz C band significant wave height (m) */
    @Transient
    @DynamicSerializeElement
    private Double rcsw;

    /** Number of 20 Hz valid points for C band significance */
    @Transient
    @DynamicSerializeElement
    private Long nvcsw;

    /** C band net instrumental correction for significance (m) */
    @Transient
    @DynamicSerializeElement
    private Double cncs;

    /** C band corrected ocean backscatter coefficient (db) */
    @Transient
    @DynamicSerializeElement
    private Double ccob;

    /** RMS C band corrected ocean backscatter coefficient (db) */
    @Transient
    @DynamicSerializeElement
    private Double rccob;

    /** Number of valid points for C band backscatter */
    @Transient
    @DynamicSerializeElement
    private Long nvpcb;

    /** C band net instrumental correction for AGC (db) */
    @Transient
    @DynamicSerializeElement
    private Double cnia;

    /** C band automatic gain control (db) */
    @Transient
    @DynamicSerializeElement
    private Double cagc;

    /** RMS C band automatic gain control (db) */
    @Transient
    @DynamicSerializeElement
    private Double rcagc;

    /** Number of valid points for C band automatic gain */
    @Transient
    @DynamicSerializeElement
    private Long nvpca;

    /** Satellite channel center frequency 1 (hz) */
    @Transient
    @DynamicSerializeElement
    private Double sccf1;

    /** Satellite channel center frequency 2 (hz) */
    @Transient
    @DynamicSerializeElement
    private Double sccf2;

    /** Satellite channel center frequency 3 (hz) */
    @Transient
    @DynamicSerializeElement
    private Double sccf3;

    /** Brightness temperature 1 (k) */
    @Transient
    @DynamicSerializeElement
    private Double tmbrst1;

    /** Brightness temperature 2 (k) */
    @Transient
    @DynamicSerializeElement
    private Double tmbrst2;

    /** Brightness temperature 3 (k) */
    @Transient
    @DynamicSerializeElement
    private Double tmbrst3;

    /** Radiometer water vapor content (kg/m**2) */
    @Transient
    @DynamicSerializeElement
    private Double rwvc;

    /** Radiometer liquid content (kg/m**) */
    @Transient
    @DynamicSerializeElement
    private Double rlqc;

    /** Height or altitude 1 (m) */
    @Transient
    @DynamicSerializeElement
    private Double hmsl1;

    /** Height or altitude 2 (m) */
    @Transient
    @DynamicSerializeElement
    private Double hmsl2;

    /** Wind speed from altimeter (m/s) */
    @Transient
    @DynamicSerializeElement
    private Double wspa;

    /** Wind speed from radiometer (m/s) */
    @Transient
    @DynamicSerializeElement
    private Double wspr;

    /** u-component of the model wind vector (m/s) */
    @Transient
    @DynamicSerializeElement
    private Double umwv;

    /** v-component of the model wind vector (m/s) */
    @Transient
    @DynamicSerializeElement
    private Double vwmv;

    /** Mean dynamic topography (m) */
    @Transient
    @DynamicSerializeElement
    private Double mdyt;

    /** Altitude of COG above reference ellipsoid (m) */
    @Transient
    @DynamicSerializeElement
    private Double alre;

    /** Instantaneous altitude rate (m/s) */
    @Transient
    @DynamicSerializeElement
    private Double ialr;

    /** Squared off nadir angle of the satellite from platform data (degree**2) */
    @Transient
    @DynamicSerializeElement
    private Double onap;

    /** Squared off nadir angle of the satellite from waveform data (degree**2) */
    @Transient
    @DynamicSerializeElement
    private Double sonaw;

    /** Ionospheric correction from model on Ku band (m) */
    @Transient
    @DynamicSerializeElement
    private Double icmk;

    /** Altimeter ionospheric correction on Ku band (m) */
    @Transient
    @DynamicSerializeElement
    private Double aick;

    /** Model dry tropospheric correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double mdtc;

    /** Model wet tropospheric correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double mwtc;

    /** Radiometer wet tropospheric correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double rwtc;

    /** Mean sea-surface height (m) */
    @Transient
    @DynamicSerializeElement
    private Double mssh;

    /** Mean sea surface height from altimeter only (m) */
    @Transient
    @DynamicSerializeElement
    private Double msha;

    /** Geoid's height (m) */
    @Transient
    @DynamicSerializeElement
    private Double geodh;

    /** Ocean depth/land elevation (m) */
    @Transient
    @DynamicSerializeElement
    private Double odle;

    /** Solid earth tide height (m) */
    @Transient
    @DynamicSerializeElement
    private Double seth;

    /** Total geocentric ocean tide height solution 1 (m) */
    @Transient
    @DynamicSerializeElement
    private Double tgoth1;

    /** Total geocentric ocean tide height solution 2 (m) */
    @Transient
    @DynamicSerializeElement
    private Double tgoth2;

    /** Loading tide height geocentric ocean tide solution 1 (m) */
    @Transient
    @DynamicSerializeElement
    private Double lths1;

    /** Loading tide height geocentric ocean tide solution 2 (m) */
    @Transient
    @DynamicSerializeElement
    private Double lths2;

    /** Long period tide height (m) */
    @Transient
    @DynamicSerializeElement
    private Double lpth;

    /** Non-equilibrium long period tide height (m) */
    @Transient
    @DynamicSerializeElement
    private Double nlth;

    /** Geocentric pole tide height (m) */
    @Transient
    @DynamicSerializeElement
    private Double gpth;

    /* Inverted barometer correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double ibco;

    /* High frequency fluctuations of the sea surface topography correction (m) */
    @Transient
    @DynamicSerializeElement
    private Double hfstc;

    /** Sea Surface Height Anomoly (m) */
    @Transient
    @DynamicSerializeElement
    private Double ssha;

    /** Report type */
    @Transient
    @DynamicSerializeElement
    @DataURI(position = 6)
    private String reportType;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Default constructor.
     */
    public SshaRecord() {
        Double doubleRmissd = Double.parseDouble(String.valueOf(RMISSD));
        Long longImissd = Long.parseLong(String.valueOf(IMISSD));
        this.reportType = "BUFRSSHA";
        this.said = longImissd;
        this.siid = longImissd;
        this.staq = " ";
        this.softv = " ";
        this.sacyln = longImissd;
        this.orbn = longImissd;
        this.numid = " ";
        this.clath = doubleRmissd;
        this.clonh = doubleRmissd;
        this.rsst = longImissd;
        this.aetp = longImissd;
        this.dsst = longImissd;
        this.intf = longImissd;
        this.eeno = longImissd;
        this.asfl = longImissd;
        this.adqf = longImissd;
        this.arqf = longImissd;
        this.alrf = longImissd;
        this.rsfl = longImissd;
        this.rdqf = longImissd;
        this.rbif = longImissd;
        this.ipin = longImissd;
        this.aasf = longImissd;
        this.mmap = longImissd;
        this.ifdt = longImissd;
        this.kbor = doubleRmissd;
        this.rkbor = doubleRmissd;
        this.nvpk2 = longImissd;
        this.kbic = doubleRmissd;
        this.sbck = doubleRmissd;
        this.kbsw = doubleRmissd;
        this.rksw = doubleRmissd;
        this.nvksw = longImissd;
        this.kncs = doubleRmissd;
        this.kobc = doubleRmissd;
        this.skobc = doubleRmissd;
        this.nvpkb = longImissd;
        this.knic = doubleRmissd;
        this.acrs1 = doubleRmissd;
        this.acrs2 = doubleRmissd;
        this.kagc = doubleRmissd;
        this.rkagc = doubleRmissd;
        this.nvkg = longImissd;
        this.cbor = doubleRmissd;
        this.rcbor = doubleRmissd;
        this.nvpc = longImissd;
        this.cbic = doubleRmissd;
        this.sbcc = doubleRmissd;
        this.cbsw = doubleRmissd;
        this.rcsw = doubleRmissd;
        this.nvcsw = longImissd;
        this.cncs = doubleRmissd;
        this.ccob = doubleRmissd;
        this.rccob = doubleRmissd;
        this.nvpcb = longImissd;
        this.cnia = doubleRmissd;
        this.cagc = doubleRmissd;
        this.rcagc = doubleRmissd;
        this.nvpca = longImissd;
        this.sccf1 = doubleRmissd;
        this.sccf2 = doubleRmissd;
        this.sccf3 = doubleRmissd;
        this.tmbrst1 = doubleRmissd;
        this.tmbrst2 = doubleRmissd;
        this.tmbrst3 = doubleRmissd;
        this.rwvc = doubleRmissd;
        this.rlqc = doubleRmissd;
        this.hmsl1 = doubleRmissd;
        this.hmsl2 = doubleRmissd;
        this.wspa = doubleRmissd;
        this.wspr = doubleRmissd;
        this.umwv = doubleRmissd;
        this.vwmv = doubleRmissd;
        this.mdyt = doubleRmissd;
        this.alre = doubleRmissd;
        this.ialr = doubleRmissd;
        this.onap = doubleRmissd;
        this.sonaw = doubleRmissd;
        this.icmk = doubleRmissd;
        this.aick = doubleRmissd;
        this.mdtc = doubleRmissd;
        this.mwtc = doubleRmissd;
        this.rwtc = doubleRmissd;
        this.mssh = doubleRmissd;
        this.msha = doubleRmissd;
        this.geodh = doubleRmissd;
        this.odle = doubleRmissd;
        this.seth = doubleRmissd;
        this.tgoth1 = doubleRmissd;
        this.tgoth2 = doubleRmissd;
        this.lths1 = doubleRmissd;
        this.lths2 = doubleRmissd;
        this.lpth = doubleRmissd;
        this.nlth = doubleRmissd;
        this.gpth = doubleRmissd;
        this.ibco = doubleRmissd;
        this.hfstc = doubleRmissd;
        this.ssha = doubleRmissd;
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public SshaRecord(String uri) {
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
     * @param said
     *            the Satellite Identifier to set
     */
    public void setSaid(Long said) {
        this.said = said;
    }

    /**
     * Get the Satellite Instruments.
     * 
     * @return the Satellite Instruments
     */
    public Long getSiid() {
        return siid;
    }

    /**
     * @param siid
     *            the Satellite Instruments to set
     */
    public void setSiid(Long siid) {
        this.siid = siid;
    }

    /**
     * Get the Station Acquisition.
     * 
     * @return the Station Acquisition
     */
    public String getStaq() {
        return staq;
    }

    /**
     * @param staq
     *            the Station Acquisition to set
     */
    public void setStaq(String staq) {
        this.staq = staq;
    }

    /**
     * Get the Software Identification and Version.
     * 
     * @return the Software Id and Version
     */
    public String getSoftv() {
        return softv;
    }

    /**
     * @param softv
     *            the Software Id and Version to set
     */
    public void setSoftv(String softv) {
        this.softv = softv;
    }

    /**
     * Get the Satellite Cycle Number.
     * 
     * @return the Satellite Cycle Number
     */
    public Long getSacyln() {
        return sacyln;
    }

    /**
     * @param sacyln
     *            the Satellite Cycle Number to set
     */
    public void setSacyln(Long sacyln) {
        this.sacyln = sacyln;
    }

    /**
     * Get the Orbit Number.
     * 
     * @return the Orbit Number
     */
    public Long getOrbn() {
        return orbn;
    }

    /**
     * @param orbn
     *            the Orbit Number to set
     */
    public void setOrbn(Long orbn) {
        this.orbn = orbn;
    }

    /**
     * Get the Numerical Model Identifier.
     * 
     * @return the Numerical Model Identifier
     */
    public String getNumid() {
        return numid;
    }

    /**
     * @param numid
     *            the Numerical Model Identifier to set
     */
    public void setNumid(String numid) {
        this.numid = numid;
    }

    /**
     * @return the clath
     */
    public Double getClath() {
        return clath;
    }

    /**
     * @param clath
     *            the latitude to set
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
     *            the longitude to set
     */
    public void setClonh(Double clonh) {
        this.clonh = clonh;
    }

    /**
     * Get the Remote Sensed Surface Type.
     * 
     * @return the Remote Sensed Surface Type
     */
    public Long getRsst() {
        return rsst;
    }

    /**
     * @param rsst
     *            the Remote Sensed Surface Type to set
     */
    public void setRsst(Long rsst) {
        this.rsst = rsst;
    }

    /**
     * Get the Altimeter Echo Type.
     * 
     * @return the Altimeter Echo Type
     */
    public Long getAetp() {
        return aetp;
    }

    /**
     * @param aetp
     *            the Altimeter Echo Type to set
     */
    public void setAetp(Long aetp) {
        this.aetp = aetp;
    }

    /**
     * Get the Radiometer Sensed Surface Type.
     * 
     * @return the Remote Sensed Surface Type
     */
    public Long getDsst() {
        return dsst;
    }

    /**
     * @param dsst
     *            the Radiometer Sensed Surface Type to set
     */
    public void setDsst(Long dsst) {
        this.dsst = dsst;
    }

    /**
     * Get the Interpolation Flag.
     * 
     * @return the Interpolation Flag
     */
    public Long getIntf() {
        return intf;
    }

    /**
     * @param intf
     *            the Interpolation Flag to set
     */
    public void setIntf(Long intf) {
        this.intf = intf;
    }

    /**
     * Get the Three-dimensional Error Estimate of the Navigator Orbit.
     * 
     * @return the Three-dimensional Error Estimate of the Navigator Orbit
     */
    public Long getEeno() {
        return eeno;
    }

    /**
     * @param eeno
     *            the Three-dimensional Error Estimate of the Navigator Orbit to
     *            set
     */
    public void setEeno(Long eeno) {
        this.eeno = eeno;
    }

    /**
     * Get the Altimeter State Flag.
     * 
     * @return the Altimeter State Flag
     */
    public Long getAsfl() {
        return asfl;
    }

    /**
     * @param asfl
     *            the Altimeter State Flag to set
     */
    public void setAsfl(Long asfl) {
        this.asfl = asfl;
    }

    /**
     * Get the Altimeter Data Quality Flag.
     * 
     * @return the Altimeter Data Quality Flag
     */
    public Long getAdqf() {
        return adqf;
    }

    /**
     * @param adqf
     *            the Altimeter Data Quality Flag to set
     */
    public void setAdqf(Long adqf) {
        this.adqf = adqf;
    }

    /**
     * Get the Altimeter Correction Quality Flag.
     * 
     * @return the Altimeter Correction Quality Flag
     */
    public Long getArqf() {
        return arqf;
    }

    /**
     * @param arqf
     *            the Altimeter Correction Quality Flag to set
     */
    public void setArqf(Long arqf) {
        this.arqf = arqf;
    }

    /**
     * Get the Altimeter Rain Flag.
     * 
     * @return the Altimeter Rain Flag
     */
    public Long getAlrf() {
        return alrf;
    }

    /**
     * @param alrf
     *            the Altimeter Rain Flag to set
     */
    public void setAlrf(Long alrf) {
        this.alrf = alrf;
    }

    /**
     * Get the Radiometer State Flag.
     * 
     * @return the Radiometer State Flag
     */
    public Long getRsfl() {
        return rsfl;
    }

    /**
     * @param rsfl
     *            the Radiometer State Flag to set
     */
    public void setRsfl(Long rsfl) {
        this.rsfl = rsfl;
    }

    /**
     * Get the Radiometer Data Quality Flag.
     * 
     * @return the Radiometer Data Quality Flag
     */
    public Long getRdqf() {
        return rdqf;
    }

    /**
     * @param rdqf
     *            the Radiometer Data Quality Flag to set
     */
    public void setRdqf(Long rdqf) {
        this.rdqf = rdqf;
    }

    /**
     * Get the Radiometer Brightness Temperature Interpretation.
     * 
     * @return the Radiometer Brightness Temperature Interpretation
     */
    public Long getRbif() {
        return rbif;
    }

    /**
     * @param rbif
     *            the Radiometer Brightness Temperature Interpretation to set
     */
    public void setRbif(Long rbif) {
        this.rbif = rbif;
    }

    /**
     * Get the Ice Presence Indicator.
     * 
     * @return the Ice Presence Indicator
     */
    public Long getIpin() {
        return ipin;
    }

    /**
     * @param ipin
     *            the Ice Presence Indicator to set
     */
    public void setIpin(Long ipin) {
        this.ipin = ipin;
    }

    /**
     * Get the Auxillliary Altimeter State Flags.
     * 
     * @return the Auxillliary Altimeter State Flags
     */
    public Long getAasf() {
        return aasf;
    }

    /**
     * @param aasf
     *            the Auxillliary Altimeter State Flags to set
     */
    public void setAasf(Long aasf) {
        this.aasf = aasf;
    }

    /**
     * Get the Meteorological Map Availablity.
     * 
     * @return the Meteorological Map Availablity
     */
    public Long getMmap() {
        return mmap;
    }

    /**
     * @param mmap
     *            the Meteorological Map Availablity to set
     */
    public void setMmap(Long mmap) {
        this.mmap = mmap;
    }

    /**
     * Get the Interpolation Flag For Mean Diurnal Tide.
     * 
     * @return the Interpolation Flag For Mean Diurnal Tide
     */
    public Long getIfdt() {
        return ifdt;
    }

    /**
     * @param ifdt
     *            the Interpolation Flag For Mean Diurnal Tide to set
     */
    public void setIfdt(Long ifdt) {
        this.ifdt = ifdt;
    }

    /**
     * Get the Ku Band Ocean Range
     * 
     * @param kbor
     *            Ku Band Ocean Range to return
     */
    public Double getKbor() {
        return kbor;
    }

    /**
     * @param kbor
     *            Ku Band Ocean Range to set
     */
    public void setKbor(Double kbor) {
        this.kbor = kbor;
    }

    /**
     * Get the RMS of 20 Hz Ku Band Ocean Range
     * 
     * @param rkbor
     *            RMS of 20 Hz Ku Band Ocean Range to return
     */
    public Double getRkbor() {
        return rkbor;
    }

    /**
     * @param rkbor
     *            RMS of 20 Hz Ku Band Ocean Range to set
     */
    public void setRkbor(Double rkbor) {
        this.rkbor = rkbor;
    }

    /**
     * Get the Number of 20 Hz valid points for Ku Band.
     * 
     * @return the Number of 20 Hz valid points for Ku Band
     */
    public Long getNvpk2() {
        return nvpk2;
    }

    /**
     * @param nvpk2
     *            the Number of 20 Hz valid points for Ku Band to set
     */
    public void setNvpk2(Long nvpk2) {
        this.nvpk2 = nvpk2;
    }

    /**
     * Get the Ku Band Net Instrumental Correction
     * 
     * @param kbic
     *            Ku Band Net Instrumental Correction to return
     */
    public Double getKbic() {
        return kbic;
    }

    /**
     * @param kbic
     *            Ku Band Net Instrumental Correction to set
     */
    public void setKbic(Double kbic) {
        this.kbic = kbic;
    }

    /**
     * Get the Sea State Bias Correction On Ku Band
     * 
     * @param sbck
     *            Sea State Bias Correction On Ku Band to return
     */
    public Double getSbck() {
        return sbck;
    }

    /**
     * @param sbck
     *            Sea State Bias Correction On Ku Band to set
     */
    public void setSbck(Double sbck) {
        this.sbck = sbck;
    }

    /**
     * Get Ku Band Significant Wave Height
     * 
     * @param kbsw
     *            Ku Band Significant Wave Height to return
     */
    public Double getKbsw() {
        return kbsw;
    }

    /**
     * @param kbsw
     *            Ku Band Significant Wave Height to set
     */
    public void setKbsw(Double kbsw) {
        this.kbsw = kbsw;
    }

    /**
     * RMS 20 Hz Ku Band Significant Wave Height
     * 
     * @param rksw
     *            RMS 20 Hz Ku Band Significant Wave Height
     */
    public Double getRksw() {
        return rksw;
    }

    /**
     * @param rksw
     *            20 Hz RMS Ku Band Significant Wave Height to set
     */
    public void setRksw(Double rksw) {
        this.rksw = rksw;
    }

    /**
     * Get the Number of 20 Hz valid points for Ku Band Significant Wave Height.
     * 
     * @return the Number of 20 Hz valid points for Ku Band Significant Wave
     *         Height
     */
    public Long getNvksw() {
        return nvksw;
    }

    /**
     * @param nvksw
     *            the Number of 20 Hz valid points for Ku Band Significant Wave
     *            Height to set
     */
    public void setNvksw(Long nvksw) {
        this.nvksw = nvksw;
    }

    /**
     * RMS 20 Hz Ku Band Net Instrumental Correction For Significant Wave Height
     * 
     * @param kncs
     *            RMS 20 Hz Ku Band Net Instrumental Correction For Significant
     *            Wave Height
     */
    public Double getKncs() {
        return kncs;
    }

    /**
     * @param kncs
     *            20 Hz RMS Ku Band Net Instrumental Correction For Significant
     *            Wave Height to set
     */
    public void setKncs(Double kncs) {
        this.kncs = kncs;
    }

    /**
     * Ku Band Corrected Ocean Backscatter coefficient
     * 
     * @param kobc
     *            Ku Band Corrected Ocean Backscatter coefficient
     */
    public Double getKobc() {
        return kobc;
    }

    /**
     * @param kobc
     *            Ku Band Corrected Ocean Backscatter coefficient to set
     */
    public void setKobc(Double kobc) {
        this.kobc = kobc;
    }

    /**
     * STD Ku Band Corrected Ocean Backscatter coefficient
     * 
     * @param kobc
     *            STD Ku Band Corrected Ocean Backscatter coefficient
     */
    public Double getSkobc() {
        return skobc;
    }

    /**
     * @param skobc
     *            STD Ku Band Corrected Ocean Backscatter coefficient to set
     */
    public void setSkobc(Double skobc) {
        this.skobc = skobc;
    }

    /**
     * Get the Number of valid points for Ku Band Backscatter.
     * 
     * @return the Number of valid points for Ku Band Backscatter
     */
    public Long getNvpkb() {
        return nvpkb;
    }

    /**
     * @param nvpkb
     *            the Number of valid points for Ku Band Backscatter to set
     */
    public void setNvpkb(Long nvpkb) {
        this.nvpkb = nvpkb;
    }

    /**
     * Ku band net instrumental correction for AGC
     * 
     * @param knic
     *            Ku band net instrumental correction for AGC
     */
    public Double getKnic() {
        return knic;
    }

    /**
     * @param knic
     *            Ku band net instrumental correction for AGC to set
     */
    public void setKnic(Double knic) {
        this.knic = knic;
    }

    /**
     * Attenuation Correction 1 On Sigma-0
     * 
     * @param acrs1
     *            Attenuation 1 Correction On Sigma-0
     */
    public Double getAcrs1() {
        return acrs1;
    }

    /**
     * @param acrs1
     *            Attenuation Correction 1 On Sigma-0
     */
    public void setAcrs1(Double acrs1) {
        this.acrs1 = acrs1;
    }

    /**
     * Attenuation Correction 2 On Sigma-0
     * 
     * @param acrs2
     *            Attenuation Correction 2 On Sigma-0
     */
    public Double getAcrs2() {
        return acrs2;
    }

    /**
     * @param acrs2
     *            Attenuation Correction 2 On Sigma-0
     */
    public void setAcrs2(Double acrs2) {
        this.acrs2 = acrs2;
    }

    /**
     * Ku Band Automatic Gain Control
     * 
     * @param kagc
     *            Ku Band Automatic Gain Control
     */
    public Double getKagc() {
        return kagc;
    }

    /**
     * @param kagc
     *            Ku Band Automatic Gain Control
     */
    public void setKagc(Double kagc) {
        this.kagc = kagc;
    }

    /**
     * RMS Ku Band Automatic Gain Control
     * 
     * @param rkagc
     *            RMS Ku Band Automatic Gain Control
     */
    public Double getRkagc() {
        return rkagc;
    }

    /**
     * @param rkagc
     *            RMS Ku Band Automatic Gain Control
     */
    public void setRkagc(Double rkagc) {
        this.rkagc = rkagc;
    }

    /**
     * Get the Number of valid points for Ku Band Automatic Gain Control.
     * 
     * @return nvkg the Number of valid points for Ku Band Automatic Gain
     *         Control
     */
    public Long getNvkg() {
        return nvkg;
    }

    /**
     * @param nvkg
     *            the Number of valid points for Ku Band Automatic Gain Control
     *            to set
     */
    public void setNvkg(Long nvkg) {
        this.nvkg = nvkg;
    }

    /**
     * C Band Ocean Range
     * 
     * @param cbor
     *            C Band Ocean Range to return
     */
    public Double getCbor() {
        return cbor;
    }

    /**
     * @param cbor
     *            C Band Ocean Range to set
     */
    public void setCbor(Double cbor) {
        this.cbor = cbor;
    }

    /**
     * RMS of C Band Ocean Range
     * 
     * @param rcbor
     *            RMS of C Band Ocean Range to return
     */
    public Double getRcbor() {
        return rcbor;
    }

    /**
     * @param rcbor
     *            RMS of C Band Ocean Range to set
     */
    public void setRcbor(Double rcbor) {
        this.rcbor = rcbor;
    }

    /**
     * Get the Number of Number of 20 Hz valid points for C band
     * 
     * @return nvpc the Number of 20 Hz valid points for C band
     */
    public Long getNvpc() {
        return nvpc;
    }

    /**
     * @param nvpc
     *            the Number of 20 Hz valid points for C band to set
     */
    public void setNvpc(Long nvpc) {
        this.nvpc = nvpc;
    }

    /**
     * C Band net instrumental correction
     * 
     * @param cbic
     *            C band net instrumental correction to return
     */
    public Double getCbic() {
        return cbic;
    }

    /**
     * @param cbic
     *            C band net instrumental correction to set
     */
    public void setCbic(Double cbic) {
        this.cbic = cbic;
    }

    /**
     * Sea state bias correction on C band
     * 
     * @param sbcc
     *            Sea state bias correction on C band to return
     */
    public Double getSbcc() {
        return sbcc;
    }

    /**
     * @param sbcc
     *            Sea state bias correction on C band to set
     */
    public void setSbcc(Double sbcc) {
        this.sbcc = sbcc;
    }

    /**
     * C band significant wave height
     * 
     * @param cbsw
     *            C band significant wave height to return
     */
    public Double getCbsw() {
        return cbsw;
    }

    /**
     * @param cbsw
     *            C band significant wave height to set
     */
    public void setCbsw(Double cbsw) {
        this.cbsw = cbsw;
    }

    /**
     * RMS 20 Hz C band significant wave height
     * 
     * @param rcsw
     *            RMS 20 Hz C band significant wave height to return
     */
    public Double getRcsw() {
        return rcsw;
    }

    /**
     * @param rcsw
     *            RMS 20 Hz C band significant wave height to set
     */
    public void setRcsw(Double rcsw) {
        this.rcsw = rcsw;
    }

    /**
     * Get the Number of 20 Hz valid points for C band significance
     * 
     * @return nvcsw the Number of 20 Hz valid points for C band significance
     */
    public Long getNvcsw() {
        return nvcsw;
    }

    /**
     * @param nvcsw
     *            the Number of 20 Hz valid points for C band significance to
     *            set
     */
    public void setNvcsw(Long nvcsw) {
        this.nvcsw = nvcsw;
    }

    /**
     * C band net instrumental correction for significance
     * 
     * @param cncs
     *            C band net instrumental correction for significance to return
     */
    public Double getCncs() {
        return cncs;
    }

    /**
     * @param cncs
     *            C band net instrumental correction for significance to set
     */
    public void setCncs(Double cncs) {
        this.cncs = cncs;
    }

    /**
     * C band corrected ocean backscatter coefficient
     * 
     * @param ccob
     *            C band corrected ocean backscatter coefficient to return
     */
    public Double getCcob() {
        return ccob;
    }

    /**
     * @param ccob
     *            C band corrected ocean backscatter coefficient to set
     */
    public void setCcob(Double ccob) {
        this.ccob = ccob;
    }

    /**
     * RMS C band corrected ocean backscatter coefficient
     * 
     * @param rccob
     *            RMS C band corrected ocean backscatter coefficient to return
     */
    public Double getRccob() {
        return rccob;
    }

    /**
     * @param rccob
     *            RMS C band corrected ocean backscatter coefficient to set
     */
    public void setRccob(Double rccob) {
        this.rccob = rccob;
    }

    /**
     * Get the Number of valid points for C band backscatter
     * 
     * @return nvpcb the Number of valid points for C band backscatter
     */
    public Long getNvpcb() {
        return nvpcb;
    }

    /**
     * @param nvpcb
     *            the Number of valid points for C band backscatter to set
     */
    public void setNvpcb(Long nvpcb) {
        this.nvpcb = nvpcb;
    }

    /**
     * RMS C band net instrumental correction for AGC
     * 
     * @param cnia
     *            C band net instrumental correction for AGC to return
     */
    public Double getCnia() {
        return cnia;
    }

    /**
     * @param cnia
     *            C band net instrumental correction for AGC to set
     */
    public void setCnia(Double cnia) {
        this.cnia = cnia;
    }

    /**
     * C band automatic gain control
     * 
     * @param cagc
     *            C band automatic gain control to return
     */
    public Double getCagc() {
        return cagc;
    }

    /**
     * @param cagc
     *            C band automatic gain control to set
     */
    public void setCagc(Double cagc) {
        this.cagc = cagc;
    }

    /**
     * RMS C band automatic gain control
     * 
     * @param rcagc
     *            RMS C band automatic gain control to return
     */
    public Double getRcagc() {
        return rcagc;
    }

    /**
     * @param rcagc
     *            RMS C band automatic gain control to set
     */
    public void setRcagc(Double rcagc) {
        this.rcagc = rcagc;
    }

    /**
     * Get the Number of valid points for C band automatic gain
     * 
     * @return nvpca the Number of valid points for C band automatic gain
     */
    public Long getNvpca() {
        return nvpca;
    }

    /**
     * @param nvpca
     *            the Number of valid points for C band automatic gain to set
     */
    public void setNvpca(Long nvpca) {
        this.nvpca = nvpca;
    }

    /**
     * Satellite channel center frequency 1
     * 
     * @param sccf1
     *            Satellite channel center frequency 1
     */
    public Double getSccf1() {
        return sccf1;
    }

    /**
     * @param sccf1
     *            Satellite channel center frequency 1
     */
    public void setSccf1(Double sccf1) {
        this.sccf1 = sccf1;
    }

    /**
     * Satellite channel center frequency 1
     * 
     * @param sccf2
     *            Satellite channel center frequency 2
     */
    public Double getSccf2() {
        return sccf2;
    }

    /**
     * @param sccf2
     *            Satellite channel center frequency 2
     */
    public void setSccf2(Double sccf2) {
        this.sccf2 = sccf2;
    }

    /**
     * Satellite channel center frequency 3
     * 
     * @param sccf1
     *            Satellite channel center frequency 3
     */
    public Double getSccf3() {
        return sccf3;
    }

    /**
     * @param sccf3
     *            Satellite channel center frequency 3
     */
    public void setSccf3(Double sccf3) {
        this.sccf3 = sccf3;
    }

    /**
     * Brightness temperature 1
     * 
     * @param tmbrst1
     *            Brightness temperature 1
     */
    public Double getTmbrst1() {
        return tmbrst1;
    }

    /**
     * @param tmbrst1
     *            Brightness temperature 1
     */
    public void setTmbrst1(Double tmbrst1) {
        this.tmbrst1 = tmbrst1;
    }

    /**
     * Brightness temperature 2
     * 
     * @param tmbrst2
     *            Brightness temperature 2
     */
    public Double getTmbrst2() {
        return tmbrst2;
    }

    /**
     * @param tmbrst2
     *            Brightness temperature 2
     */
    public void setTmbrst2(Double tmbrst2) {
        this.tmbrst2 = tmbrst2;
    }

    /**
     * Brightness temperature 3
     * 
     * @param tmbrst3
     *            Brightness temperature 3
     */
    public Double getTmbrst3() {
        return tmbrst3;
    }

    /**
     * @param tmbrst3
     *            Brightness temperature 3
     */
    public void setTmbrst3(Double tmbrst3) {
        this.tmbrst3 = tmbrst3;
    }

    /**
     * Radiometer water vapor content
     * 
     * @param rwvc
     *            Radiometer water vapor content to return
     */
    public Double getRwvc() {
        return rwvc;
    }

    /**
     * @param rwvc
     *            Radiometer water vapor content to set
     */
    public void setRwvc(Double rwvc) {
        this.rwvc = rwvc;
    }

    /**
     * Radiometer liquid content
     * 
     * @param rlqc
     *            Radiometer liquid content to return
     */
    public Double getRlqc() {
        return rlqc;
    }

    /**
     * @param rlqc
     *            Radiometer liquid content to set
     */
    public void setRlqc(Double rlqc) {
        this.rlqc = rlqc;
    }

    /**
     * Height or altitude 1
     * 
     * @param hmsl
     *            Height or altitude 1 to return
     */
    public Double getHmsl1() {
        return hmsl1;
    }

    /**
     * @param hmsl
     *            Height or altitude 1 to set
     */
    public void setHmsl1(Double hmsl1) {
        this.hmsl1 = hmsl1;
    }

    /**
     * Height or altitude 2
     * 
     * @param hmsl
     *            Height or altitude 2 to return
     */
    public Double getHmsl2() {
        return hmsl2;
    }

    /**
     * @param hmsl
     *            Height or altitude 2 to set
     */
    public void setHmsl2(Double hmsl2) {
        this.hmsl2 = hmsl2;
    }

    /**
     * Wind speed from altimeter (m/s)
     * 
     * @param wspa
     *            Wind speed from altimeter to return
     */
    public Double getWspa() {
        return wspa;
    }

    /**
     * @param wspa
     *            Wind speed from altimeter to set
     */
    public void setWspa(Double wspa) {
        this.wspa = wspa;
    }

    /**
     * Wind speed from radiometer (m/s)
     * 
     * @param wspr
     *            Wind speed from radiometer to return
     */
    public Double getWspr() {
        return wspr;
    }

    /**
     * @param wspr
     *            Wind speed from radiometer to set
     */
    public void setWspr(Double wspr) {
        this.wspr = wspr;
    }

    /**
     * u-component of the model wind vector (m/s)
     * 
     * @param umwv
     *            u-component of the model wind vector to return
     */
    public Double getUmwv() {
        return umwv;
    }

    /**
     * @param umwv
     *            u-component of the model wind vector to set
     */
    public void setUmwv(Double umwv) {
        this.umwv = umwv;
    }

    /**
     * u-component of the model wind vector (m/s)
     * 
     * @param vwmv
     *            u-component of the model wind vector to return
     */
    public Double getVwmv() {
        return vwmv;
    }

    /**
     * @param vwmv
     *            u-component of the model wind vector to set
     */
    public void setVwmv(Double vwmv) {
        this.vwmv = vwmv;
    }

    /**
     * Mean dynamic topography (m)
     * 
     * @param 1G Mean dynamic topography to return
     */
    public Double getMdyt() {
        return mdyt;
    }

    /**
     * @param mdyt
     *            Mean dynamic topography to set
     */
    public void setMdyt(Double mdyt) {
        this.mdyt = mdyt;
    }

    /**
     * Altitude of COG above reference ellipsoid (m)
     * 
     * @param alre
     *            Altitude of COG above reference ellipsoid
     */
    public Double getAlre() {
        return alre;
    }

    /**
     * @param alre
     *            Altitude of COG above reference ellipsoid to set
     */
    public void setAlre(Double alre) {
        this.alre = alre;
    }

    /**
     * Instantaneous altitude rate (m/s)
     * 
     * @param ialr
     *            Instantaneous altitude rate to return
     */
    public Double getIalr() {
        return ialr;
    }

    /**
     * @param ialr
     *            Instantaneous altitude rate to set
     */
    public void setIalr(Double ialr) {
        this.ialr = ialr;
    }

    /**
     * Squared off nadir angle of the satellite from platform data (degree**2)
     * 
     * @param onap
     *            Squared off nadir angle of the satellite from platform data to
     *            set
     */
    public Double getOnap() {
        return onap;
    }

    /**
     * @param onap
     *            Squared off nadir angle of the satellite from platform data to
     *            set
     */
    public void setOnap(Double onap) {
        this.onap = onap;
    }

    /**
     * Squared off nadir angle of the satellite from waveform data (degree**2)
     * 
     * @param sonaw
     *            Squared off nadir angle of the satellite from waveform data to
     *            return
     */
    public Double getSonaw() {
        return sonaw;
    }

    /**
     * @param sonaw
     *            Squared off nadir angle of the satellite from waveform data to
     *            set
     */
    public void setSonaw(Double sonaw) {
        this.sonaw = sonaw;
    }

    /**
     * Ionospheric correction from model on Ku band (m)
     * 
     * @param icmk
     *            Ionospheric correction from model on Ku band to return
     */
    public Double getIcmk() {
        return icmk;
    }

    /**
     * @param icmk
     *            Ionospheric correction from model on Ku band to set
     */
    public void setIcmk(Double icmk) {
        this.icmk = icmk;
    }

    /**
     * Ionospheric correction from model on Ku band (m)
     * 
     * @param aick
     *            Ionospheric correction from model on Ku band to return
     */
    public Double getAick() {
        return aick;
    }

    /**
     * @param aick
     *            Ionospheric correction from model on Ku band to set
     */
    public void setAick(Double aick) {
        this.aick = aick;
    }

    /**
     * Model dry tropospheric correction (m)
     * 
     * @param mdtc
     *            Model dry tropospheric correction to return
     */
    public Double getMdtc() {
        return mdtc;
    }

    /**
     * @param mdtc
     *            Model dry tropospheric correction to set
     */
    public void setMdtc(Double mdtc) {
        this.mdtc = mdtc;
    }

    /**
     * Model wet tropospheric correction (m)
     * 
     * @param mwtc
     *            Model wet tropospheric correction to return
     */
    public Double getMwtc() {
        return mwtc;
    }

    /**
     * @param mwtc
     *            Model wet tropospheric correction to set
     */
    public void setMwtc(Double mwtc) {
        this.mwtc = mwtc;
    }

    /**
     * Radiometer wet tropospheric correction (m)
     * 
     * @param rwtc
     *            Radiometer wet tropospheric correction to return
     */
    public Double getRwtc() {
        return rwtc;
    }

    /**
     * @param rwtc
     *            Radiometer wet tropospheric correction to set
     */
    public void setRwtc(Double rwtc) {
        this.rwtc = rwtc;
    }

    /**
     * Mean sea-surface height (m)
     * 
     * @param mssh
     *            Mean sea-surface height to return
     */
    public Double getMssh() {
        return mssh;
    }

    /**
     * @param mssh
     *            Mean sea-surface height to set
     */
    public void setMssh(Double mssh) {
        this.mssh = mssh;
    }

    /**
     * Mean sea-surface height from altimeter only (m)
     * 
     * @param msha
     *            Mean sea-surface height from altimeter only to return
     */
    public Double getMsha() {
        return msha;
    }

    /**
     * @param msha
     *            Mean sea-surface height from altimeter only to set
     */
    public void setMsha(Double msha) {
        this.msha = msha;
    }

    /**
     * Geoid's height (m)
     * 
     * @param geodh
     *            Geoid's height to return
     */
    public Double getGeodh() {
        return geodh;
    }

    /**
     * @param odle
     *            Ocean depth/land elevation (m)
     */
    public void setGeodh(Double geodh) {
        this.geodh = geodh;
    }

    /**
     * Ocean depth/land elevation (m)
     * 
     * @param odle
     *            Ocean depth/land elevation to return
     */
    public Double getOdle() {
        return odle;
    }

    /**
     * @param odle
     *            Ocean depth/land elevation to set
     */
    public void setOdle(Double odle) {
        this.odle = odle;
    }

    /**
     * Solid earth tide height (m)
     * 
     * @param seth
     *            Solid earth tide height to return
     */
    public Double getSeth() {
        return seth;
    }

    /**
     * @param seth
     *            Solid earth tide height to set
     */
    public void setSeth(Double seth) {
        this.seth = seth;
    }

    /**
     * Total geocentric ocean tide height solution 1 (m)
     * 
     * @param tgoth1
     *            Total geocentric ocean tide height solution 1 to return
     */
    public Double getTgoth1() {
        return tgoth1;
    }

    /**
     * @param tgoth1
     *            Total geocentric ocean tide height solution 1 to set
     */
    public void setTgoth1(Double tgoth1) {
        this.tgoth1 = tgoth1;
    }

    /**
     * Total geocentric ocean tide height solution 2 (m)
     * 
     * @param tgoth2
     *            Total geocentric ocean tide height solution 2 to return
     */
    public Double getTgoth2() {
        return tgoth2;
    }

    /**
     * @param tgoth2
     *            Total geocentric ocean tide height solution 2 to set
     */
    public void setTgoth2(Double tgoth2) {
        this.tgoth2 = tgoth2;
    }

    /**
     * Loading tide height geocentric ocean tide solution 1 (m)
     * 
     * @param lths1
     *            Loading tide height geocentric ocean tide solution 1 to return
     */
    public Double getLths1() {
        return lths1;
    }

    /**
     * @param lths1
     *            Loading tide height geocentric ocean tide solution 1 to set
     */
    public void setLths1(Double lths1) {
        this.lths1 = lths1;
    }

    /**
     * Loading tide height geocentric ocean tide solution 2 (m)
     * 
     * @param lths2
     *            Loading tide height geocentric ocean tide solution 2 to return
     */
    public Double getLths2() {
        return lths2;
    }

    /**
     * @param lths2
     *            Loading tide height geocentric ocean tide solution 2 to set
     */
    public void setLths2(Double lths2) {
        this.lths2 = lths2;
    }

    /**
     * Long period tide height (m)
     * 
     * @param lpth
     *            Long period tide height to return
     */
    public Double getLpth() {
        return lpth;
    }

    /**
     * @param lpth
     *            Long period tide height to set
     */
    public void setLpth(Double lpth) {
        this.lpth = lpth;
    }

    /**
     * Non-equilibrium long period tide height (m)
     * 
     * @param nlth
     *            Non-equilibrium long period tide height to return
     */
    public Double getNlth() {
        return nlth;
    }

    /**
     * @param nlth
     *            Non-equilibrium long period tide height to set
     */
    public void setNlth(Double nlth) {
        this.nlth = nlth;
    }

    /**
     * Geocentric pole tide height (m)
     * 
     * @param gpth
     *            Geocentric pole tide height to return
     */
    public Double getGpth() {
        return gpth;
    }

    /**
     * @param gpth
     *            Geocentric pole tide height to set
     */
    public void setGpth(Double gpth) {
        this.gpth = gpth;
    }

    /**
     * Inverted barometer correction (m)
     * 
     * @param ibco
     *            Inverted barometer correction to return
     */
    public Double getIbco() {
        return ibco;
    }

    /**
     * @param ibco
     *            Inverted barometer correction to set
     */
    public void setIbco(Double ibco) {
        this.ibco = ibco;
    }

    /**
     * High frequency fluctuations of the sea surface topography correction (m)
     * 
     * @param hfstc
     *            High frequency fluctuations of the sea surface topography
     *            correction
     * 
     */
    public Double getHfstc() {
        return hfstc;
    }

    /**
     * @param hfstc
     *            High frequency fluctuations of the sea surface topography
     *            correction to set
     */
    public void setHfstc(Double hfstc) {
        this.hfstc = hfstc;
    }

    /**
     * Sea Surface Height Anomoly
     * 
     * @param ssha
     *            Sea Surface Height Anomoly to return
     */
    public Double getSsha() {
        return ssha;
    }

    /**
     * @param ssha
     *            Sea Surface Height Anomoly to set
     */
    public void setSsha(Double ssha) {
        this.ssha = ssha;
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
        return "ssha";
    }
}
