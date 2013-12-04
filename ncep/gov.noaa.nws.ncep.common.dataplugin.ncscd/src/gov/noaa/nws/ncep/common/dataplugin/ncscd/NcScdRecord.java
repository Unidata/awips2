package gov.noaa.nws.ncep.common.dataplugin.ncscd;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;
import java.util.Collection;

import javax.measure.quantity.Length;
import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * NcScdRecord
 * 
 * This java class performs the mapping to the database tables for SCD.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#  Engineer  Description
 * ------------ -------- --------- ----------------------------------
 * 12/2008      41       T. Lee    Created
 * 04/2009      41       T. Lee    Migrated to TO10
 * 07/2009      41       T. Lee    Migrated to TO11
 * 11/2009      41       T. Lee    Migrated to TO11D6
 * 06/2011      41       F. J. Yen Convert SCD to HDF5 (OB11.5).  Changed type
 *                                 of suspectTimeFlag from Boolean to String
 *                                 since undefined in PointDataDescription.
 * 09/2011      457      S. Gurung Renamed H5 to Nc and h5 to nc
 * Apr 04, 2013 1846     bkowal    Added an index on refTime and forecastTime
 * Apr 04, 2013 1293     bkowal    Removed references to hdffileid.
 * Apr 12, 2013 1857     bgonzale  Added SequenceGenerator annotation.
 * May 07, 2013 1869     bsteffen  Remove dataURI column from PluginDataObject.
 * Aug 30, 2013 2298     rjpeter   Make getPluginName abstract.
 * Dec 03, 2013 2551     rjpeter   Extend PersistablePluginDataObject.
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 * 
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ncscdseq")
@Table(name = "ncscd", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ncscd", indexes = { @Index(name = "ncscd_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcScdRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IDecoderGettable, IPointData {

    private static final long serialVersionUID = 1L;

    public static final Unit<Temperature> TDXC_UNIT = SI.CELSIUS;

    public static final Unit<Length> PRECIP_UNIT = NonSI.INCH;

    /** Report type */
    @XmlElement
    @DynamicSerializeElement
    @Column
    @DataURI(position = 1)
    private String reportType;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    /** Bulletin correction */
    @Column(length = 8)
    @DataURI(position = 3)
    @XmlElement
    @DynamicSerializeElement
    private String corIndicator;

    /** Bulletin observation time */
    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar obsTime;

    /** Bulletin issuance time */
    @Column
    @DataURI(position = 4)
    @XmlElement
    @DynamicSerializeElement
    private Calendar issueTime;

    /** Maximum 24h temperature in Celsius */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float TDXC;

    /** Minimum 24h temperature in Celsius */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float TDNC;

    /** Six hour accumulated precipitation in inches */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float P06I;

    /** Twenty-four hour accumulated precipitation in inches */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float P24I;

    /** Character weather phenomenon */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String WTHR;

    /** Snow depth */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float SNOW;

    /** New snow depth on the ground */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float SNEW;

    /** Total snow depth in a Calendar day */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float S24I;

    /** Water equivalent of snow */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private float WEQS;

    /** Duration of sunshine */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int MSUN;

    /** Low-level cloud genera from WMO Code 0513 */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int CTYL;

    /** Mid-level cloud genera from WMO Code 0513 */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int CTYM;

    /** High-level cloud genera from WMO Code 0513 */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int CTYH;

    /** Fraction of celestial dome covered by cloud from WMO Code 2700 */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int CFRT;

    /**
     * Fraction of celestial dome covered by low or mid cloud from WMO Code 2700
     */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int CFRL;

    /** Cloud base height from WMO Code 1600 */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private int CBAS;

    /** Suspect time flag */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String suspectTimeFlag;

    /** Raw report */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    private String report;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Default constructor
     * 
     */
    public NcScdRecord() {
        this.location = null;
        this.issueTime = null;
        this.corIndicator = "REG";
        this.obsTime = null;
        this.TDXC = IDecoderConstantsN.FLOAT_MISSING;
        this.TDNC = IDecoderConstantsN.FLOAT_MISSING;
        this.P06I = IDecoderConstantsN.FLOAT_MISSING;
        this.P24I = IDecoderConstantsN.FLOAT_MISSING;
        this.WTHR = "";
        this.SNOW = IDecoderConstantsN.FLOAT_MISSING;
        this.SNEW = IDecoderConstantsN.FLOAT_MISSING;
        this.S24I = IDecoderConstantsN.FLOAT_MISSING;
        this.WEQS = IDecoderConstantsN.FLOAT_MISSING;
        this.MSUN = IDecoderConstantsN.INTEGER_MISSING;
        this.CTYL = IDecoderConstantsN.INTEGER_MISSING;
        this.CTYM = IDecoderConstantsN.INTEGER_MISSING;
        this.CTYH = IDecoderConstantsN.INTEGER_MISSING;
        this.CFRT = IDecoderConstantsN.INTEGER_MISSING;
        this.CFRL = IDecoderConstantsN.INTEGER_MISSING;
        this.CBAS = IDecoderConstantsN.INTEGER_MISSING;
        this.suspectTimeFlag = "false";
        this.report = "";
    }

    /**
     * Constructs a SCD record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public NcScdRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public String getStationId() {
        return location.getStationId();
    }

    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    public SurfaceObsLocation getLocation() {
        return location;
    }

    public void setLocation(SurfaceObsLocation obsLoc) {
        this.location = obsLoc;
    }

    public double getLatitude() {
        return location.getLatitude();
    }

    public double getLongitude() {
        return location.getLongitude();
    }

    public Integer getElevation() {
        return location.getElevation();
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public String getCorIndicator() {
        return corIndicator;
    }

    public void setCorIndicator(String corIndicator) {
        this.corIndicator = corIndicator;
    }

    public Calendar getObsTime() {
        return obsTime;
    }

    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    public float getTDXC() {
        return TDXC;
    }

    public void setTDXC(float TDXC) {
        this.TDXC = TDXC;
    }

    public float getTDNC() {
        return TDNC;
    }

    public void setTDNC(float TDNC) {
        this.TDNC = TDNC;
    }

    public float getP06I() {
        return P06I;
    }

    public void setP06I(float P06I) {
        this.P06I = P06I;
    }

    public float getP24I() {
        return P24I;
    }

    public void setP24I(float P24I) {
        this.P24I = P24I;
    }

    public String getWTHR() {
        return WTHR;
    }

    public void setWTHR(String WTHR) {
        this.WTHR = WTHR;
    }

    public float getSNOW() {
        return SNOW;
    }

    public void setSNOW(float SNOW) {
        this.SNOW = SNOW;
    }

    public float getSNEW() {
        return SNEW;
    }

    public void setSNEW(float SNEW) {
        this.SNEW = SNEW;
    }

    public float getS24I() {
        return S24I;
    }

    public void setS24I(float S24I) {
        this.S24I = S24I;
    }

    public float getWEQS() {
        return WEQS;
    }

    public void setWEQS(float WEQS) {
        this.WEQS = WEQS;
    }

    public int getMSUN() {
        return MSUN;
    }

    public void setMSUN(int MSUN) {
        this.MSUN = MSUN;
    }

    public int getCTYL() {
        return CTYL;
    }

    public void setCTYL(int CTYL) {
        this.CTYL = CTYL;
    }

    public int getCTYM() {
        return CTYM;
    }

    public void setCTYM(int CTYM) {
        this.CTYM = CTYM;
    }

    public int getCTYH() {
        return CTYH;
    }

    public void setCTYH(int CTYH) {
        this.CTYH = CTYH;
    }

    public int getCFRT() {
        return CFRT;
    }

    public void setCFRT(int CFRT) {
        this.CFRT = CFRT;
    }

    public int getCFRL() {
        return CFRL;
    }

    public void setCFRL(int CFRL) {
        this.CFRL = CFRL;
    }

    public int getCBAS() {
        return CBAS;
    }

    public void setCBAS(int CBAS) {
        this.CBAS = CBAS;
    }

    public String getReport() {
        return report;
    }

    public void setReport(String report) {
        this.report = report;
    }

    public String getSuspectTimeFlag() {
        return suspectTimeFlag;
    }

    public void setSuspectTimeFlag(String suspectTimeFlag) {
        this.suspectTimeFlag = suspectTimeFlag;
    }

    /**
     * Get the value and units of a named parameter within this observation.
     * 
     * @param paramName
     *            The name of the parameter value to retrieve.
     * @return An Amount with value and units. If the parameter is unknown, a
     *         null reference is returned.
     */

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
     */
    @Override
    public PointDataView getPointDataView() {
        return this.pointDataView;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointData#setPointDataView(com.raytheon
     * .uf.common.pointdata.PointDataView)
     */
    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public Amount getValue(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Collection<Amount> getValues(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getString(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String[] getStrings(String paramName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

    @Override
    public String getPluginName() {
        return "ncscd";
    }
}
