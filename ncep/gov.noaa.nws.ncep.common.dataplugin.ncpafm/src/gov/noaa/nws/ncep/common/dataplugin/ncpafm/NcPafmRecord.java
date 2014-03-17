package gov.noaa.nws.ncep.common.dataplugin.ncpafm;

import java.util.Calendar;
import java.util.Date;

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

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * NcPafmRecord is the Data Access component for PAFM Point/Area Forecast
 * Matrices data. This contains getters and setters for the main table "ncpafm".
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 05, 2009 126        F. J. Yen   Initial creation
 * Jan 06, 2010 126        F. J. Yen   Migrated and refactored from to11dr3 to
 *                                     to11dr11
 * 30 Sep 2011  126        B. Hebbard  Initial version in refactored NcPafm
 *                                     decoder.  This new (fine-grain) PDO
 *                                     contains data associated  with one
 *                                     location / forecast hour pair only, for
 *                                     efficiency of retrieval.  It also
 *                                     implements  IPointData -- that is,
 *                                     presents a PointDataView  object suitable
 *                                     for HDF5 persistence.
 * 10 Oct 2011  126        G. Hull     replace stnid,lat&lon with the
 *                                     SurfaceObsLocation.
 * 03 Feb 2012  606        G. Hull     added reportType to the URI for inventory
 *                                     updating
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * 08 Apr 2013  1293       bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 11, 2014 2784       rferrel     Remove override of setIdentifier.
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ncpafmseq")
@Table(name = "ncpafm", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ncpafm", indexes = { @Index(name = "ncpafm_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcPafmRecord extends PersistablePluginDataObject implements
// TODO: Make absolutely sure these are NO LONGER required... ISpatialEnabled,
// IDecoderGettable,
        IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    /** Report type */
    @Column(length = 32)
    @DataURI(position = 6)
    @XmlElement
    @DynamicSerializeElement
    private String reportType;

    // WMO header
    @DataURI(position = 4)
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String wmoHeader;

    // The issue office where the report from
    @Column(length = 32)
    @DataURI(position = 1)
    @XmlElement
    @DynamicSerializeElement
    private String issueOffice;

    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement
    private Calendar issueTime;

    // The stationId here is the FIPS code and not the issuing office
    @Embedded
    @DataURI(position = 3, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Column(length = 8)
    @DynamicSerializeElement
    @XmlElement
    private String designatorBBB;

    // The mndTime
    @Column(length = 72)
    @XmlElement
    @DynamicSerializeElement
    private String mndTime;

    /** Matrix type */
    @DataURI(position = 5)
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String matrixType;

    // The universal geographic code
    @Column(length = 640)
    @XmlElement
    @DynamicSerializeElement
    private String ugc;

    // The product purge time
    @XmlElement
    @DynamicSerializeElement
    private Calendar prodPurgeTime;

    // Text information for this segment
    // @Column(length=12000)
    // @XmlElement
    // @DynamicSerializeElement
    private String segment;

    // // The county FIPS : this is the stationId in the SurfaceObsLocation
    // @Column(length=16)
    // @XmlElement
    // @DynamicSerializeElement
    // private String fips;

    // The group of parameter values associated with the above combination.
    @Transient
    private NcPafmParameters pafmParms;

    // The PointDataView representation of data associated with this PDO
    // to be persisted as HDF5
    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Default Constructor
     */
    public NcPafmRecord() {
        this.location = null;
        this.issueOffice = null;
        this.reportType = "PAFM";
        this.matrixType = " ";
        this.issueTime = null;
        this.wmoHeader = null;
        this.designatorBBB = " ";
        this.mndTime = null;
    }

    /**
     * Constructs a pafm record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */
    public NcPafmRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    public String getReportType() {
        return reportType;
    }

    public void setReportType(String reportType) {
        this.reportType = reportType;
    }

    public String getMatrixType() {
        return matrixType;
    }

    public void setMatrixType(String matrixType) {
        this.matrixType = matrixType;
    }

    public String getWmoHeader() {
        return wmoHeader;
    }

    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    public String getIssueOffice() {
        return issueOffice;
    }

    public void setIssueOffice(String issueOffice) {
        this.issueOffice = issueOffice;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public String getDesignatorBBB() {
        return designatorBBB;
    }

    public void setDesignatorBBB(String designatorBBB) {
        this.designatorBBB = designatorBBB;
    }

    public String getMndTime() {
        return mndTime;
    }

    public void setMndTime(String mndTime) {
        this.mndTime = mndTime;
    }

    /**
     * @return the ugc
     */
    public String getUgc() {
        return ugc;
    }

    /**
     * @param ugc
     *            to set
     */
    public void setUgc(String ugc) {
        this.ugc = ugc;
    }

    /**
     * @return the prodPurgeTime
     */
    public Calendar getProdPurgeTime() {
        return prodPurgeTime;
    }

    /**
     * @param prodPurgeTime
     *            to set
     */
    public void setProdPurgeTime(Calendar prodPurgeTime) {
        this.prodPurgeTime = prodPurgeTime;
    }

    /**
     * @return the segment
     */
    public String getSegment() {
        return segment;
    }

    /**
     * @param segment
     *            to set
     */
    public void setSegment(String segment) {
        this.segment = segment;
    }

    // public String getFips() {
    // return fips;
    // }

    public String getStationId() {
        return location.getStationId();
    }

    // if this needs to be an ISpatialObject
    // @Override
    // public SurfaceObsLocation getSpatialObject() {
    // return location;
    // }

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

    // public void setFips(String fips) {
    // this.fips = fips;
    // }

    /**
     * @return the set of Parameters
     */
    public NcPafmParameters getPafmParms() {
        return pafmParms;
    }

    /**
     * @param pafmParameters
     *            -the set of Parmameters to set
     */
    public void setPafmParms(NcPafmParameters pafmParams) {
        this.pafmParms = pafmParams;
    }

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
    public Date getPersistenceTime() {
        // return this.dataTime.getRefTime();
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
        return "ncpafm";
    }
}
