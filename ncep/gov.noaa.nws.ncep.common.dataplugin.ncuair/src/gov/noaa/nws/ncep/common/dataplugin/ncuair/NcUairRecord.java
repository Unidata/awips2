/**
 * NcUairRecord
 * 
 * <pre>
 * The java class defines the parameters in the postgres table and
 * the for HDF5 dataset for the upper air data.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin     	Initial creation
 * 4/2011						T. Lee		Persist to HDF5
 * 09/2011      457             S. Gurung   Renamed H5 to Nc and h5 to nc
 * 09/2011                   	Chin Chen   support batch decoding methods for better performance and
 * 											remove xml serialization as well
 * 10/2011                      S. Gurung   Replace slat/slon/selv with location of type SurfaceObsLocation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/2013      1293            bkowal      Removed references to hdffileid. 
 * Apr 12, 2013 1857            bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869            bsteffen    Remove dataURI column from
 *                                          PluginDataObject.
 * Dec 03, 2013 2551            rjpeter     Remove get/setPersistenceTime
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.ncuair;

import java.util.Calendar;
import java.util.Collection;
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
import javax.xml.bind.annotation.XmlElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
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

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ncuairseq")
@Table(name = "ncuair", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "ncuair", indexes = { @Index(name = "ncuair_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class NcUairRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IDecoderGettable, IPointData, IPersistable {

    private static final long serialVersionUID = 1L;

    public static final String PLUGIN_NAME = "ncuair";

    // Time of the UTC
    @DataURI(position = 4)
    @Column
    @DynamicSerializeElement
    private int UTC;

    // The observation report type.
    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    private String reportType;

    // Issue time for the bulletin
    @Column
    @DynamicSerializeElement
    private Calendar issueTime;

    // Observation time for the bulletin
    @Column
    @DynamicSerializeElement
    private Calendar obsTime;

    // Synoptic time for the bulletin
    // Time of the observation to the nearest hour.
    @Column
    @DynamicSerializeElement
    private Calendar synopticTime;

    // Type of data such as TTAA/BB/CC/DD or PP...
    @DataURI(position = 3)
    @Column
    @DynamicSerializeElement
    private String dataType;

    // Correction indicator from wmo header
    @DataURI(position = 5)
    @Column
    @DynamicSerializeElement
    private String corr;

    // Text of the WMO header
    @Column
    @DynamicSerializeElement
    private String wmoHeader;

    // Station number
    // @DataURI(position = 2)
    @Column
    @DynamicSerializeElement
    private String stnum;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @XmlElement
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    // Yes if report is a NIL.
    @Column
    @DynamicSerializeElement
    private Boolean nil;

    // bulletin message
    @Transient
    @DynamicSerializeElement
    private String bullMessage;

    /**
     * Uair observation levels
     */
    @DynamicSerializeElement
    @Transient
    private Set<NcUairObsLevels> obsLevels = new HashSet<NcUairObsLevels>();

    /**
     * Uair tropopause data
     */
    @DynamicSerializeElement
    @Transient
    private Set<NcUairTropopause> tropopause = new HashSet<NcUairTropopause>();

    /**
     * Uair maxwind data
     */
    @DynamicSerializeElement
    @Transient
    private Set<NcUairMaxWind> maxwind = new HashSet<NcUairMaxWind>();

    /**
     * Uair lifted index data
     */
    @DynamicSerializeElement
    @Transient
    private Set<NcUairLiftedIndex> liftedindex = new HashSet<NcUairLiftedIndex>();

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    /**
     * Empty constructor.
     */
    public NcUairRecord() {
        this.nil = false;
        this.stnum = "";
        this.wmoHeader = "";
        // this.stid="";
        this.corr = "";
        this.dataType = "";
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
    public NcUairRecord(String uri) {
        super(uri);
        if (location != null) {
            String staId = location.getStationId();
            location.setStationId("null".equals(staId) ? null : staId);
        }
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
     * Get the report correction indicator.
     * 
     * @return The corr
     */
    public String getCorr() {
        return corr;
    }

    /**
     * Set the report correction indicator.
     * 
     * @param corr
     *            The corr.
     */
    public void setCorr(String corr) {
        this.corr = corr;
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

    public String getStnum() {
        return stnum;
    }

    public void setStnum(String stnum) {
        this.stnum = stnum;
    }

    /*
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
     * Get whether the location for this observation is defined.
     * 
     * @return Is this location defined.
     */
    public Boolean getLocationDefined() {
        return location.getLocationDefined();
    }

    public Calendar getObsTime() {
        return obsTime;
    }

    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    public Calendar getSynopticTime() {
        return synopticTime;
    }

    public void setSynopticTime(Calendar synopticTime) {
        this.synopticTime = synopticTime;
    }

    public String getBullMessage() {
        return bullMessage;
    }

    public void setBullMessage(String bullMessage) {
        this.bullMessage = bullMessage;
    }

    public Calendar getIssueTime() {
        return issueTime;
    }

    public void setIssueTime(Calendar issueTime) {
        this.issueTime = issueTime;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public int getUTC() {
        return UTC;
    }

    public void setUTC(int utc) {
        UTC = utc;
    }

    public Boolean getNil() {
        return nil;
    }

    public void setNil(Boolean nil) {
        this.nil = nil;
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @return the set of uair observation levels
     */
    public Set<NcUairObsLevels> getObsLevels() {
        return obsLevels;
    }

    /**
     * @param uair
     *            observation levels to set
     */
    public void setObsLevels(Set<NcUairObsLevels> uairLevel) {
        this.obsLevels = uairLevel;
    }

    /**
     * @param add
     *            uair observation levels to set
     */
    public void addObsLevels(NcUairObsLevels plevel) {
        obsLevels.add(plevel);
        // plevel.setParentID(this);
    }

    /**
     * @return the set of uair observation levels
     */
    public Set<NcUairTropopause> getTropopause() {
        return tropopause;
    }

    /**
     * @param uair
     *            observation levels to set
     */
    public void setTropopause(Set<NcUairTropopause> trop) {
        this.tropopause = trop;
    }

    /**
     * @param add
     *            uair observation levels to set
     */
    public void addTropopause(NcUairTropopause trop) {
        tropopause.add(trop);
        // trop.setParentID(this);
    }

    /**
     * @return the set of uair maximum wind
     */
    public Set<NcUairMaxWind> getMaxWind() {
        return maxwind;
    }

    /**
     * @param uair
     *            maximum wind to set
     */
    public void setMaxWind(Set<NcUairMaxWind> mwind) {
        this.maxwind = mwind;
    }

    /**
     * @param add
     *            uair maximum wind to set
     */
    public void addMaxWind(NcUairMaxWind mwind) {
        maxwind.add(mwind);
        // mwind.setParentID(this);
    }

    /**
     * @return the set of uair lifted index
     */
    public Set<NcUairLiftedIndex> getLiftedIndex() {
        return liftedindex;
    }

    /**
     * @param uair
     *            lifted index to set
     */
    public void setLiftedIndex(Set<NcUairLiftedIndex> li) {
        this.liftedindex = li;
    }

    /**
     * @param add
     *            uair lifted index to set
     */
    public void addLiftedIndex(NcUairLiftedIndex li) {
        liftedindex.add(li);
        // li.setParentID(this);
    }

    /**
     * Override existing set method to modify any classes that use the dataURI
     * as a foreign key
     */
    @Override
    public void setIdentifier(Object dataURI) {

        this.identifier = dataURI;

    }

    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    public SurfaceObsLocation getLocation() {
        if (location == null) {
            location = new SurfaceObsLocation();
        }
        return location;
    }

    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
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
        return "ncuair";
    }
}