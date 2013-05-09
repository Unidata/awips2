
package gov.noaa.nws.ncep.common.dataplugin.ncccfp;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
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
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * NC_CCFP Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * --------- ----------  ----------- --------------------------
 * 10/05/2009   155         F. J. Yen   From Raytheon's CCFP; mod for NC_CCFP
 * 26/05/2010	155			F. J. Yen	Refactored to dataplugin for migration to to11dr11
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869     bsteffen    Remove dataURI column from
 *                                   PluginDataObject.
 * 
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ncccfpseq")
@Table(name = "ncccfp", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ncccfp",
		indexes = {
				@Index(name = "ncccfp_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcccfpRecord extends PluginDataObject implements ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    @DataURI(position = 2)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar issuetime;

    @DataURI(position = 1)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Calendar validtime;
    
    @Column(length = 8)
    @XmlAttribute
    @DynamicSerializeElement
    private String producttype;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer coverage;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer conf;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer growth;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer tops;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer speed;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer direction;

    @DataURI(position = 3)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer numPts;
    
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Boolean canadaflag;

    /*
     * locationUri contains up to the first seven coordinates.  This is for making the dataURI
     * unique.  (It was suggested that it was highly unlikely that 7 coordinates would not
     * be unique enough.  The number of coordinates was reduced due to the limited length of
     * the string dataURI)
     */
    @DataURI(position = 4)
    @Column(length=150)
    @XmlElement
    @DynamicSerializeElement
    private String locationUri;
    
    private NcccfpLocation location;
 
    /**
     * Default Constructor
     */
    public NcccfpRecord() {
    }

    // /**
    // * Constructor.
    // *
    // * @param message
    // * The text of the message
    // */
    // public NcccfpRecord(String message) {
    // super(message);
    // }

    /**
     * Constructs an ncccfp record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public NcccfpRecord(String uri) {
        super(uri);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        // TODO Auto-generated method stub
        return null;
    }

    public java.util.Calendar getIssuetime() {
        return issuetime;
    }

    public void setIssuetime(java.util.Calendar issuetime) {
        this.issuetime = issuetime;
    }

    public java.util.Calendar getValidtime() {
        return validtime;
    }

    public void setValidtime(java.util.Calendar validtime) {
        this.validtime = validtime;
    }

    public java.lang.String getProducttype() {
        return producttype;
    }

    public void setProducttype(java.lang.String producttype) {
        this.producttype = producttype;
    }

    public double getBoxLat() {
        return location.getBoxLat();
    }

    public double getBoxLong() {
        return location.getBoxLong();
    }

    public Boolean getCanadaflag() {
        return canadaflag;
    }

    public void setCanadaflag(Boolean canadaflag) {
        this.canadaflag = canadaflag;
    }

    /**
     * @return the coverage
     */
    public Integer getCoverage() {
        return coverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public void setCoverage(Integer coverage) {
        this.coverage = coverage;
    }

    /**
     * @return the conf
     */
    public Integer getConf() {
        return conf;
    }

    /**
     * @param conf
     *            the conf to set
     */
    public void setConf(Integer conf) {
        this.conf = conf;
    }

    /**
     * @return the growth
     */
    public Integer getGrowth() {
        return growth;
    }

    /**
     * @param growth
     *            the growth to set
     */
    public void setGrowth(Integer growth) {
        this.growth = growth;
    }

    /**
     * @return the tops
     */
    public Integer getTops() {
        return tops;
    }

    /**
     * @param tops
     *            the tops to set
     */
    public void setTops(Integer tops) {
        this.tops = tops;
    }

    /**
     * @return the speed
     */
    public Integer getSpeed() {
        return speed;
    }

    /**
     * @param speed
     *            the speed to set
     */
    public void setSpeed(Integer speed) {
        this.speed = speed;
    }

    /**
     * @return the direction
     */
    public Integer getDirection() {
        return direction;
    }

    /**
     * @param direction
     *            the direction to set
     */
    public void setDirection(Integer direction) {
        this.direction = direction;
    } 
    
    /**
     * @return the number of points
     */
    public Integer getNumPts() {
        return numPts;
    }

    /**
     * @param numPts
     *            the number of points
     */
    public void setNumPts(Integer numPts) {
        this.numPts = numPts;
    }
    @Override
    public NcccfpLocation getSpatialObject() {
        return location;
    }

    public String getLocationUri() {
        return locationUri;
    }

    public void setLocationUri(String locationUri) {
        this.locationUri = locationUri;
    }

    public NcccfpLocation getLocation() {
        return location;
    }

    public void setLocation (NcccfpLocation location) {
        this.location = location;
	}

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

}
