/**
 * ConvsigmetSection
 * 
 * This java class defines the getters and setters for the 
 *      convective sigmet section table.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009      87/114			L. Lin     	Initial coding
 * 07/2009		87/114		    L. Lin		Migration to TO11
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.convsigmet;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.io.Serializable;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@Entity
@Table(name = "convsigmet_section")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ConvSigmetSection implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Integer recordId = null;

    // The CONVSIGMET record this object belongs to
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private ConvSigmetRecord parentID;

    // Class type such as: LINE, AREA, ISOL, CS, and OUTLOOK
    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String classType;

    // Sequence ID of a convective sigmet report
    @Column(length = 16)
    @XmlElement
    @DynamicSerializeElement
    private String sequenceID;

    // Start time of the report
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar startTime;

    // End time of the report
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Calendar endTime;

    /*
     * Intensity is DMSHG (weakening), DSIPTG (ending) INTSFYG (strengthening),
     * DVLPG (beginning/growing)
     */
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String intensity;

    // To where flight level from tops
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int flightLevel;

    // TOPS TO or TOPS ABV for flight level
    @Column(length = 16)
    @XmlElement
    @DynamicSerializeElement
    private String cloudTop;

    // Direction of weather report heads to
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int direction;

    // wind speed in knots
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int speed;

    // Distance is the Distance or Area diameter of the area or line
    @Column
    @XmlElement
    @DynamicSerializeElement
    private int distance;

    // The information of a complete section
    @Column(length = 720)
    @XmlElement
    @DynamicSerializeElement
    private String segment;

    /**
     * Convsigmet location
     */
    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    @Index(name = "convsigmet_location_parentid_idex")
    private Set<ConvSigmetLocation> convSigmetLocation = new HashSet<ConvSigmetLocation>();

    /**
     * No-Arg Convstructor
     */
    public ConvSigmetSection() {
        this.intensity = null;
        this.cloudTop = null;
        this.segment = null;
        this.classType = null;
        this.sequenceID = null;

        this.direction = IDecoderConstantsN.INTEGER_MISSING;
        this.speed = IDecoderConstantsN.INTEGER_MISSING;
        this.distance = IDecoderConstantsN.INTEGER_MISSING;
        this.flightLevel = IDecoderConstantsN.INTEGER_MISSING;
    }

    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the sequenceID
     */
    public String getSequenceID() {
        return sequenceID;

    }

    /**
     * @return the sequenceID
     */
    public void setSequenceID(String sequenceID) {
        this.sequenceID = sequenceID;
    }

    /**
     * @return the intensity
     */
    public String getIntensity() {
        return intensity;
    }

    /**
     * @param intensity
     *            to set
     */
    public void setIntensity(String intensity) {
        this.intensity = intensity;
    }

    /**
     * @return the direction
     */
    public int getDirection() {
        return direction;
    }

    /**
     * @param direction
     *            to set
     */
    public void setDirection(int direction) {
        this.direction = direction;
    }

    /**
     * @return the speed
     */
    public int getSpeed() {
        return speed;
    }

    /**
     * @param speed
     *            to set
     */
    public void setSpeed(int speed) {
        this.speed = speed;
    }

    /**
     * @return the distance
     */
    public int getDistance() {
        return distance;
    }

    /**
     * @param distance
     *            to set
     */
    public void setDistance(int distance) {
        this.distance = distance;
    }

    /**
     * @return the set of convective sigmet location
     */
    public Set<ConvSigmetLocation> getConvSigmetLocation() {
        return convSigmetLocation;
    }

    /**
     * @param convsigmet
     *            the location to set
     */
    public void setConvSigmetLocation(Set<ConvSigmetLocation> convLocation) {
        this.convSigmetLocation = convLocation;
    }

    /**
     * @param add
     *            conv Sigmet location to set
     */
    public void addConvSigmetLocation(ConvSigmetLocation plocation) {
        convSigmetLocation.add(plocation);
    }

    /**
     * @return the parentID
     */
    // public String getParentID() {
    public ConvSigmetRecord getParentID() {

        return parentID;
    }

    /**
     * @param parentID
     *            the parentID to set
     */
    public void setParentID(ConvSigmetRecord parentID) {
        this.parentID = parentID;
        if (this.getConvSigmetLocation() != null
                && this.getConvSigmetLocation().size() > 0) {
            for (Iterator<ConvSigmetLocation> iter = this
                    .getConvSigmetLocation().iterator(); iter.hasNext();) {
                ConvSigmetLocation cond = iter.next();
                cond.setParentID(this);
            }
        }
    }

    /**
     * @return the classType
     */
    public String getClassType() {
        return classType;
    }

    /**
     * @param classType
     *            to set
     */
    public void setClassType(String classType) {
        this.classType = classType;
    }

    /**
     * @return the section
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

    /**
     * @return the startTime
     */
    public Calendar getStartTime() {
        return startTime;
    }

    /**
     * @param startTime
     *            to set
     */
    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    /**
     * @return the endTime
     */
    public Calendar getEndTime() {
        return endTime;
    }

    /**
     * @param endTIme
     *            to set
     */
    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * @return the cloudTop
     */
    public String getCloudTop() {
        return cloudTop;
    }

    /**
     * @param cloudTop
     *            to set
     */
    public void setCloudTop(String cloudTop) {
        this.cloudTop = cloudTop;
    }

    /**
     * @return the flightLevel
     */
    public int getFlightLevel() {
        return flightLevel;
    }

    /**
     * @param flightLevel
     *            to set
     */
    public void setFlightLevel(int flightLevel) {
        this.flightLevel = flightLevel;
    }

    /**
     * Get the record id.
     * 
     * @return The recordId. If not set returns null.
     */
    public Integer getRecordId() {
        return recordId;
    }

    /**
     * Set the record id.
     * 
     * @param record
     */
    public void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }

}
