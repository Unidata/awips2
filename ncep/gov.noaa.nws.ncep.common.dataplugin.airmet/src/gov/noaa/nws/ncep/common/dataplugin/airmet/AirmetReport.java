/**
 * AirmetReport
 * 
 * This java class defines the getters and setters for the 
 *      AIRMET report table.
 * 
 * HISTORY
 *
 * Date         Author      Description
 * ------------ ----------  ----------- --------------------------
 * 05/2009      L. Lin      Initial creation    
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.airmet;

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
@Table(name = "airmet_report")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AirmetReport implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Integer recordId = null;

    // The AIRMET record this object belongs to
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private AirmetRecord parentID;

    /*
     * hazard type as: Instrument Flight Rules(IR), Mountain Obscuration(MO),
     * Turbulence(TB), Icing(IC), Sustained SFC Winds(SW), Low Level Wind
     * Shear(LLWS), or CANCEL. If a report is outlook, then an "OUTLOOK: will be
     * added.
     */
    @Column(length = 40)
    @XmlElement
    @DynamicSerializeElement
    private String hazardType;

    // The report indicator will be AIRMET or OUTLOOK.
    @Column(length = 16)
    @XmlElement
    @DynamicSerializeElement
    private String reportIndicator;

    // Sequence ID of an AIRMET report
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

    // Flight level 1
    @Column(length = 16)
    @XmlElement
    @DynamicSerializeElement
    private String flightLevel1;

    // Flight level 2
    @Column(length = 16)
    @XmlElement
    @DynamicSerializeElement
    private String flightLevel2;

    // cancelFlag is a flag indicating a cancellation (0 or 1)
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Integer cancelFlag;

    // The information of a complete report
    @Column(length = 1440)
    @XmlElement
    @DynamicSerializeElement
    private String segment;

    /**
     * Airmet location
     */
    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentID", fetch = FetchType.EAGER)
    @Index(name = "airmet_location_parentid_idex")
    private Set<AirmetLocation> airmetLocation = new HashSet<AirmetLocation>();

    /**
     * No-Arg Convstructor
     */
    public AirmetReport() {
        this.flightLevel1 = null;
        this.flightLevel2 = null;
        this.segment = null;
        this.hazardType = null;
        this.reportIndicator = null;
        this.sequenceID = null;
        this.cancelFlag = 0;
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
     * @return the set of airmet location
     */
    public Set<AirmetLocation> getAirmetLocation() {
        return airmetLocation;
    }

    /**
     * @param airmet
     *            the location to set
     */
    public void setAirmetLocation(Set<AirmetLocation> curLocation) {
        this.airmetLocation = curLocation;
    }

    /**
     * @param add
     *            airmet location to set
     */
    public void addAirmetLocation(AirmetLocation plocation) {
        airmetLocation.add(plocation);
    }

    /**
     * @return the parentID
     */
    // public String getParentID() {
    public AirmetRecord getParentID() {

        return parentID;
    }

    /**
     * @param parentID
     *            the parentID to set
     */
    public void setParentID(AirmetRecord parentID) {
        this.parentID = parentID;
        if (this.getAirmetLocation() != null
                && this.getAirmetLocation().size() > 0) {
            for (Iterator<AirmetLocation> iter = this.getAirmetLocation()
                    .iterator(); iter.hasNext();) {
                AirmetLocation cond = iter.next();
                cond.setParentID(this);
            }
        }
    }

    /**
     * @return the report
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
     * @return the flightLevel1
     */
    public String getFlightLevel1() {
        return flightLevel1;
    }

    /**
     * @param flightLevel1
     *            to set
     */
    public void setFlightLevel1(String flightLevel1) {
        this.flightLevel1 = flightLevel1;
    }

    /**
     * @return the flightLevel2
     */
    public String getFlightLevel2() {
        return flightLevel2;
    }

    /**
     * @param flightLevel2
     *            to set
     */
    public void setFlightLevel2(String flightLevel2) {
        this.flightLevel2 = flightLevel2;
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

    /**
     * @return the cancelFlag
     */
    public Integer getCancelFlag() {
        return cancelFlag;
    }

    /**
     * @param cancelFlag
     *            to set
     */
    public void setCancelFlag(Integer cancelFlag) {
        this.cancelFlag = cancelFlag;
    }

    /**
     * @return the hazardType
     */
    public String getHazardType() {
        return hazardType;
    }

    /**
     * @param hazardType
     *            to set
     */
    public void setHazardType(String hazardType) {
        this.hazardType = hazardType;
    }

    /**
     * @return the reportIndicator
     */
    public String getReportIndicator() {
        return reportIndicator;
    }

    /**
     * @param reportIndicator
     *            to set
     */
    public void setReportIndicator(String reportIndicator) {
        this.reportIndicator = reportIndicator;
    }

}
