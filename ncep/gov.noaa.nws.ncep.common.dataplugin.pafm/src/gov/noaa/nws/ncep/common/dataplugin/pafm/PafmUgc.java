/**
 * PafmUgc
 * 
 * This java class represents the UGC for an Pafm record.
 * 
 * HISTORY
 *
 * Date     	Ticket #    Author		Description
 * ------------	---------	-----------	--------------------------
 * 08/21/09		  126		F. J. Yen	Initial creation based on AWW Decoder
 * 01/06/10		  126		F. J. Yen	Migrated and refactored from to11dr3 to to11dr11
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.dataplugin.pafm;

import java.io.Serializable;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
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
@Table(name = "pafm_ugc")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PafmUgc implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Integer recordId = null;

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
    @Column(length = 12000)
    @XmlElement
    @DynamicSerializeElement
    private String segment;

    /**
     * Pafm FIPS Table
     */
    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentId", nullable = false)
    @Index(name = "pafm_fips_parentid_idx")
    private Set<PafmFips> pafmFIPS = new HashSet<PafmFips>();

    /**
     * Pafm Parameters Table
     */
    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinColumn(name = "parentId", nullable = false)
    @Index(name = "pafm_parameters_parentid_idx")
    private Set<PafmParameters> pafmParms = new HashSet<PafmParameters>();

    /**
     * No-Arg Constructor.
     */
    public PafmUgc() {
        this.ugc = null;
        this.prodPurgeTime = null;
        this.segment = null;
        // System.out.println("IN UGC constructor.....");
        // Thread.dumpStack();
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
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the ugc
     */
    public String getUgc() {
        return ugc;
    }

    /**
     * @param ugc
     *            to set on the UGC record and children
     *            tables.~lukelin/bin/stopcamel
     */
    public void setUgc(String ugc) {
        this.ugc = ugc;
    }

    /**
     * @return the set of pafmFIPS
     */
    public Set<PafmFips> getPafmFIPS() {
        return pafmFIPS;
    }

    /**
     * @param pafmFIPS
     *            -the set of pafmFIPS to set
     */
    public void setPafmFIPS(Set<PafmFips> pafmFips) {
        this.pafmFIPS = pafmFips;
    }

    /**
     * Add PafmFips to set
     */
    public void addPafmFIPS(PafmFips pfips) {
        pafmFIPS.add(pfips);
    }

    /**
     * @return the set of Parameters
     */
    public Set<PafmParameters> getPafmParms() {
        return pafmParms;
    }

    /**
     * @param pafmParameters
     *            -the set of Parmameters to set
     */
    public void setPafmParms(Set<PafmParameters> pafmParams) {
        this.pafmParms = pafmParams;
    }

    /**
     * Add PafmParameters to set
     */
    public void addPafmParms(PafmParameters pparms) {
        pafmParms.add(pparms);
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

}
