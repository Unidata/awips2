/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataplugin.vaa;

import java.io.Serializable;
import java.util.HashSet;
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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */
@Entity
@Table(name="vaa_subpart")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class VAASubPart implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;
    
    @Id
    @GeneratedValue
    private Integer recordId = null;

    // The  record this object belongs to 
    @ManyToOne
    @JoinColumn(name="parentId", nullable=false)
    private VAARecord parentId;
    
    @Column(length = 32)
    @XmlElement
    @DynamicSerializeElement
    private String subText; 
    
    /**
     *  Shape of ash area
     *     "LINE" : 
     *     "AREA"
     *     
     */
    @Column(length = 8)
    @XmlElement
    @DynamicSerializeElement
    private String shapeType; 
    
    /** 
     * Vertex locations
     */
    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "parentId", fetch = FetchType.EAGER)
    private Set<VAALocation> locations = new HashSet<VAALocation>();


    /**
     * @return the parentID
     */
    public VAARecord getParentId() {
        return parentId;
    }

    /**
     * @param parentID the parentID to set
     */
    public void setParentId(VAARecord parentID) {
        this.parentId = parentID;
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
      * @param record
      */
    public void setRecordId(Integer recordId) {
        this.recordId = recordId;
    }
    
    /**
     * @return the subText
     */
    public String getSubText() {
        return subText;
    }

    /**
     * @param subText the subText to set
     */
    public void setSubText(String subText) {
        this.subText = subText;
    }

    /**
     * @return the shapeType
     */
    public String getShapeType() {
        return shapeType;
    }

    /**
     * @param shapeType the shapeType to set
     */
    public void setShapeType(String shapeType) {
        this.shapeType = shapeType;
    }

    public void addVertex(double lat, double lon, int index) {
        if(locations == null) {
            locations = new HashSet<VAALocation>();
        }
        locations.add(new VAALocation(lat,lon,index,this));
    }

    /**
     * @return the locations
     */
    public Set<VAALocation> getLocations() {
        return locations;
    }

    /**
     * @param locations the locations to set
     */
    public void setLocations(Set<VAALocation> locations) {
        for(VAALocation loc : locations) {
            loc.setParentId(this);
        }
        this.locations = locations;
    }
}
