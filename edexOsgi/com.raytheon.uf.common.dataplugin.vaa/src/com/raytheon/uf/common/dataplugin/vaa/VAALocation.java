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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
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
@Table(name="vaa_location")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class VAALocation implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;
    
    @Id
    @GeneratedValue
    private Integer recordId = null;

    // 
    @ManyToOne
    @JoinColumn(name="parentId", nullable=false)
    private VAASubPart parentId;

    
    // Point latitude
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double latitude;

    // Point longitude
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double longitude;   
    
    // Position index. These points are kept in a set. Order
    // is not preserved.
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Integer index;

    /**
     * 
     */
    public VAALocation() {
        // Empty constructor for bean operations
    }
    
    /**
     * 
     * @param lat
     * @param lon
     * @param index
     */
    public VAALocation(double lat, double lon,int index, VAASubPart parent) {
        latitude = lat;
        longitude = lon;
        this.index = index;
        parentId = parent;
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
      * @return the parentID
      */
     public VAASubPart getParentId() {
         return parentId;
     }

     /**
      * @param parentID the parentID to set
      */
     public void setParentId(VAASubPart parent) {
         this.parentId = parent;
     }

    /**
     * @return the latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude the latitude to set
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * @param longitude the longitude to set
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the index
     */
    public Integer getIndex() {
        return index;
    }

    /**
     * @param index the index to set
     */
    public void setIndex(Integer index) {
        this.index = index;
    }

}
