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
package com.raytheon.uf.common.dataplugin.bufrsigwx.common;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
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
 * Sep 14, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class TropopauseLayerData implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    // tropopause layer type (height, min_height, max_height)
    @XmlAttribute
    @DynamicSerializeElement
    private Integer tropType;
    
    // tropopause height in meters.
    @XmlElement
    @DynamicSerializeElement
    private List<Double> height;
        
    @XmlElement
    @DynamicSerializeElement
    private List<Double> latitude;

    @XmlElement
    @DynamicSerializeElement
    private List<Double> longitude;
    
    public TropopauseLayerData() {
    }

    public TropopauseLayerData(Integer type) {
        tropType = type;
    }

    /**
     * 
     * @param hgt
     * @param lat
     * @param lon
     */
    public void addLevel(Double hgt, Double lat, Double lon) {
        if(height == null) {
            height = new ArrayList<Double>();
        }
        height.add(hgt);
        if(latitude == null) {
            latitude = new ArrayList<Double>();
        }
        latitude.add(lat);
        if(longitude == null) {
            longitude = new ArrayList<Double>();
        }
        longitude.add(lon);
    }

    /**
     * @return the tropType
     */
    public Integer getTropType() {
        return tropType;
    }

    /**
     * @param tropType the tropType to set
     */
    public void setTropType(Integer type) {
        tropType = type;
    }

    /**
     * @return the height
     */
    public List<Double> getHeight() {
        return height;
    }

    /**
     * @param height the height to set
     */
    public void setHeight(List<Double> height) {
        this.height = height;
    }

    /**
     * @return the latitude
     */
    public List<Double> getLatitude() {
        return latitude;
    }

    /**
     * @param latitude the latitude to set
     */
    public void setLatitude(List<Double> latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public List<Double> getLongitude() {
        return longitude;
    }

    /**
     * @param longitude the longitude to set
     */
    public void setLongitude(List<Double> longitude) {
        this.longitude = longitude;
    }
        
}
