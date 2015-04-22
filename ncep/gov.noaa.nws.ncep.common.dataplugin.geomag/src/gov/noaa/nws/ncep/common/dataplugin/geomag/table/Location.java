package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Defines a location (latitude and longitude) of a station.
 * 
 *<pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 03/29/2013   975        sgurung     Initial Creation 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlRootElement(name = "location")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "")
public class Location {

    @XmlAttribute
    protected Double latitude;
    
    @XmlAttribute
    protected Double longitude;
    
    public Location() {
    	
    }

    /**
     * Gets the value of the longitude property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getLongitude() {
        return longitude;
    }

    /**
     * Sets the value of the longitude property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setLongitude(Double value) {
        this.longitude = value;
    }

    /**
     * Gets the value of the latitude property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getLatitude() {
        return latitude;
    }

    /**
     * Sets the value of the latitude property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setLatitude(Double value) {
        this.latitude = value;
    }

}
