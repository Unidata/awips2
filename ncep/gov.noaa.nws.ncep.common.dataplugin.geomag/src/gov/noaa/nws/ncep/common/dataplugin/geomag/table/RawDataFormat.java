package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Defines the raw data format for the data of a station.
 * 
 *<pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 04/02/2013   975        sgurung     Initial Creation 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlRootElement(name = "rawDataFormat")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "")
public class RawDataFormat {

    @XmlElement
    protected HeaderFormat headerFormat;
    
    @XmlElement
    protected DataFormat dataFormat;
    
    public RawDataFormat() {
    	
    }

    /**
     * Gets the value of the headerFormat property.
     * 
     * @return headerFormat
     *     
     */
    public HeaderFormat getHeaderFormat() {
        return headerFormat;
    }

    /**
     * Sets the value of the headerFormat property.
     * 
     * @param headerFormat
     *     
     */
    public void setHeaderFormat(HeaderFormat headerFormat) {
        this.headerFormat = headerFormat;
    }

    /**
     * Gets the value of the dataFormat property.
     * 
     * @return dataFormat
     *     
     */
    public DataFormat getDataFormat() {
        return dataFormat;
    }

    /**
     * Sets the value of the dataFormat property.
     * 
     * @param dataFormat
     *     
     */
    public void setDataFormat(DataFormat dataFormat) {
        this.dataFormat = dataFormat;
    }

}
