package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Defines a group of a regular expression in a DataFormat (HeaderFormat/DataFormat).
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
@XmlRootElement(name = "group")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "")
public class Group {

    @XmlAttribute
    protected Integer id;
    
    @XmlAttribute
    protected String name;
    
    @XmlAttribute
    protected String format;
    
    @XmlAttribute
    protected String refersTo;
    
    public Group() {
    	
    }

    /**
     * Gets the value of the id property.
     * 
     * @return id
     */
    public Integer getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param id
     *     
     */
    public void setId(Integer value) {
        this.id = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return name
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param name
     */
    public void setName(String value) {
        this.name = value;
    }
    
    /**
     * Gets the value of the format property.
     * 
     * @return format
     *     
     */
    public String getFormat() {
        return format;
    }

    /**
     * Sets the value of the format property.
     * 
     * @param format
     */
    public void setFormat(String value) {
        this.format = value;
    }

    /**
     * Gets the value of the refersTo property.
     * 
     * @return refersTo
     *     
     */
    public String getRefersTo() {
        return refersTo;
    }

    /**
     * Sets the value of the refersTo property.
     * 
     * @param refersTo
     */
    public void setRefersTo(String value) {
        this.refersTo = refersTo;
    }
}
