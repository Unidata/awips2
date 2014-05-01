/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.common.dataplugin.nctaf;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a sky coverage item contained in a taf message
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/09/2011   458			sgurung	    Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	    Converted to HDF5
 * 10/26/2011               sgurung     Added isProbable to indicate if it is for TEMPO/PROB
 * 11/03/2011               sgurung     Removed isProbable (not needed)
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcTafSkyCover extends PersistableDataObject implements
        ISerializableObject, Comparable<NcTafSkyCover> {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private int id;

    /** The taf record this skycover object belongs to * */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private NcTafChangeGroup parentID;

    /** The type of sky coverage * */
    @DynamicSerializeElement
    @XmlElement
    private String type;

    /** The height of the cloud layer * */
    @DynamicSerializeElement
    @XmlElement
    private Integer height;

    // For convective low level cloud - CB
    @DynamicSerializeElement
    @XmlElement
    private String genus;
    
    /**
     * @return the parentID
     */
    public NcTafChangeGroup getParentID() {
        return parentID;
    }

    /**
     * @param parentID
     *            the parentID to set
     */
    public void setParentID(NcTafChangeGroup parentID) {
        this.parentID = parentID;
    }

    /**
     * No-Arg Constructor.
     */
    public NcTafSkyCover() {
        this.type = "";
        this.height = 0;
    }

    /**
     * Constructor
     * 
     * @param type
     * @param height
     */
    public NcTafSkyCover(String type, int height) {
        this.type = type;
        this.height = height;
    }    
   
    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the height
     */
    public Integer getHeight() {
        return height;
    }

    /**
     * @param height
     *            the height to set
     */
    public void setHeight(Integer height) {
        this.height = height;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the genus
     */
    public String getGenus() {
        return genus;
    }

    /**
     * @param genus
     *            the genus to set
     */
    public void setGenus(String genus) {
        this.genus = genus;
    }    
   
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof NcTafSkyCover) {
            NcTafSkyCover sky = (NcTafSkyCover) obj;

            if (parentID != sky.parentID) {
                return false;
            }

            if (!(this.height == null ? sky.getHeight() == null : this.height
                    .equals(sky.getHeight()))) {
                return false;
            }

            if (!(this.type == null ? sky.getType() == null : this.type
                    .equals(sky.getType()))) {
                return false;
            }
            
            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(parentID).append(height)
                .append(type).toHashCode();
    }

    /**
     * @return
     */
    @Override
    public String toString() {
        StringBuilder retVal = new StringBuilder("_TAF SKY COVER_");

        if (type != null) {
            retVal.append(type);
        } else {
            retVal.append("---");
        }
        if (height != null) {
            retVal.append(String.format("%3d", height));
        } else {
            retVal.append("---");
        }
        if (genus != null) {
            retVal.append(genus);
        }
        return retVal.toString();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
    
    public int compareTo(NcTafSkyCover sc) {
        int rVal = 0;

        if (this.height != null && sc.getHeight() != null) {
            rVal = height.compareTo(sc.getHeight());
        } else if (this.height != null && sc.getHeight() == null) {
            rVal = -1;
        } else if (this.height == null && sc.getHeight() != null) {
            rVal = 1;
        }

        return rVal;
    }

}
