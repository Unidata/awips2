/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.common.dataplugin.nctaf;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a weather condition item contained in a taf message
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date        Ticket#     Engineer    Description
 *  ------------    ----------  ----------- --------------------------
 * 09/09/2011   458			sgurung	    Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	    Converted to HDF5
 * 10/26/2011               sgurung     Added isProbable to indicate if it is for TEMPO/PROB
 * 11/03/2011               sgurung     Removed isProbable (not needed). Added method to convert weather to canonical form.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcTafWeatherCondition extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private int id;

    /** The taf record this weather condition object belongs to * */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private NcTafChangeGroup parentID;

    /** The intensity proximity notation * */
    @DynamicSerializeElement
    @XmlElement
    private String intensityProximity = "";

    /** The descriptor notation * */
    @DynamicSerializeElement
    @XmlElement
    private String descriptor = "";

    /** The precipitation notation * */
    @DynamicSerializeElement
    @XmlElement
    private String precipitation = "";

    /** The obscuration notation * */
    @DynamicSerializeElement
    @XmlElement
    private String obscuration = "";

    /** The other notation * */
    @DynamicSerializeElement
    @XmlElement
    private String other = "";
   
    @Transient
    private Integer sequenceId;

    /**
     * No-Arg Constructor.
     */
    public NcTafWeatherCondition() {
        this.intensityProximity = "";
        this.descriptor = "";
        this.precipitation = "";
        this.obscuration = "";
        this.other = "";
    }

    /**
     * Constructor.
     * 
     * @param intensityProximity
     * @param descriptor
     * @param precipitation
     * @param obscuration
     * @param other
     */
    public NcTafWeatherCondition(NcTafChangeGroup parentID, String intensityProximity,
            String descriptor, String precipitation, String obscuration,
            String other, int sequence) {

        this.parentID = parentID;
        sequenceId = sequence;
        if (intensityProximity == null) {
            intensityProximity = "";
        }
        if (descriptor == null) {
            descriptor = "";
        }
        if (precipitation == null) {
            precipitation = "";
        }
        if (obscuration == null) {
            obscuration = "";
        }
        if (other == null) {
            other = "";
        }
        
        setIntensityProximity(intensityProximity);
        setDescriptor(descriptor);
        setPrecipitation(precipitation);
        setObscuration(obscuration);
        setOther(other);
    }
    
   /* Constructor.
    * 
    * @param intensityProximity
    * @param descriptor
    * @param precipitation
    * @param obscuration
    * @param other
    */
   public NcTafWeatherCondition(NcTafChangeGroup parentID, String intensityProximity,
           String descriptor, String precipitation, String obscuration,
           String other, String isProbable, int sequence) {

       this.parentID = parentID;
       sequenceId = sequence;
       if (intensityProximity == null) {
           intensityProximity = "";
       }
       if (descriptor == null) {
           descriptor = "";
       }
       if (precipitation == null) {
           precipitation = "";
       }
       if (obscuration == null) {
           obscuration = "";
       }
       if (other == null) {
           other = "";
       }
       if (isProbable == null) {
    	   isProbable = "";
       }
       setIntensityProximity(intensityProximity);
       setDescriptor(descriptor);
       setPrecipitation(precipitation);
       setObscuration(obscuration);
       setOther(other);
   }

    /**
     * @return the descriptor
     */
    public String getDescriptor() {
        return descriptor;
    }

    /**
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(String descriptor) {
        if (descriptor != null) {
            this.descriptor = descriptor;
        }
    }

    /**
     * @return the intensityProximity
     */
    public String getIntensityProximity() {
        return intensityProximity;
    }

    /**
     * @param intensityProximity
     *            the intensityProximity to set
     */
    public void setIntensityProximity(String intensityProximity) {
        if (intensityProximity != null) {
            this.intensityProximity = intensityProximity;
        }
    }

    /**
     * @return the obscuration
     */
    public String getObscuration() {
        return obscuration;
    }

    /**
     * @param obscuration
     *            the obscuration to set
     */
    public void setObscuration(String obscuration) {
        if (obscuration != null) {
            this.obscuration = obscuration;
        }
    }

    /**
     * @return the other
     */
    public String getOther() {
        return other;
    }

    /**
     * @param other
     *            the other to set
     */
    public void setOther(String other) {
        if (other != null) {
            this.other = other;
        }
    }
        
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
     * @return the precipitation
     */
    public String getPrecipitation() {
        return precipitation;
    }

    /**
     * @param precipitation
     *            the precipitation to set
     */
    public void setPrecipitation(String precipitation) {
        if (precipitation != null) {
            this.precipitation = precipitation;
        }
    }
   
    /**
     * @return the sequenceId
     */
    public Integer getSequenceId() {
        return sequenceId;
    }

    public StringBuilder toString(StringBuilder buffer) {
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        // buffer.append(parentID);
        // buffer.append(":");
        buffer.append((sequenceId != null) ? sequenceId : "--");
        buffer.append(":");
        buffer.append((intensityProximity != null) ? intensityProximity : "_");

        buffer.append((intensityProximity != null) ? intensityProximity : "_");
        buffer.append(":");
        buffer.append((descriptor != null) ? descriptor : "__");
        buffer.append(":");
        buffer.append((precipitation != null) ? precipitation : "__");
        buffer.append(":");
        buffer.append((obscuration != null) ? obscuration : "__");
        buffer.append(":");
        buffer.append((other != null) ? other : "__");
      
        return buffer;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Int:");
        sb.append(intensityProximity);
        sb.append(":Desc:");
        sb.append(descriptor);
        sb.append(":Precip:");
        sb.append(precipitation);
        sb.append(":Obscuration:");
        sb.append(obscuration);
        sb.append(":other:");
        sb.append(other);
        return sb.toString();
    }

    /**
     * @param sequenceId
     *            the sequenceId to set
     */
    public void setSequenceId(Integer sequenceId) {
        this.sequenceId = sequenceId;
    }

    @Override
    public boolean equals(Object obj) {

        if (obj instanceof NcTafWeatherCondition) {
            NcTafWeatherCondition cond = (NcTafWeatherCondition) obj;

            if (!(this.precipitation == null ? cond.getPrecipitation() == null
                    : this.precipitation.equals(cond.getPrecipitation()))) {
                return false;
            }

            if (!(this.obscuration == null ? cond.getObscuration() == null
                    : this.obscuration.equals(cond.getObscuration()))) {
                return false;
            }

            if (parentID != cond.getParentID()) {
                return false;
            }

            if (!(this.intensityProximity == null ? cond
                    .getIntensityProximity() == null : this.intensityProximity
                    .equals(cond.getIntensityProximity()))) {
                return false;
            }

            if (!(this.descriptor == null ? cond.getDescriptor() == null
                    : this.descriptor.equals(cond.getDescriptor()))) {
                return false;
            }

            if (!(this.other == null ? cond.other == null : this.other
                    .equals(cond.getOther()))) {
                return false;
            }
            
            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(precipitation).append(
                obscuration).append(parentID).append(intensityProximity)
                .append(descriptor).append(other).toHashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
    
    /**
     * 
     * @return
     */
    public String toCanonicalForm() {
        StringBuilder sb = new StringBuilder();
        if((intensityProximity != null)&&(!"".equals(intensityProximity))) {
            sb.append(intensityProximity);
        }
        if((descriptor != null)&&(!"".equals(descriptor))) {
            sb.append(descriptor);
        }
        if((precipitation != null)&&(!"".equals(precipitation))) {
            sb.append(precipitation);
        }
        if((obscuration != null)&&(!"".equals(obscuration))) {
            sb.append(obscuration);
        }
        if((other != null)&&(!"".equals(other))) {
            sb.append(other);
        }
        return sb.toString();
    }

    /**
     * 
     * @return
     */
    public static String toCanonicalForm(Set<NcTafWeatherCondition> wthrConds) {
    	
    	List<NcTafWeatherCondition> groups = new ArrayList<NcTafWeatherCondition>(wthrConds);
        
        StringBuilder sb = new StringBuilder();
        if((groups != null)&&(groups.size() > 0)) {
            sb.append(groups.get(0).toCanonicalForm());
            for(int i = 1;i < groups.size();i++) {
                sb.append(" ");
                sb.append(groups.get(i).toCanonicalForm());
            }
        }
        return sb.toString();
    }
    
    /**
     * 
     * @param args
     */
    public static final void main(String [] args) {
        
    	Set<NcTafWeatherCondition> wthrConds = new HashSet<NcTafWeatherCondition>();
    		
    	NcTafWeatherCondition cond = new NcTafWeatherCondition();
        cond.setIntensityProximity("+");
        cond.setDescriptor("SH");
        cond.setPrecipitation("RA");
        cond.setObscuration("");
        cond.setOther("");
        wthrConds.add(cond);        
        
        for(NcTafWeatherCondition w : wthrConds) {
            System.out.println(w);
            System.out.println(w.toCanonicalForm());
        }
    }
}
