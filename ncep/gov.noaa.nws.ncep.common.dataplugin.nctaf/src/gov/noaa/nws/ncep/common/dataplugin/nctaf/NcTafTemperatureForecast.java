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
 * 
 * A temperature forecast found in a taf message.
 * 
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 *   Date           Ticket#     Engineer    Description
 *   ------------   ----------  ----------- --------------------------
 * 09/09/2011   458			sgurung	    Initial Creation from Raytheon's taf plugin
 * 09/23/2011   458			sgurung	    Converted to HDF5
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcTafTemperatureForecast extends PersistableDataObject implements
        ISerializableObject {

    @Id
    @GeneratedValue
    private int id;

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** The taf record this weather condition object belongs to * */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private NcTafChangeGroup parentID;

    /** Temperature valid time */
    @DynamicSerializeElement
    @XmlElement
    private Integer valid_time;

    /** Surface temperature */
    @DynamicSerializeElement
    @XmlElement
    private Integer sfc_temp_c;

    public NcTafTemperatureForecast() {

    }

    /**
     * Constructor for new temperature forecast
     * 
     * @param negative
     *            "M" if temperature is negative
     * @param temp
     *            The temperature
     * @param time
     *            The valid time for the temperature
     */
    public NcTafTemperatureForecast(NcTafChangeGroup parentid, String negative,
            String temp, String time) {
        this.parentID = parentid;
        sfc_temp_c = Integer.parseInt(temp);
        if (negative != null) {
            sfc_temp_c *= -1;
        }

        valid_time = Integer.parseInt(time);
    }

    /**
     * Converts Temperature Forecast object to a string
     */
    @Override
    public String toString() {
        return "\nTemperature Forecast-> Surface Temp: " + sfc_temp_c + " at "
                + valid_time + " Z";
    }

    public NcTafChangeGroup getParentID() {
        return parentID;
    }

    public void setParentID(NcTafChangeGroup parentID) {
        this.parentID = parentID;
    }

    public Integer getValid_time() {
        return valid_time;
    }

    public void setValid_time(Integer valid_time) {
        this.valid_time = valid_time;
    }

    public Integer getSfc_temp_c() {
        return sfc_temp_c;
    }

    public void setSfc_temp_c(Integer sfc_temp_c) {
        this.sfc_temp_c = sfc_temp_c;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof NcTafTemperatureForecast) {
            NcTafTemperatureForecast forecast = (NcTafTemperatureForecast) obj;

            if (this.parentID != forecast.parentID) {
                return false;
            }

            if (!(this.valid_time == null ? forecast.getValid_time() == null
                    : this.valid_time.equals(forecast.getValid_time()))) {
                return false;
            }

            if (!(this.sfc_temp_c == null ? forecast.getSfc_temp_c() == null
                    : this.sfc_temp_c.equals(forecast.getSfc_temp_c()))) {
                return false;
            }

            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(parentID).append(
                this.valid_time).append(this.sfc_temp_c).toHashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

}
