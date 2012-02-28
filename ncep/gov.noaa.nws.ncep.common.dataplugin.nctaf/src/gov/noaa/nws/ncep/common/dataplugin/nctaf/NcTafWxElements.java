/**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

package gov.noaa.nws.ncep.common.dataplugin.nctaf;

/**
 * Intermediate data element used to parse weather data elements. It differs from
 * the TafWeatherCondition by allowing the text position of elements to be kept.
 * It also implements Comparable allowing these elements to be sorted by their
 * position.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09/2011   458			sgurung	    Initial Creation from Raytheon's taf plugin
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class NcTafWxElements implements Comparable<NcTafWxElements> {

    private String intensity;
    
    private String descriptor;
    
    private String wx;
    
    private String obscuration;
    
    private String other;

    private Integer pos;
    
    /**
     * Get the intensity value.
     * @return the intensity
     */
    public String getIntensity() {
        return intensity;
    }

    /**
     * Set the intensity value.
     * @param intensity the intensity to set
     */
    public void setIntensity(String intensity) {
        this.intensity = intensity;
    }

    /**
     * Get the descriptor value.
     * @return the descriptor
     */
    public String getDescriptor() {
        return descriptor;
    }

    /**
     * Set the descriptor value.
     * @param descriptor the descriptor to set
     */
    public void setDescriptor(String descriptor) {
        this.descriptor = descriptor;
    }

    /**
     * Get the weather value.
     * @return the wx
     */
    public String getWx() {
        return wx;
    }

    /**
     * Set the weather value.
     * @param wx the wx to set
     */
    public void setWx(String wx) {
        this.wx = wx;
    }

    /**
     * Get the obscuration value.
     * @return the obscuration
     */
    public String getObscuration() {
        return obscuration;
    }

    /**
     * Get the obscuration value.
     * @param obscuration the obscuration to set
     */
    public void setObscuration(String obscuration) {
        this.obscuration = obscuration;
    }

    /**
     * Get the value of other weather data elements
     * @return the other
     */
    public String getOther() {
        return other;
    }

    /**
     * Set the value of other weather data elements.
     * @param other the other to set
     */
    public void setOther(String other) {
        this.other = other;
    }

    /**
     * Get the position where this element was found in the data.
     * @return the pos
     */
    public Integer getPos() {
        return pos;
    }

    /**
     * Set the position where this element was found in the data.
     * @param pos the pos to set
     */
    public void setPos(Integer pos) {
        this.pos = pos;
    }

    public StringBuilder toString(StringBuilder buffer) {
        if(buffer == null) {
            buffer = new StringBuilder();
        }
        buffer.append((intensity != null) ? intensity : "_");
        buffer.append(":");
        buffer.append((descriptor != null) ? descriptor : "__");
        buffer.append(":");
        buffer.append((wx != null) ? wx : "__");
        buffer.append(":");
        buffer.append((obscuration != null) ? obscuration : "__");
        buffer.append(":");
        buffer.append((other != null) ? other : "__");
        return buffer;
    }

    public String toString() {
        return toString(null).toString();
    }

    @Override
    public int compareTo(NcTafWxElements element) {
        return getPos().compareTo(element.getPos());
    }
    
    /**
     * Factory method to create a TafWeatherCondition entry from this data.
     * @param sequence An externally supplied sequence number to use.
     * @return The created TafWeatherCondition element.
     */
    public NcTafWeatherCondition getWeather(int sequence) {
        NcTafWeatherCondition condition = new NcTafWeatherCondition();
        
        condition.setSequenceId(sequence);
        condition.setIntensityProximity(intensity);
        condition.setDescriptor(descriptor);
        condition.setPrecipitation(wx);
        condition.setObscuration(obscuration);
        condition.setOther(other);
        
        return condition;
    }
}
