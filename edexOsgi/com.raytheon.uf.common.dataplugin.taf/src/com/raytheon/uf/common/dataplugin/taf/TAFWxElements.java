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
package com.raytheon.uf.common.dataplugin.taf;

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
 * 20080605           1001 jkorman     Initial implementation.
 * May 15, 2014       3002 bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * </pre>
 *
 * @author jkorman
 * @version 1.0 
 */
public class TAFWxElements implements Comparable<TAFWxElements> {

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
    public int compareTo(TAFWxElements element) {
        return getPos().compareTo(element.getPos());
    }
    
    /**
     * Factory method to create a TafWeatherCondition entry from this data.
     * @param sequence An externally supplied sequence number to use.
     * @return The created TafWeatherCondition element.
     */
    public TafWeatherCondition getWeather(int sequence) {
        TafWeatherCondition condition = new TafWeatherCondition();
        
        condition.setSequenceId(sequence);
        condition.setIntensityProximity(intensity);
        condition.setDescriptor(descriptor);
        condition.setPrecipitation(wx);
        condition.setObscuration(obscuration);
        condition.setOther(other);
        
        return condition;
    }
}
