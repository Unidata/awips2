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
package com.raytheon.viz.aviation.model;

/**
 * WeatherGroup class represents the weather group within the common part of a
 * normalized TAF and a normalized METAR.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 30, 2008	937			grichard	Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class WeatherGroup {

    /** The intensity proximity notation * */
    private String intensityProximity = "";

    /** The descriptor notation * */
    private String descriptor = "";

    /** The precipitation notation * */
    private String precipitation = "";

    /** The obscuration notation * */
    private String obscuration = "";

    /** The other notation * */
    private String other = "";

    /**
     * Method that gets the intensity proximity.
     * 
     * @return the intensityProximity
     */
    public String getIntensityProximity() {
        return intensityProximity;
    }

    /**
     * Method that sets the intensity proximity.
     * 
     * @param intensityProximity
     *            the intensityProximity to set
     */
    public void setIntensityProximity(String intensityProximity) {
        this.intensityProximity = intensityProximity;
    }

    /**
     * Method that gets the descriptor.
     * 
     * @return the descriptor
     */
    public String getDescriptor() {
        return descriptor;
    }

    /**
     * Method that sets the descriptor.
     * 
     * @param descriptor
     *            the descriptor to set
     */
    public void setDescriptor(String descriptor) {
        this.descriptor = descriptor;
    }

    /**
     * Method that gets the precipitation.
     * 
     * @return the precipitation
     */
    public String getPrecipitation() {
        return precipitation;
    }

    /**
     * Method that sets the precipitation.
     * 
     * @param precipitation
     *            the precipitation to set
     */
    public void setPrecipitation(String precipitation) {
        this.precipitation = precipitation;
    }

    /**
     * Method that gets the obscuration.
     * 
     * @return the obscuration
     */
    public String getObscuration() {
        return obscuration;
    }

    /**
     * Method that sets the obscuration.
     * 
     * @param obscuration
     *            the obscuration to set
     */
    public void setObscuration(String obscuration) {
        this.obscuration = obscuration;
    }

    /**
     * Method that gets the other.
     * 
     * @return the other
     */
    public String getOther() {
        return other;
    }

    /**
     * Method that sets the other.
     * 
     * @param other
     *            the other to set
     */
    public void setOther(String other) {
        this.other = other;
    }

}
