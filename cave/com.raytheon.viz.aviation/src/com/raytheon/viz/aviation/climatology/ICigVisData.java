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
package com.raytheon.viz.aviation.climatology;

/**
 * ICigVisData Interface used by the month, hour, and wind direction" data
 * classes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03 MAR 2008  938        lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public interface ICigVisData {
    final String[] months = { "null", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    public enum Element {
        VISIBILITY, CEILING, JOINT
    }

    /**
     * Get MVFR data array.
     * 
     * @return Array of MVFR data.
     */
    public float[] getMvfrArray(Element element);

    /**
     * Get IFR data array.
     * 
     * @return Array of IFR data.
     */
    public float[] getIfrArray(Element element);

    /**
     * Get LIFR data array.
     * 
     * @return Array of LIFR data.
     */
    public float[] getLifrArray(Element element);

    /**
     * Get VLIFR data array.
     * 
     * @return Array of VLIFR data.
     */
    public float[] getVlifrArray(Element element);

    public void set(int month, int hour, int windDir, int flightCat, float vis,
            float cig, float jnt);

    public void setYears(int startYear, int endYear);

    public void setSite(String site);

    public int getStartYear();

    public int getEndYear();

    public String getSite();
}
