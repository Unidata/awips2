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
 * This class contains Ceiling & Visibility Trend graph data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15 MAY 2008  1119        lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TrendCigVisGraphData implements Cloneable {
    /**
     * Number of hours.
     */
    private int numberOfHours = 0;

    /**
     * Starting hour.
     */
    private int startingHour = 0;

    /**
     * 2 dimensional array of integers.
     */
    private float[][] dataArray;

    /**
     * Site name.
     */
    private String site = "";

    /**
     * MVFR array index.
     */
    private final int mvfrIndex = 3;

    /**
     * IFR array index.
     */
    private final int ifrIndex = 2;

    /**
     * LIFR array index.
     */
    private final int lifrIndex = 1;

    /**
     * VLIFR array index.
     */
    private final int vlifrIndex = 0;

    /**
     * Count array index.
     */
    private final int countIndex = 4;

    /**
     * Trend element enumeration.
     * 
     * @author lvenable
     */
    public enum trendElement {
        Visibility, Ceiling, Joint
    };

    /**
     * Selected day of the week.
     */
    private trendElement selectedElement;

    /**
     * Constructor.
     */
    public TrendCigVisGraphData() {
    }

    /**
     * Constructor.
     * 
     * @param numberOfHours
     *            Number of hours.
     * @param startingHour
     *            Starting hour.
     * @param dataArray
     *            Array of data to graph.
     * @param site
     *            Site name.
     * @param selectedElement
     *            Selected trend element.
     */
    public TrendCigVisGraphData(int numberOfHours, int startingHour,
            float[][] dataArray, String site, trendElement selectedElement) {
        super();
        this.numberOfHours = numberOfHours;
        this.startingHour = startingHour;
        this.dataArray = dataArray;
        this.site = site;
        this.selectedElement = selectedElement;
    }

    /**
     * Get the number of hours.
     * 
     * @return The number of hours.
     */
    public int getNumberOfHours() {
        return numberOfHours;
    }

    /**
     * Set the number of hours.
     * 
     * @param numberOfHours
     *            The number of hours.
     */
    public void setNumberOfHours(int numberOfHours) {
        this.numberOfHours = numberOfHours;
    }

    /**
     * Get the starting hour.
     * 
     * @return The starting hour.
     */
    public int getStartingHour() {
        return startingHour;
    }

    /**
     * Set the starting hour.
     * 
     * @param startingHour
     *            The starting hour.
     */
    public void setStartingHour(int startingHour) {
        this.startingHour = startingHour;
    }

    /**
     * Get the data array.
     * 
     * @return The array of graph data.
     */
    public float[][] getDataArray() {
        return dataArray;
    }

    /**
     * Set the data array.
     * 
     * @param dataArray
     *            The array of graph data.
     */
    public void setDataArray(float[][] dataArray) {
        this.dataArray = dataArray;
    }

    /**
     * Return a clone of WindRoseConfigData data.
     * 
     * @return A clone of the WindRoseConfigData data.
     */
    public TrendCigVisGraphData cloneData() {
        try {
            return (TrendCigVisGraphData) super.clone();
        } catch (CloneNotSupportedException cnse) {
            System.out.println("Clone not supported...");
            cnse.printStackTrace();
            return this;
        }
    }

    /**
     * Get the Site name.
     * 
     * @return The Site name.
     */
    public String getSite() {
        return site;
    }

    /**
     * Set the Site name.
     * 
     * @param site
     *            The Site name.
     */
    public void setSite(String site) {
        this.site = site;
    }

    /**
     * Get the selected trend element.
     * 
     * @return The selected trend element.
     */
    public trendElement getSelectedElement() {
        return selectedElement;
    }

    /**
     * Set the selected trend element.
     * 
     * @param selectedElement
     *            The selected trend element.
     */
    public void setSelectedElement(trendElement selectedElement) {
        this.selectedElement = selectedElement;
    }

    /**
     * Get the MVFR index.
     * 
     * @return The MVFR index.
     */
    public int getMvfrIndex() {
        return mvfrIndex;
    }

    /**
     * Get the IFR index.
     * 
     * @return The IFR index.
     */
    public int getIfrIndex() {
        return ifrIndex;
    }

    /**
     * Get the LIFR index.
     * 
     * @return The LIFR index.
     */
    public int getLifrIndex() {
        return lifrIndex;
    }

    /**
     * Get the VLIFR index.
     * 
     * @return The VLIFR index.
     */
    public int getVlifr() {
        return vlifrIndex;
    }

    /**
     * Get the count index.
     * 
     * @return The count index.
     */
    public int getCountIndex() {
        return countIndex;
    }
}
