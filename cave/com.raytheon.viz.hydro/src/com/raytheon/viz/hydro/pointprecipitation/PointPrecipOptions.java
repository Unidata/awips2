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
package com.raytheon.viz.hydro.pointprecipitation;

import java.util.Date;

/**
 * Object to hold the point precip options.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2009  2257       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointPrecipOptions {
    private Date endDate = null;

    private boolean detailsSwitch;

    private boolean hsaSwitch;

    private boolean locSwitch;

    private boolean petsSwitch;

    private boolean ppAccumSwitch;

    private int hrWindow;

    private String lid = null;

    private int sortOption;

    private int[] durations;

    private int otherDuration;
    
    private int[] hsaSet;
    
    private String[] pctsSet;
    
    private String[] pptsSet;
    
    private int numPcSelected;
    
    private int numPpSelected;
    
    private int numHsaSelected;

    /**
     * @return the endDate
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @param endDate
     *            the endDate to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * @return the detailsSwitch
     */
    public boolean isDetailsSwitch() {
        return detailsSwitch;
    }

    /**
     * @param detailsSwitch
     *            the detailsSwitch to set
     */
    public void setDetailsSwitch(boolean detailsSwitch) {
        this.detailsSwitch = detailsSwitch;
    }

    /**
     * @return the hsaSwitch
     */
    public boolean isHsaSwitch() {
        return hsaSwitch;
    }

    /**
     * @param hsaSwitch
     *            the hsaSwitch to set
     */
    public void setHsaSwitch(boolean hsaSwitch) {
        this.hsaSwitch = hsaSwitch;
    }

    /**
     * @return the locSwitch
     */
    public boolean isLocSwitch() {
        return locSwitch;
    }

    /**
     * @param locSwitch
     *            the locSwitch to set
     */
    public void setLocSwitch(boolean locSwitch) {
        this.locSwitch = locSwitch;
    }

    /**
     * @return the petsSwitch
     */
    public boolean isPetsSwitch() {
        return petsSwitch;
    }

    /**
     * @param petsSwitch
     *            the petsSwitch to set
     */
    public void setPetsSwitch(boolean petsSwitch) {
        this.petsSwitch = petsSwitch;
    }

    /**
     * @return the ppAccumSwitch
     */
    public boolean isPpAccumSwitch() {
        return ppAccumSwitch;
    }

    /**
     * @param ppAccumSwitch
     *            the ppAccumSwitch to set
     */
    public void setPpAccumSwitch(boolean ppAccumSwitch) {
        this.ppAccumSwitch = ppAccumSwitch;
    }

    /**
     * @return the hrWindow
     */
    public int getHrWindow() {
        return hrWindow;
    }

    /**
     * @param hrWindow
     *            the hrWindow to set
     */
    public void setHrWindow(int hrWindow) {
        this.hrWindow = hrWindow;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the sortOption
     */
    public int getSortOption() {
        return sortOption;
    }

    /**
     * @param sortOption
     *            the sortOption to set
     */
    public void setSortOption(int sortOption) {
        this.sortOption = sortOption;
    }

    /**
     * @return the durations
     */
    public int[] getDurations() {
        return durations;
    }

    /**
     * @param durations
     *            the durations to set
     */
    public void setDurations(int[] durations) {
        this.durations = durations;
    }

    /**
     * @return the otherDuration
     */
    public int getOtherDuration() {
        return otherDuration;
    }

    /**
     * @param otherDuration
     *            the otherDuration to set
     */
    public void setOtherDuration(int otherDuration) {
        this.otherDuration = otherDuration;
    }

    /**
     * @return the hsaSet
     */
    public int[] getHsaSet() {
        return hsaSet;
    }

    /**
     * @param hsaSet the hsaSet to set
     */
    public void setHsaSet(int[] hsaSet) {
        this.hsaSet = hsaSet;
    }

    /**
     * @return the pctsSet
     */
    public String[] getPctsSet() {
        return pctsSet;
    }

    /**
     * @param pctsSet the pctsSet to set
     */
    public void setPctsSet(String[] pctsSet) {
        this.pctsSet = pctsSet;
    }

    /**
     * @return the pptsSet
     */
    public String[] getPptsSet() {
        return pptsSet;
    }

    /**
     * @param pptsSet the pptsSet to set
     */
    public void setPptsSet(String[] pptsSet) {
        this.pptsSet = pptsSet;
    }

    /**
     * @return the numPcSelected
     */
    public int getNumPcSelected() {
        return numPcSelected;
    }

    /**
     * @param numPcSelected the numPcSelected to set
     */
    public void setNumPcSelected(int numPcSelected) {
        this.numPcSelected = numPcSelected;
    }

    /**
     * @return the numPpSelected
     */
    public int getNumPpSelected() {
        return numPpSelected;
    }

    /**
     * @param numPpSelected the numPpSelected to set
     */
    public void setNumPpSelected(int numPpSelected) {
        this.numPpSelected = numPpSelected;
    }

    /**
     * @return the numHsaSelected
     */
    public int getNumHsaSelected() {
        return numHsaSelected;
    }

    /**
     * @param numHsaSelected the numHsaSelected to set
     */
    public void setNumHsaSelected(int numHsaSelected) {
        this.numHsaSelected = numHsaSelected;
    }

}
