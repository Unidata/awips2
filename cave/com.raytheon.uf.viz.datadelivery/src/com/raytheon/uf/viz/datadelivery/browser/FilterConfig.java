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
package com.raytheon.uf.viz.datadelivery.browser;

import com.raytheon.viz.ui.widgets.duallist.DualListConfig;

/**
 * Filter composite configuration object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FilterConfig {
	
    /** Regular Expression search field flag. */
    private boolean regExVisible;

    /** Match control visible flag. */
    private boolean matchControlVisible;

    /** Dual List Visible flag. */
    private boolean dualListVisible;

    /** Dual List Configuration object. */
    private DualListConfig dualConfig;
    
    /** Filter Identification. */
    private String filterID;

    /**
     * Return if Regular Expression is present.
     * 
     * @return the regExVisible
     */
    public boolean isRegExVisible() {
        return regExVisible;
    }

    /**
     * Set Regular Expression flag to Visible.
     * 
     * @param regExVisible
     *            the regExVisible to set
     */
    public void setRegExVisible(boolean regExVisible) {
        this.regExVisible = regExVisible;
    }

    /**
     * Return if match control is visible.
     * 
     * @return the matchControlVisible
     */
    public boolean isMatchControlVisible() {
        return matchControlVisible;
    }

    /**
     * Set match control visible flag.
     * 
     * @param matchControlVisible
     *            the matchControlVisible to set
     */
    public void setMatchControlVisible(boolean matchControlVisible) {
        this.matchControlVisible = matchControlVisible;
    }

    /**
     * Return dual list visible flag.
     * 
     * @return the dualListVisible
     */
    public boolean isDualListVisible() {
        return dualListVisible;
    }

    /**
     * Set dualListVisible flag.
     * 
     * @param dualListVisible
     *            the dualListVisible to set
     */
    public void setDualListVisible(boolean dualListVisible) {
        this.dualListVisible = dualListVisible;
    }

    /**
     * Get dual configuration object.
     * 
     * @return the dualConfig
     */
    public DualListConfig getDualConfig() {
        return dualConfig;
    }

    /**
     * Set dual configuration object.
     * 
     * @param dualConfig
     *            the dualConfig to set
     */
    public void setDualConfig(DualListConfig dualConfig) {
        this.dualConfig = dualConfig;
    }

    /**
     * Get the filter identity.
     * 
     * @return the filterID
     */
    public String getFilterID() {
        return filterID;
    }

    /**
     * Set the filter identity.
     * 
     * @param filterID the filterID to set
     */
    public void setFilterID(String filterID) {
        this.filterID = filterID;
    }
}
