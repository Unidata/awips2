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
package com.raytheon.viz.avnconfig;

import java.util.ArrayList;

/**
 * This is the main class containing all of the default rule method data. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class DefaultRuleData {
    /**
     * Array of PageData objects.
     */
    private ArrayList<PageData> pageArray;

    /**
     * Constructor.
     */
    public DefaultRuleData() {
        init();
    }

    /**
     * Constructor.
     * @param pageArray Array of PageData objects.
     */
    public DefaultRuleData(ArrayList<PageData> pageArray) {
        this.pageArray = pageArray;
    }

    /**
     * Initialization class.
     */
    private void init() {
        // Initialize the PageData array.
        pageArray = new ArrayList<PageData>();
    }

    /**
     * Add PageData to the PageData array.
     * @param pageData Page data to add to the array.
     */
    public void addPageData(PageData pageData) {
        pageArray.add(pageData);
    }

    /**
     * Get the array of PageData.
     * @return The array of PageData.
     */
    public ArrayList<PageData> getPageArray() {
        return pageArray;
    }

    /**
     * Set the array of PageData.
     * @param pageArray Array of PageData.
     */
    public void setPageArray(ArrayList<PageData> pageArray) {
        this.pageArray = pageArray;
    }
}
