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
package com.raytheon.viz.volumebrowser.vbui;

import com.raytheon.viz.volumebrowser.xml.MenuContribution;

/**
 * 
 * This class contains data for the selection list control.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2009  #2161      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class SelectionListData
{
    /**
     * String displayed in the list control.
     */
    private String displayStr;
    
    /**
     * Data that is associated with the list item.  This is information about the
     * menu item selected.
     */
    private MenuContribution menuContribution;
    
    /**
     * Constructor.
     * @param displayStr The string displayed in the list control.
     * @param menuContribution Data that is associated with the list item.  This is
     *                         information about the menu item selected.
     */
    public SelectionListData(String displayStr, MenuContribution menuContribution)
    {
        this.displayStr = displayStr;
        this.menuContribution = menuContribution;
    }
    
    /**
     * Get the display string.
     * @return The display string.
     */
    public String getDisplayString()
    {
        return displayStr;
    }
    
    /**
     * Get the menu contribution data that is associated with the list item.
     * @return The menu contribution data.
     */
    public MenuContribution getMenuContribution()
    {
        return menuContribution;
    }
}
