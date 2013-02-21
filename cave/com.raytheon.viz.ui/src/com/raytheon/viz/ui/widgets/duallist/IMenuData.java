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
package com.raytheon.viz.ui.widgets.duallist;


import org.eclipse.swt.widgets.Shell;

/**
 * Interface representing menu data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            mpduff     Initial creation
 * Nov 02, 2012 1302       djohnson  Add javadoc.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface IMenuData {
    /**
     * Show a popup menu on the list right click
     * 
     * @param shell The shell
     * @param menuText The menu text
     */
    public void showListMenu(Shell shell, String menuText);
    
    /**
     * Show a popup menu.
     * 
     * @return true to show menu, false to not show a menu
     */
    public boolean isShowMenu();
    
    /**
     * Get the list menu text.
     * 
     * @return the menu text
     */
    public String getMenuText();

    /**
     * Set the list selection.
     * 
     * @param string The selection
     */
    public void setListSelection(String string);
}
