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

import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * 
 * Interface for using the DataListsProdTableComp class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public interface IDataListProdTable extends IProductTable {
    /**
     * Clear all of the lists.
     */
    void clearAllLists();

    /**
     * Clear the Sources list.
     */
    void clearSourcesList(boolean updateMenu);

    /**
     * Clear the Fields list.
     */
    void clearFieldsList(boolean updateMenu);

    /**
     * Clear the planes list.
     */
    void clearPlanesList(boolean updateMenu);

    /**
     * Deselect all list items.
     */
    void deselectAllListItems();

    /**
     * Select all list items.
     */
    void selectAllListItems();

    /**
     * Update the toolbar menus.
     * 
     * @param settings
     *            The selected "setting".
     * @param spaceTime
     *            Space or Time.
     */
    void updateToolbarMenus(ViewMenu settings, SpaceTimeMenu spaceTime);
}
