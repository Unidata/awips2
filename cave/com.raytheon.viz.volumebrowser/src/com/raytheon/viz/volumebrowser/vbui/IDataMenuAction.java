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

import com.raytheon.viz.volumebrowser.vbui.DataListsProdTableComp.DataSelection;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;

/**
 * 
 * Used to interface with the classes that deal with the toolbar menus.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable     Initial creation
 * Jan 24, 2013 #1516      rferrel     Added setActiveDataSelection
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public interface IDataMenuAction {

    /**
     * Add menu item to the proper list control.
     * 
     * @param displayStr
     *            The string to be displayed.
     * @param menuContribution
     *            The menu information.
     */
    void addToList(String displayStr, MenuContribution menuContribution);

    /**
     * Get the active data selection.
     * 
     * @return The active data selection.
     */
    DataSelection getActiveDataSelection();

    /**
     * Set the active data selection.
     * 
     * @param dataSelection
     */
    void setActiveDataSelection(DataSelection dataSelection);
}
