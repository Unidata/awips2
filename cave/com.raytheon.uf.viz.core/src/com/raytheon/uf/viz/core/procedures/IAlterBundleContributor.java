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
package com.raytheon.uf.viz.core.procedures;

import java.util.Map;

/**
 * Interface for methods needed for handling alter bundles.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2010            mschenke     Initial creation
 * Aug 8, 2012  875        rferrel     Add separators for menu support.
 * Oct 3, 2012  1248       rferrel     Added bundle change listeners.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IAlterBundleContributor {

    /**
     * Used to separate menu from entries. This string can not be part of any
     * name. An entry ending with this separator indicates a sub-menu. Entries
     * not ending with the separator are menu items added to the desired menu.
     * The top menu should always be created. Submenus should be create prior to
     * adding entries to them. Entries should be displayed in menus in the order
     * they are listed. Examples:
     * 
     * <pre>
     * D2D->                  -- this is a sub-menu off the top menu named: D2D
     * Original Contributions -- A menu item placed in the main menu.
     * D2D->Point A           -- A menu item placed in the sub menu  name: Point A
     * ->D2D->alt->           -- A sub-menu of D2D name: alt
     * ->D2D->alt->Alt A      -- A menu item for the above sub-menu name: Alt A
     * </pre>
     */
    public final static String MENU_SEPARATOR = "->";

    /**
     * When this is the menu item's name a menu item separator entry is
     * generated in the desired menu. Examples:
     * 
     * <pre>
     * <>            -- Place a menu item separator in the main menu.
     * D2D-><>       -- Place a menu item separator in the D2D sub-menu
     * </pre>
     */
    public final static String MI_SEPARATOR = "<>";

    /**
     * Get a mapping of key to alternate values for the key
     * 
     * @return
     */
    public Map<String, String[]> getAlterables();

    /**
     * Alter the bundle for the key with the selected value
     * 
     * @param bundleToAlter
     * @param alterKey
     * @param alterValue
     */
    public void alterBundle(Bundle bundleToAlter, String alterKey,
            String alterValue);

    /**
     * Git the keys associated with the bundle.
     * 
     * @param key
     * @return
     */
    public String[] getAlterables(String key);

    /**
     * @param listener
     */
    public void addAlterBundleChangeListener(IAlterBundleChangeListener listener);

    /**
     * @param listener
     */
    public void removeAlterBundeChangeListener(
            IAlterBundleChangeListener listener);

    /**
     * Allows setup for handling of IAlterBundleChangeListener.
     */
    public void listenerSetup();

    /**
     * Allows clean up of listener setup when bundle no longer needs to handler
     * IAlterBundleChangeListeners.
     */
    public void listenerShutdown();
}
