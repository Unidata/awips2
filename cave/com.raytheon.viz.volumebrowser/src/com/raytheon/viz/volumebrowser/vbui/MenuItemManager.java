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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.raytheon.viz.volumebrowser.vbui.DataListsProdTableComp.DataSelection;
import com.raytheon.viz.volumebrowser.widget.MenuContributionItem;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;

/**
 * 
 * This is a singleton class that is used to bridge the gap between the toolbar
 * "menu" items and the Volume Browser GUI controls.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable     Initial creation
 * Jan 24, 2013 #1516      rferrel     Methods to change/get 
 *                                      Active Data Selection.
 * Dec 06, 2013 #2271      mpduff      Added the set of keys for the selected plane items
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class MenuItemManager {
    /**
     * Class instance.
     */
    private static MenuItemManager classInstance;

    /**
     * Sources menu item map.
     */
    private HashMap<String, ArrayList<MenuContributionItem>> sourcesMenuItemMap;

    /**
     * Fields menu item map.
     */
    private HashMap<String, ArrayList<MenuContributionItem>> fieldsMenuItemMap;

    /**
     * Planes menu item map.
     */
    private HashMap<String, ArrayList<MenuContributionItem>> planesMenuItemMap;

    /**
     * Callback when a menu action takes place.
     */
    private IDataMenuAction menuActionCB;

    /**
     * Set of keys for the selected plane items.
     */
    private Set<String> selectedPlaneItems = new HashSet<String>(0);

    /**
     * Constructor.
     */
    private MenuItemManager() {
        initData();
    }

    /**
     * Get an instance of this class.
     * 
     * @return An instance of this class.
     */
    public static synchronized MenuItemManager getInstance() {
        // If the RangeUtil has not been created
        // then create a new instance.
        if (classInstance == null) {
            classInstance = new MenuItemManager();
        }
        return classInstance;
    }

    /**
     * Set the menu update callback.
     * 
     * @param menuActionCB
     *            Menu action callback.
     */
    public void setMenuUpdateCallback(IDataMenuAction menuActionCB) {
        this.menuActionCB = menuActionCB;
    }

    /**
     * Initialize the data.
     */
    private void initData() {
        sourcesMenuItemMap = new HashMap<String, ArrayList<MenuContributionItem>>();
        fieldsMenuItemMap = new HashMap<String, ArrayList<MenuContributionItem>>();
        planesMenuItemMap = new HashMap<String, ArrayList<MenuContributionItem>>();
    }

    /**
     * Add a menu contribution item to a data map.
     * 
     * @param key
     *            Item key.
     * @param mci
     *            Menu contribution item.
     */
    public void addMenuContribItem(String key, MenuContributionItem mci) {
        /*
         * Determine which map to put the data.
         */
        if (menuActionCB.getActiveDataSelection() == DataSelection.SOURCES) {
            addToMap(sourcesMenuItemMap, mci);
        } else if (menuActionCB.getActiveDataSelection() == DataSelection.FIELDS) {
            addToMap(fieldsMenuItemMap, mci);
        } else if (menuActionCB.getActiveDataSelection() == DataSelection.PLANES) {
            addToMap(planesMenuItemMap, mci);
        }
    }

    /**
     * Add data to the map.
     * 
     * @param map
     *            Data map.
     * @param mci
     *            Menu contribution item.
     */
    private void addToMap(HashMap<String, ArrayList<MenuContributionItem>> map,
            MenuContributionItem mci) {
        /*
         * If the map does not contain the key then create a new map entry else
         * add the menu contribution item to the existing entry.
         */
        if (map.containsKey(mci.getMenuContribution().xml.key) == false) {
            ArrayList<MenuContributionItem> array = new ArrayList<MenuContributionItem>();
            array.add(mci);
            map.put(mci.getMenuContribution().xml.key, array);
        } else {
            boolean enabled = map.get(mci.getMenuContribution().xml.key).get(0)
                    .isEnabled();

            mci.enableMenu(enabled);

            // The key exists so just update the array with the object.
            map.get(mci.getMenuContribution().xml.key).add(mci);
        }
    }

    /**
     * Action taken when a menu item is selected.
     * 
     * @param menuText
     *            Menu text.
     * @param menuContrib
     *            Menu contribution item.
     */
    public void menuItemSelectedAction(String menuText,
            MenuContribution menuContrib) {
        /*
         * Determine which data section the menu item was selected and then
         * process the action.
         */
        if (menuActionCB.getActiveDataSelection() == DataSelection.SOURCES) {
            handleMenuItemSelected(sourcesMenuItemMap, menuText, menuContrib);
        } else if (menuActionCB.getActiveDataSelection() == DataSelection.FIELDS) {
            handleMenuItemSelected(fieldsMenuItemMap, menuText, menuContrib);
        } else if (menuActionCB.getActiveDataSelection() == DataSelection.PLANES) {
            handleMenuItemSelected(planesMenuItemMap, menuText, menuContrib);
        }
    }

    /**
     * Handle the menu selection by disabling the menu items with the same key.
     * 
     * @param map
     *            Data map.
     * @param menuText
     *            Menu text.
     * @param menuContrib
     *            Menu contribution item.
     */
    private void handleMenuItemSelected(
            HashMap<String, ArrayList<MenuContributionItem>> map,
            String menuText, MenuContribution menuContrib) {
        /*
         * Check if the map contains the key (it should).
         */
        if (map.containsKey(menuContrib.xml.key) == true) {
            /*
             * Disable the menus that have the same key.
             */
            ArrayList<MenuContributionItem> array = map
                    .get(menuContrib.xml.key);

            for (MenuContributionItem mci : array) {
                mci.enableMenu(false);
            }
            menuActionCB.addToList(menuText, menuContrib);
        }
    }

    /**
     * Clear all of the data maps.
     */
    public void clearAllMaps() {
        clearSourcesMap();
        clearFieldsMap();
        clearPlanesMap();
    }

    /**
     * Clear the Sources data maps.
     */
    public void clearSourcesMap() {
        sourcesMenuItemMap.clear();
    }

    /**
     * Clear the Fields data maps.
     */
    public void clearFieldsMap() {
        fieldsMenuItemMap.clear();
    }

    /**
     * Clear the Planes data maps.
     */
    public void clearPlanesMap() {
        planesMenuItemMap.clear();
    }

    /**
     * Enable all of the menu items for Sources, Fields, and Planes.
     */
    public void enableAllMenuItems() {
        enableSourcesMenus();
        enableFieldsMenus();
        enablePlanesMenus();
    }

    /**
     * Enable the Sources menu items.
     */
    public void enableSourcesMenus() {
        enableMenus(sourcesMenuItemMap);
    }

    /**
     * Enable the Fields menu items.
     */
    public void enableFieldsMenus() {
        enableMenus(fieldsMenuItemMap);
    }

    /**
     * Enable the Planes menu items.
     */
    public void enablePlanesMenus() {
        enableMenus(planesMenuItemMap);
    }

    /**
     * Enable the menus for the specified data map (Sources, Fields, or Planes).
     * 
     * @param map
     *            Data map.
     */
    private void enableMenus(
            HashMap<String, ArrayList<MenuContributionItem>> map) {
        Set<String> keys = map.keySet();

        String key;
        ArrayList<MenuContributionItem> array;

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            key = iterator.next();
            array = map.get(key);

            for (MenuContributionItem menuContributionItem : array) {
                menuContributionItem.enableMenu(true);
            }
        }
    }

    /**
     * Disable menu item(s) associated with the specified key. There can be
     * multiple menu items that share the same key.
     * 
     * @param key
     *            Item key.
     * @param selection
     *            Data selection type (Sources, Fields, or Planes).
     */
    public void disableMenuItem(String key, DataSelection selection) {
        /*
         * Determine the data selection type and then disable the proper menus.
         */
        if (selection == DataSelection.SOURCES) {
            findAndDisableMenuItem(key, sourcesMenuItemMap);
        } else if (selection == DataSelection.FIELDS) {
            findAndDisableMenuItem(key, fieldsMenuItemMap);
        } else if (selection == DataSelection.PLANES) {
            findAndDisableMenuItem(key, planesMenuItemMap);
        }
    }

    /**
     * Find the menus matching the key and disable them.
     * 
     * @param key
     *            Item key.
     * @param map
     *            Data map.
     */
    private void findAndDisableMenuItem(String key,
            HashMap<String, ArrayList<MenuContributionItem>> map) {
        ArrayList<MenuContributionItem> array = map.get(key);

        if (array == null) {
            return;
        }

        for (MenuContributionItem menuContributionItem : array) {
            menuContributionItem.enableMenu(false);
        }
    }

    /**
     * Get map of available keys for the specified data selection type (Sources,
     * Fields, or Planes).
     * 
     * @param selectedType
     *            The data selection type (Sources, Fields, or Planes).
     * @return Map of keys for the specified data selection.
     */
    public HashMap<String, Object> getMapOfKeys(DataSelection selectedType) {
        if (selectedType == DataSelection.SOURCES) {
            return getKeysAsMap(sourcesMenuItemMap);
        } else if (selectedType == DataSelection.FIELDS) {
            return getKeysAsMap(fieldsMenuItemMap);
        } else if (selectedType == DataSelection.PLANES) {
            return getKeysAsMap(planesMenuItemMap);
        }

        return new HashMap<String, Object>();
    }

    /**
     * Get a map of keys from the specified map.
     * 
     * @param map
     *            Data map.
     * @return Map of keys.
     */
    private HashMap<String, Object> getKeysAsMap(
            HashMap<String, ArrayList<MenuContributionItem>> map) {
        HashMap<String, Object> newKeyMap = new HashMap<String, Object>();

        Set<String> keys = map.keySet();

        for (String keyStr : keys) {
            newKeyMap.put(keyStr, null);
        }

        return newKeyMap;
    }

    /**
     * Mark the menu item as having data.
     * 
     * @param key
     *            Data key.
     */
    public Collection<MenuContributionItem> menuItemHasData(String key) {
        if (sourcesMenuItemMap.containsKey(key) == true) {
            changeImage(sourcesMenuItemMap, true, key);
            return sourcesMenuItemMap.get(key);
        } else if (fieldsMenuItemMap.containsKey(key) == true) {
            changeImage(fieldsMenuItemMap, true, key);
            return fieldsMenuItemMap.get(key);
        } else if (planesMenuItemMap.containsKey(key) == true) {
            changeImage(planesMenuItemMap, true, key);
            return planesMenuItemMap.get(key);
        }
        return null;
    }

    /**
     * Mark all of the menu item as having no data.
     */
    public void markAllMenuItemsToNoData() {
        changeImages(sourcesMenuItemMap, false);
        changeImages(fieldsMenuItemMap, false);
        changeImages(planesMenuItemMap, false);
    }

    /**
     * Mark the all menu items of the map to have data or to not have data
     * according to the flag passed in.
     * 
     * @param map
     *            Data map.
     * @param flag
     *            True to mark all menu items to show data is available, false
     *            to mark all menu items to show data is not available.
     */
    private void changeImages(
            HashMap<String, ArrayList<MenuContributionItem>> map, boolean flag) {
        Set<String> keys = map.keySet();

        String key;
        ArrayList<MenuContributionItem> array;

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            key = iterator.next();
            array = map.get(key);

            for (MenuContributionItem menuContributionItem : array) {
                menuContributionItem.markDataAvailable(flag);
            }
        }
    }

    /**
     * Change the "inventory" image of the specified menu item.
     * 
     * @param map
     *            Data map.
     * @param flag
     *            True will mark menu as having data; false will mark menu as
     *            menu having no data.
     * @param key
     *            Key to the menu item to be marked.
     */
    private void changeImage(
            HashMap<String, ArrayList<MenuContributionItem>> map, boolean flag,
            String key) {

        ArrayList<MenuContributionItem> array;

        array = map.get(key);

        for (MenuContributionItem menuContributionItem : array) {
            menuContributionItem.markDataAvailable(flag);
        }
    }

    /**
     * 
     * Convenience methods to print all of the data.
     * 
     * Can remove when no longer needed...
     */
    public void printAllMenuInformation() {
        System.out.println("******************* Sources Menu ***************");
        printMenu(sourcesMenuItemMap);

        System.out.println("******************* Fields Menu ****************");
        printMenu(fieldsMenuItemMap);

        System.out.println("******************* Planes Menu ****************");
        printMenu(planesMenuItemMap);
    }

    /**
     * Print out the entries of the desired map.
     * 
     * @param map
     */
    private void printMenu(HashMap<String, ArrayList<MenuContributionItem>> map) {
        Set<String> keys = map.keySet();

        String key;
        ArrayList<MenuContributionItem> array;

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            key = iterator.next();
            array = map.get(key);

            for (MenuContributionItem menuContributionItem : array) {
                System.out.println("key = " + key + "\t\t" + "Menu Text = "
                        + menuContributionItem.getMenuItemText());
            }
        }
    }

    /**
     * 
     * @param data
     *            the data selection for which to obtain data
     * @return an array of menu contribution items that have data available and
     *         are not currently selected
     */
    public ArrayList<MenuContributionItem> getAvailableItems(DataSelection data) {
        ArrayList<MenuContributionItem> availableItems = new ArrayList<MenuContributionItem>();

        HashMap<String, ArrayList<MenuContributionItem>> menuItemMap = null;
        switch (data) {
        case FIELDS:
            menuItemMap = fieldsMenuItemMap;
            break;
        case PLANES:
            menuItemMap = planesMenuItemMap;
            break;
        case SOURCES:
            menuItemMap = sourcesMenuItemMap;
            break;
        }

        for (ArrayList<MenuContributionItem> menuItems : menuItemMap.values()) {
            // don't worry about the multiple menus associated with a single key
            MenuContributionItem menuItem = menuItems.get(0);

            if (menuItem.isAvailable()) {
                availableItems.add(menuItem);
            }
        }

        return availableItems;
    }

    /**
     * Obtain all the keys for lat/lon entries of the Planes Menu Item map.
     * 
     * @return latLonKeys
     * 
     */
    public List<String> getLatLonKeys() {

        List<String> latLonKeys = new ArrayList<String>();

        for (ArrayList<MenuContributionItem> menuContributions : planesMenuItemMap
                .values()) {
            for (MenuContributionItem menuContribution : menuContributions) {
                MenuContribution menuItem = menuContribution
                        .getMenuContribution();
                if (menuItem.xml.key.startsWith("Lat")
                        || menuItem.xml.key.startsWith("Lon")) {
                    latLonKeys.add(menuItem.xml.key);
                }
            }
        }

        // add the keys that include all lats and all lons
        latLonKeys.add("LATS");
        latLonKeys.add("LONS");

        return latLonKeys;
    }

    /**
     * Obtain the Active Data Selection.
     * 
     * @return dataSelection
     */
    public DataSelection getActiveDataSelection() {
        return menuActionCB.getActiveDataSelection();
    }

    /**
     * Set the Active Data Selection.
     * 
     * @param dataSelection
     */
    public void setActiveDataSelection(DataSelection dataSelection) {
        menuActionCB.setActiveDataSelection(dataSelection);
    }

    /**
     * Set the set of selected keys for the plane item list
     * 
     * @param selectedPlaneItems
     *            Set of selected keys
     */
    public void setSelectedPlaneItems(Set<String> selectedPlaneItems) {
        this.selectedPlaneItems = selectedPlaneItems;
    }

    /**
     * Get the set of selected keys for the plane item list
     * 
     * @return The selected keys
     */
    public Set<String> getSelectedPlaneItems() {
        return selectedPlaneItems;
    }
}
