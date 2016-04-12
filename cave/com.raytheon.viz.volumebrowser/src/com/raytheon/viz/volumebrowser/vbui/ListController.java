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
import java.util.Arrays;
import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.List;

import com.raytheon.viz.volumebrowser.vbui.DataListsProdTableComp.DataSelection;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;

/**
 * 
 * This class encompasses a List control and adds convenience methods. The list
 * control is specialized as the indexing allows for single click
 * selection/deselection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable    Initial creation
 * Jan 12, 2016 #5055      randerso    Removed width hint to improve layout when dialog resized
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ListController {
    /**
     * Parent composite.
     */
    private final Composite parentComp;

    /**
     * List control.
     */
    private List dataItemList;

    /**
     * Array of indexes to track what items are selected.
     */
    private boolean[] indexArray = new boolean[0];

    /**
     * Array of data.
     */
    private ArrayList<SelectionListData> dataArray;

    /**
     * Data selection type.
     */
    private final DataSelection dataSelType;

    /**
     * Callback when an item is selected or unselected.
     */
    private final IListAction callback;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param dataSelType
     *            Data selection type.
     * @param callback
     *            Action callback.
     */
    public ListController(Composite parentComp, DataSelection dataSelType,
            IListAction callback) {
        this.parentComp = parentComp;
        this.dataSelType = dataSelType;

        /**
         * Callback called when an item in the list control is selected or
         * unselected.
         */
        this.callback = callback;

        initialize();
    }

    /**
     * Initialize method.
     */
    private void initialize() {
        dataArray = new ArrayList<SelectionListData>();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 125;

        dataItemList = new List(parentComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        dataItemList.setLayoutData(gd);
        dataItemList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateListIndexes();
            }
        });
    }

    /**
     * Update the list indexes every time an item is selected/deselected.
     */
    private void updateListIndexes() {

        boolean itemSelected = false;
        int selectionIndex = dataItemList.getSelectionIndex();

        if (selectionIndex >= 0) {
            if (indexArray[selectionIndex] == false) {
                indexArray[selectionIndex] = true;
                itemSelected = true;
            } else {
                indexArray[selectionIndex] = false;
                itemSelected = false;
            }
        }

        dataItemList.deselectAll();
        int indexes[] = new int[dataItemList.getItemCount()];
        Arrays.fill(indexes, -99);
        int counter = 0;

        for (int i = 0; i < indexArray.length; i++) {
            if (indexArray[i] == true) {
                indexes[counter] = i;
                ++counter;
            }
        }

        if (dataSelType == DataSelection.PLANES
                && VolumeBrowserAction.getVolumeBrowserDlg()
                        .getDialogSettings().getViewSelection() == ViewMenu.CROSSSECTION) {
            Arrays.fill(indexArray, false);
            if (itemSelected) {
                dataItemList.select(selectionIndex);
                indexArray[selectionIndex] = true;
            }
            // remove old products
            callback.listSelectionChange(false, dataSelType);
        } else {
            dataItemList.select(indexes);
        }

        // Call the callback to report if an item was selected or not.
        callback.listSelectionChange(itemSelected, this.dataSelType);
    }

    /**
     * Add an item to the list.
     * 
     * @param displayStr
     *            String to be displayed.
     * @param menuContrib
     *            Menu information.
     */
    public void addItemToList(String displayStr, MenuContribution menuContrib) {
        dataArray.add(new SelectionListData(displayStr, menuContrib));

        boolean isCrossSectionPlane = dataSelType == DataSelection.PLANES
                && VolumeBrowserAction.getVolumeBrowserDlg()
                        .getDialogSettings().getViewSelection() == ViewMenu.CROSSSECTION;

        dataItemList.add(displayStr);

        boolean[] tmpIndexes = new boolean[dataItemList.getItemCount()];
        Arrays.fill(tmpIndexes, (isCrossSectionPlane) ? false : true);

        if (indexArray.length > 0 && indexArray.length <= tmpIndexes.length) {
            System.arraycopy(indexArray, 0, tmpIndexes, 0, indexArray.length);
        }

        indexArray = new boolean[tmpIndexes.length];
        System.arraycopy(tmpIndexes, 0, indexArray, 0, tmpIndexes.length);

        if (isCrossSectionPlane) {
            dataItemList.deselectAll();
            dataItemList.notifyListeners(SWT.Selection, new Event());
        }

        dataItemList.select(dataItemList.getItemCount() - 1);

    }

    /**
     * Update the indexes after an item has been deleted.
     */
    private void updateDeletedItemsIndex() {
        indexArray = new boolean[dataItemList.getItemCount()];
        Arrays.fill(indexArray, true);

        for (int i = 0; i < indexArray.length; i++) {
            indexArray[i] = dataItemList.isSelected(i);
        }
    }

    /**
     * Clear the list.
     */
    public void clearList() {
        dataArray.clear();
        dataItemList.removeAll();
        indexArray = new boolean[0];
    }

    /**
     * Deselect all of the items in the list.
     */
    public void deselectAll() {
        Arrays.fill(indexArray, false);
        dataItemList.deselectAll();
    }

    /**
     * Select all of the items in the list.
     */
    public void selectAll() {
        if (!(dataSelType == DataSelection.PLANES && VolumeBrowserAction
                .getVolumeBrowserDlg().getDialogSettings().getViewSelection() == ViewMenu.CROSSSECTION)) {
            Arrays.fill(indexArray, true);
            dataItemList.selectAll();
        }
    }

    /**
     * Retain the items matching the map of keys passed in. Those items that are
     * not in the map are deleted.
     * 
     * @param keys
     *            Map of keys used to retain items.
     */
    public void retainMatchingItems(HashMap<String, Object> keys) {
        SelectionListData selListData;

        for (int i = dataArray.size() - 1; i >= 0; i--) {
            selListData = dataArray.get(i);

            if (keys.containsKey(selListData.getMenuContribution().xml.key) == false) {
                dataArray.remove(i);
                dataItemList.remove(i);
            }
        }

        updateDeletedItemsIndex();
    }

    /**
     * Get the available keys of the items in the list control.
     * 
     * @return Map of item keys in the list control.
     */
    public HashMap<String, Object> getAvailableKeys() {
        HashMap<String, Object> keyMap = new HashMap<String, Object>();

        for (SelectionListData selListData : dataArray) {
            keyMap.put(selListData.getMenuContribution().xml.key, null);
        }

        return keyMap;
    }

    /**
     * Get the data selection type of this list control.
     * 
     * @return The data selection type.
     */
    public DataSelection getDataSelectionType() {
        return dataSelType;
    }

    /**
     * Get a string array of the keys of the selected items in the list control.
     * 
     * @return
     */
    public String[] getSelectedKeys() {
        int[] selIndexes = dataItemList.getSelectionIndices();

        String[] keys = new String[selIndexes.length];

        for (int i = 0; i < selIndexes.length; i++) {
            keys[i] = dataArray.get(selIndexes[i]).getMenuContribution().xml.key;
        }

        return keys;
    }

    /**
     * Get a map containing the display string (key) and data key (object).
     * 
     * @return Map of display string (key) and data key (object).
     */
    public HashMap<String, String> getTextKeyMap() {
        HashMap<String, String> map = new HashMap<String, String>();

        for (SelectionListData data : dataArray) {
            map.put(data.getMenuContribution().xml.menuText,
                    data.getMenuContribution().xml.key);
        }

        return map;
    }

    /**
     * Get the selected indexes.
     * 
     * @return Int array of selected indexes.
     */
    public int[] getSelectedIndexes() {
        return dataItemList.getSelectionIndices();
    }

    /**
     * Get an array of unselected indexes.
     * 
     * @return Int array of unselected indexes.
     */
    public int[] getUnselectedIndexes() {
        int numIdx = dataItemList.getItemCount()
                - dataItemList.getSelectionCount();

        int[] unSelIdx = new int[numIdx];
        int counter = 0;

        for (int i = 0; i < dataItemList.getItemCount(); i++) {
            if (dataItemList.isSelected(i) == false) {
                unSelIdx[counter] = i;
                ++counter;
            }
        }

        return unSelIdx;
    }

    /**
     * Create a unique key.
     * 
     * @param index
     *            List index.
     * @return A unique key.
     */
    public String createUniqueKey(int index) {
        SelectionListData data = dataArray.get(index);

        return data.getMenuContribution().xml.key;

    }

    /**
     * Get the key given the list index.
     * 
     * @param index
     *            List index.
     * @return The key.
     */
    public String getKey(int index) {
        return dataArray.get(index).getMenuContribution().xml.key;
    }

    /**
     * Get the display text given the list index.
     * 
     * @param index
     *            The list index.
     * @return The display text.
     */
    public String getItemText(int index) {
        return dataArray.get(index).getMenuContribution().xml.menuText;
    }

}
