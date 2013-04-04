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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

/**
 * Config file for DualList class. Reused from Data Delivery.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2012            mpduff      Initial creation.
 * Aug 10, 2012  1002      mpduff      Added numeric flag for sorting.
 * Jan 07, 2013  1431      mpduff      Add case sensitive and exclude flags.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DualListConfig {

    /**
     * Available list text set to a default.
     */
    private String availableListText = "Available:";

    /**
     * Selected list text set to a default.
     */
    private String selectedListText = "Selected:";

    /**
     * Width of the list controls.
     */
    private int listWidth = 100;

    /**
     * Height of the list controls.
     */
    private int listHeight = 125;

    /**
     * Flag to determine if the up/down buttons should be shown.
     */
    private boolean showUpDownBtns = false;

    /**
     * List of items that should initially appear in the selected item list.
     */
    private List<String> selectedList = new ArrayList<String>();

    /**
     * Full list of available items.
     */
    private List<String> fullList = new ArrayList<String>();

    /**
     * The list to include.
     */
    private HashSet<String> includeList = new HashSet<String>();

    /**
     * The search field.
     */
    private String searchField = null;

    /**
     * Case Sensitive search flag.
     */
    private boolean caseFlag = false;

    /**
     * Exclude search flag.
     */
    private boolean excludeFlag = false;

    private IMenuData menuData;

    /** Flag for numeric data */
    private boolean numericData = false;

    /**
     * Constructor.
     */
    public DualListConfig() {

    }

    /**
     * Get the include list.
     * 
     * @return the include list.
     */
    public HashSet<String> getIncludeList() {
        return includeList;
    }

    /**
     * Set the include list.
     * 
     * @param includeList
     *            List to always include.
     */
    public void setIncludeList(HashSet<String> includeList) {
        this.includeList = includeList;
    }

    /**
     * Get the available list header text.
     * 
     * @return Available list header text.
     */
    public String getAvailableListText() {
        return availableListText;
    }

    /**
     * Set the available list header text.
     * 
     * @param availableListLabel
     *            Available list header text.
     */
    public void setAvailableListLabel(String availableListLabel) {
        this.availableListText = availableListLabel;
    }

    /**
     * Get the selected list header text.
     * 
     * @return Selected list header text.
     */
    public String getSelectedListText() {
        return selectedListText;
    }

    /**
     * Set the selected list header text.
     * 
     * @param selectedListLabel
     *            Selected list header text.
     */
    public void setSelectedListLabel(String selectedListLabel) {
        this.selectedListText = selectedListLabel;
    }

    /**
     * Get the list control width.
     * 
     * @return The list width.
     */
    public int getListWidth() {
        return listWidth;
    }

    /**
     * Set the width of the list control.
     * 
     * @param listWidth
     *            Width of the list control.
     */
    public void setListWidth(int listWidth) {
        this.listWidth = listWidth;
    }

    /**
     * Get the height of the list control.
     * 
     * @return The height of the list control.
     */
    public int getListHeight() {
        return listHeight;
    }

    /**
     * Set the height of the list control.
     * 
     * @param listHeight
     *            The height of the list control.
     */
    public void setListHeight(int listHeight) {
        this.listHeight = listHeight;
    }

    /**
     * Check if the up/down buttons should be shown.
     * 
     * @return True if the buttons are shown, false if hidden.
     */
    public boolean isShowUpDownBtns() {
        return showUpDownBtns;
    }

    /**
     * Set the show up/down button flag.
     * 
     * @param showUpDownBtns
     *            True to show the buttons, false to not show the buttons.
     */
    public void setShowUpDownBtns(boolean showUpDownBtns) {
        this.showUpDownBtns = showUpDownBtns;
    }

    /**
     * Get an array of selected items.
     * 
     * @return An array of selected items.
     */
    public List<String> getSelectedList() {
        return selectedList;
    }

    /**
     * Set the array of selected items.
     * 
     * @param selectedList
     *            Array of selected items.
     */
    public void setSelectedList(List<String> selectedList) {
        this.selectedList = selectedList;
    }

    /**
     * Get an array of all of the available items.
     * 
     * @return The array of all available items.
     */
    public List<String> getFullList() {
        return fullList;
    }

    /**
     * Set the array of all of the available items.
     * 
     * @param fullList
     *            The array of all available items.
     */
    public void setFullList(List<String> fullList) {
        this.fullList = fullList;
    }

    /**
     * Get the search field text.
     * 
     * @return the String the search field text.
     */
    public String getSearchField() {
        return searchField;
    }

    /**
     * Set the search field text.
     * 
     * @param searchField
     *            the search field text.
     */
    public void setSearchField(String searchField) {
        this.searchField = searchField;
    }

    public IMenuData getMenuData() {
        return menuData;
    }

    public void setMenuData(IMenuData menuData) {
        this.menuData = menuData;
    }

    /**
     * @param numericData
     *            the numericData to set
     */
    public void setNumericData(boolean numericData) {
        this.numericData = numericData;
    }

    /**
     * @return the numericData
     */
    public boolean isNumericData() {
        return numericData;
    }

    /**
     * @return the caseFlag
     */
    public boolean isCaseFlag() {
        return caseFlag;
    }

    /**
     * @return the excludeFlag
     */
    public boolean isExcludeFlag() {
        return excludeFlag;
    }

    /**
     * @param caseFlag
     *            the caseFlag to set
     */
    public void setCaseFlag(boolean caseFlag) {
        this.caseFlag = caseFlag;
    }

    /**
     * @param excludeFlag
     *            the excludeFlag to set
     */
    public void setExcludeFlag(boolean excludeFlag) {
        this.excludeFlag = excludeFlag;
    }
}
