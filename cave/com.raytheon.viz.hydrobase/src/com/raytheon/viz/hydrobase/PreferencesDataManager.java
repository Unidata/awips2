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
package com.raytheon.viz.hydrobase;

import java.util.ArrayList;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.hydrobase.PreferencesData.SortCriteria;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;

/**
 * Used to manage and provides an interface to the user's preferences for
 * HydroBase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2008 1636       askripsky   Initial Creation
 * Dec 3, 2008  1744       askripsky   Modified to only show lid or name if not empty
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class PreferencesDataManager extends HydroDataManager {

    private static PreferencesDataManager manager = null;

    private PreferencesData preferencesData;

    private static final String INSERT_QUERY = "INSERT into userprefs (title, statlist, sortlist, fieldlist, userid) VALUES ('%s', '%s', '%s', '%s', '%s')";

    private static final String UPDATE_QUERY = "UPDATE userprefs SET title = '%s', statlist = '%s', sortlist = '%s', fieldlist = '%s'"
            + " WHERE userid = '%s'";

    private static final String SELECT_QUERY = "SELECT title, statlist, sortlist, fieldlist "
            + "FROM userprefs WHERE userid = '%s'";

    /**
     * Private constructor that pulls in the default preferences.
     */
    private PreferencesDataManager() {
        // Query for pre-existing preferences
        // if they don't exist, load defaults
        loadPreferences();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized PreferencesDataManager getInstance() {
        if (manager == null) {
            manager = new PreferencesDataManager();
        }

        return manager;
    }

    public SortCriteria getSortCriteria() {
        return preferencesData.getSelectedSortOption();
    }

    /**
     * Get which columns to display on the main HydroBase dialog
     * 
     * @return what columns to display
     */
    public int getDisplayColumns() {
        int rval = 0;

        if (preferencesData.isDisplayStateCounty())
            rval |= PreferencesData.SHOW_STATE_COUNTY;

        if (preferencesData.isDisplayBasin())
            rval |= PreferencesData.SHOW_BASIN;

        if (preferencesData.isDisplayRiverStream())
            rval |= PreferencesData.SHOW_RIVER_STREAM;

        if (preferencesData.isDisplayLatLon())
            rval |= PreferencesData.SHOW_LAT_LON;

        return rval;
    }

    /**
     * Returns the proper title of the dialog depending on the user's
     * preferences.
     * 
     * @param dialogTitle
     *            The base title of the dialog
     * @param lid
     *            The lid that the current dialog is referring to
     * @return The title for the dialog with, depending on the user's
     *         preferences, the LID and location name.
     */
    public String getTitleString(HydroStationData data) {
        StringBuffer rval = new StringBuffer();

        if (preferencesData.isDisplayLIDTitle()
                && !data.getStation().equals("")) {
            rval.append(" - ");
            rval.append(data.getStation());
        }

        if (preferencesData.isDisplayLocationNameTitle()
                && !data.getName().equals("")) {
            rval.append(" - ");
            rval.append(data.getName());
        }

        return rval.toString();
    }

    public void setSelectedColumns(boolean state, boolean basin,
            boolean riverStream, boolean latLon) {
        preferencesData.setDisplayStateCounty(state);
        preferencesData.setDisplayBasin(basin);
        preferencesData.setDisplayRiverStream(riverStream);
        preferencesData.setDisplayLatLon(latLon);
    }

    public void setSortCriteria(SortCriteria criteria) {
        preferencesData.setSelectedSortOption(criteria);
    }

    public void setTitleString(boolean showLID, boolean showName) {
        preferencesData.setDisplayLIDTitle(showLID);
        preferencesData.setDisplayLocationNameTitle(showName);
    }

    public boolean getShowLID() {
        return preferencesData.isDisplayLIDTitle();
    }

    public boolean getShowName() {
        return preferencesData.isDisplayLocationNameTitle();
    }

    private void loadPreferences() {
        // Try and get preferences from the DB
        ArrayList<Object[]> data = getPreferences();

        if (data != null && data.size() > 0) {
            preferencesData = new PreferencesData(data.get(0));
        } else {
            preferencesData = new PreferencesData();
        }
    }

    private ArrayList<Object[]> getPreferences() {
        return runQuery(String.format(SELECT_QUERY, LocalizationManager
                .getInstance().getCurrentUser()));
    }

    public void savePreferences() throws VizException {
        // Check if preferences exist
        // if not, insert new
        String queryString;
        if (getPreferences().size() > 0) {
            queryString = UPDATE_QUERY;
        } else {
            queryString = INSERT_QUERY;
        }

        // title, statlist, sortlist, fieldlist, userid
        runStatement(String.format(queryString, preferencesData.getTitleCode(),
                preferencesData.getStatCode(), preferencesData.getSortCode(),
                preferencesData.getFieldCode(), LocalizationManager
                        .getInstance().getCurrentUser()));
    }
}
