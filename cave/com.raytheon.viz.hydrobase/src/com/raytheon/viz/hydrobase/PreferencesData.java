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

/**
 * this class contains the preferences data for the HydroBase Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Oct 20, 2008				askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class PreferencesData {

    public final static int SHOW_STATE_COUNTY = 1;

    public final static int SHOW_BASIN = 2;

    public final static int SHOW_RIVER_STREAM = 4;

    public final static int SHOW_LAT_LON = 8;

    public final static int SHOW_TITLE_LID = 1;

    public final static int SHOW_TITLE_NAME = 2;

    public final static int SORT_STATION = 0;

    public final static int SORT_NAME = 1;

    public final static int SORT_STATE_COUNTY = 2;

    private boolean displayStateCounty;

    private boolean displayBasin;

    private boolean displayRiverStream;

    private boolean displayLatLon;

    private boolean displayLIDTitle;

    private boolean displayLocationNameTitle;

    private SortCriteria selectedSortOption;

    public static enum SortCriteria {
        STATION, NAME, STATE_COUNTY
    }

    /**
     * Constructor that sets the display defaults
     */
    public PreferencesData() {
        this.displayStateCounty = true;
        this.displayBasin = true;
        this.displayRiverStream = true;
        this.displayLatLon = false;

        this.selectedSortOption = SortCriteria.STATION;

        this.displayLIDTitle = false;
        this.displayLocationNameTitle = false;
    }

    /**
     * Converts the raw db data
     * 
     * @param data
     */
    public PreferencesData(Object[] data) {
        // title
        displayLIDTitle = (((Integer) data[0] & SHOW_TITLE_LID) == SHOW_TITLE_LID);
        displayLocationNameTitle = (((Integer) data[0] & SHOW_TITLE_NAME) == SHOW_TITLE_NAME);

        // statlist

        // sortlist
        if (((Integer) data[2] & SORT_NAME) == SORT_NAME) {
            this.selectedSortOption = SortCriteria.NAME;
        } else if (((Integer) data[2] & SORT_STATE_COUNTY) == SORT_STATE_COUNTY) {
            this.selectedSortOption = SortCriteria.STATE_COUNTY;
        } else {
            this.selectedSortOption = SortCriteria.STATION;
        }

        // fieldlist
        displayStateCounty = (((Integer) data[3] & SHOW_STATE_COUNTY) == SHOW_STATE_COUNTY);
        displayBasin = (((Integer) data[3] & SHOW_BASIN) == SHOW_BASIN);
        displayRiverStream = (((Integer) data[3] & SHOW_RIVER_STREAM) == SHOW_RIVER_STREAM);
        displayLatLon = (((Integer) data[3] & SHOW_LAT_LON) == SHOW_LAT_LON);
    }

    /**
     * Get the DB code for the sort criteria
     * 
     * @return
     */
    public int getSortCode() {
        int rval = 0;

        if (selectedSortOption == SortCriteria.NAME) {
            rval = SORT_NAME;
        } else if (selectedSortOption == SortCriteria.STATE_COUNTY) {
            rval = SORT_STATE_COUNTY;
        } else if (selectedSortOption == SortCriteria.STATION) {
            rval = SORT_STATION;
        }

        return rval;
    }

    /**
     * Get the DB code for the fields selected
     * 
     * @return
     */
    public int getFieldCode() {
        int rval = 0;

        if (displayBasin) {
            rval |= SHOW_BASIN;
        }

        if (displayLatLon) {
            rval |= SHOW_LAT_LON;
        }

        if (displayRiverStream) {
            rval |= SHOW_RIVER_STREAM;
        }

        if (displayStateCounty) {
            rval |= SHOW_STATE_COUNTY;
        }

        return rval;
    }

    /**
     * Get the DB code for the title options
     * 
     * @return
     */
    public int getTitleCode() {
        int rval = 0;

        if (displayLIDTitle) {
            rval |= SHOW_TITLE_LID;
        }

        if (displayLocationNameTitle) {
            rval |= SHOW_TITLE_NAME;
        }

        return rval;
    }

    public int getStatCode() {
        return 0;
    }

    public boolean isDisplayStateCounty() {
        return displayStateCounty;
    }

    public void setDisplayStateCounty(boolean displayStateCounty) {
        this.displayStateCounty = displayStateCounty;
    }

    public boolean isDisplayBasin() {
        return displayBasin;
    }

    public void setDisplayBasin(boolean displayBasin) {
        this.displayBasin = displayBasin;
    }

    public boolean isDisplayRiverStream() {
        return displayRiverStream;
    }

    public void setDisplayRiverStream(boolean displayRiverStream) {
        this.displayRiverStream = displayRiverStream;
    }

    public boolean isDisplayLatLon() {
        return displayLatLon;
    }

    public void setDisplayLatLon(boolean displayLatLon) {
        this.displayLatLon = displayLatLon;
    }

    public boolean isDisplayLIDTitle() {
        return displayLIDTitle;
    }

    public void setDisplayLIDTitle(boolean displayLIDTitle) {
        this.displayLIDTitle = displayLIDTitle;
    }

    public boolean isDisplayLocationNameTitle() {
        return displayLocationNameTitle;
    }

    public void setDisplayLocationNameTitle(boolean displayLocationNameTitle) {
        this.displayLocationNameTitle = displayLocationNameTitle;
    }

    public SortCriteria getSelectedSortOption() {
        return selectedSortOption;
    }

    public void setSelectedSortOption(SortCriteria selectedSortOption) {
        this.selectedSortOption = selectedSortOption;
    }
}
