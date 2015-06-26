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
package com.raytheon.uf.viz.alertview.prefs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.alertview.Alert.Priority;
import com.raytheon.uf.viz.alertview.filter.FilterManager;
import com.raytheon.uf.viz.alertview.ui.view.AlertTable;
import com.raytheon.uf.viz.alertview.ui.view.AlertView;

/**
 * Contains the preferences that control how {@link AlertView} appears to the
 * user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlRootElement
public class AlertViewPreferences {

    private final List<FilterMenu> DEFAULT_FILTERS = Arrays.asList(
            new FilterMenu("All", FilterManager.ALL), new FilterMenu(
                    "Only Errors", Priority.ERROR.name().toLowerCase()),
            new FilterMenu("Errors + Warnings", FilterManager.WARN_PLUS));

    private final List<String> DEFAULT_COLUMNS = Arrays.asList(
            AlertTable.COLUMN_TIME, AlertTable.COLUMN_PRIORITY,
            AlertTable.COLUMN_MESSAGE);

    private String openFilter;

    private String activeFilter;

    private List<FilterMenu> filterMenu;

    private List<String> columns;

    private int alertsToLoad;

    /* Time in ms */
    private int mergeRepeatInterval;

    public AlertViewPreferences() {
        /* Everything needs reasonable defaults to keep PreferenceFile happy. */
        openFilter = FilterManager.NONE;
        activeFilter = FilterManager.WARN_PLUS;
        filterMenu = new ArrayList<>(DEFAULT_FILTERS);
        columns = new ArrayList<>(DEFAULT_COLUMNS);
        alertsToLoad = 1000;
        mergeRepeatInterval = 1000;
    }

    public AlertViewPreferences(AlertViewPreferences other) {
        this.openFilter = other.getOpenFilter();
        this.activeFilter = other.getActiveFilter();
        this.filterMenu = other.getFilterMenu();
        this.columns = other.getColumns();
        this.alertsToLoad = other.getAlertsToLoad();
        this.mergeRepeatInterval = other.getMergeRepeatInterval();
    }

    public String getOpenFilter() {
        return openFilter;
    }

    public void setOpenFilter(String openFilter) {
        this.openFilter = openFilter;
    }

    public String getActiveFilter() {
        return activeFilter;
    }

    public void setActiveFilter(String activeFilter) {
        this.activeFilter = activeFilter;
    }

    public List<FilterMenu> getFilterMenu() {
        return filterMenu;
    }

    public void setFilterMenu(List<FilterMenu> filterMenu) {
        this.filterMenu = filterMenu;
    }

    public int getAlertsToLoad() {
        return alertsToLoad;
    }

    public void setAlertsToLoad(int alertsToLoad) {
        this.alertsToLoad = alertsToLoad;
    }

    @XmlElement(name = "column")
    public List<String> getColumns() {
        return columns;
    }

    public void setColumns(List<String> columns) {
        this.columns = columns;
    }

    public int getMergeRepeatInterval() {
        return mergeRepeatInterval;
    }

    public void setMergeRepeatInterval(int mergeRepeatInterval) {
        this.mergeRepeatInterval = mergeRepeatInterval;
    }


    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((activeFilter == null) ? 0 : activeFilter.hashCode());
        result = prime * result + alertsToLoad;
        result = prime * result + ((columns == null) ? 0 : columns.hashCode());
        result = prime * result
                + ((filterMenu == null) ? 0 : filterMenu.hashCode());
        result = prime * result + mergeRepeatInterval;
        result = prime * result
                + ((openFilter == null) ? 0 : openFilter.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AlertViewPreferences other = (AlertViewPreferences) obj;
        if (activeFilter == null) {
            if (other.activeFilter != null)
                return false;
        } else if (!activeFilter.equals(other.activeFilter))
            return false;
        if (alertsToLoad != other.alertsToLoad)
            return false;
        if (columns == null) {
            if (other.columns != null)
                return false;
        } else if (!columns.equals(other.columns))
            return false;
        if (filterMenu == null) {
            if (other.filterMenu != null)
                return false;
        } else if (!filterMenu.equals(other.filterMenu))
            return false;
        if (mergeRepeatInterval != other.mergeRepeatInterval)
            return false;
        if (openFilter == null) {
            if (other.openFilter != null)
                return false;
        } else if (!openFilter.equals(other.openFilter))
            return false;
        return true;
    }


    public static class FilterMenu {

        private String text;

        private String filter;

        public FilterMenu() {

        }

        public FilterMenu(String text, String filter) {
            this.text = text;
            this.filter = filter;
        }

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public String getFilter() {
            return filter;
        }

        public void setFilter(String filter) {
            this.filter = filter;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((filter == null) ? 0 : filter.hashCode());
            result = prime * result + ((text == null) ? 0 : text.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            FilterMenu other = (FilterMenu) obj;
            if (filter == null) {
                if (other.filter != null)
                    return false;
            } else if (!filter.equals(other.filter))
                return false;
            if (text == null) {
                if (other.text != null)
                    return false;
            } else if (!text.equals(other.text))
                return false;
            return true;
        }

    }

    public static PreferenceFile<AlertViewPreferences> load(
            PreferenceFile.Listener<? super AlertViewPreferences> listener) {
        return new PreferenceFile<>("alert_view.xml",
                AlertViewPreferences.class, listener);
    }

}
