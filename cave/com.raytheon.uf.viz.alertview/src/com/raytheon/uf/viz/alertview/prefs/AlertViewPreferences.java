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
            new FilterMenu("All", "all"),
            new FilterMenu("Only Errors", "error"), new FilterMenu(
                    "Errors + Warnings", "warnPlus"));

    private final List<String> DEFAULT_COLUMNS = Arrays.asList(
            AlertTable.COLUMN_TIME, AlertTable.COLUMN_PRIORITY,
            AlertTable.COLUMN_MESSAGE);

    private String activeFilter = "warnPlus";

    private List<FilterMenu> filterMenu = new ArrayList<>(DEFAULT_FILTERS);

    private List<String> columns = new ArrayList<>(DEFAULT_COLUMNS);

    private int alertsToLoad = 1000;

    /* Time in ms */
    private int mergeRepeatInterval = 1000;

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

    }

}
