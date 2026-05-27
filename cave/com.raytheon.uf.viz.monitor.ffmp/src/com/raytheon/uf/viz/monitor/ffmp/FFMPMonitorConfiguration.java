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
package com.raytheon.uf.viz.monitor.ffmp;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.TableFields;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.ffmpTable;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * FFMPMonitorConfiguration object stores configuration data specific to FFMP.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date           Ticket#  Engineer  Description
 * -------------- -------- --------- -------------------
 * 29 June, 2009  2521     dhladky   Initial creation
 * Jan 04, 2016   5115     skorolev  Corrected imports.
 * Aug 14, 2018   6670     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author dhladky
 */

public class FFMPMonitorConfiguration {

    // SCAN Stuff
    private String[] pluginName;

    private boolean[] ffmpTableAttributes;

    /**
     * Constructor
     */
    public FFMPMonitorConfiguration() {

    }

    /**
     * @return HierarchicalPreferenceStore
     */
    private HierarchicalPreferenceStore getStore() {
        return (HierarchicalPreferenceStore) Activator.getDefault()
                .getPreferenceStore();
    }

    /**
     * Retrieve one set of values for all attributes on a given table.
     *
     * @param scanTable
     *            Table to retrieve values from.
     * @param field
     *            Field name to retrieve.
     *
     * @return Map of attribute/value pairs.
     */
    public Map<String, String> getValues(ffmpTable ffmpTable,
            TableFields field) {
        HierarchicalPreferenceStore store = getStore();
        Map<String, String> map = new HashMap<>();

        for (ffmpTable table : ffmpTable.values()) {
            String value = store.getString(table.toString(field));
            map.put(table.toString(), value);
        }

        return map;
    }

    /**
     * Retrieve all fields for a single attribute from the FFMP Table.
     *
     * @param attribute
     *            from the ffmp table.
     * @return Map containing field value pairs for the given attribute.
     */
    public Map<String, String> getValues(ffmpTable attribute) {
        HierarchicalPreferenceStore store = getStore();
        Map<String, String> map = new HashMap<>();

        for (TableFields field : TableFields.values()) {
            map.put(field.toString(),
                    store.getString(attribute.toString(field)));
        }

        return map;
    }

    /**
     * Retrieve the threshold data for one of the SCAN tables. Only returns
     * those attributes that have thresholds other than the default "999" value.
     *
     * @param table
     *            FFMPTables value: either CELL, MESO, or TVS.
     * @return Map keyed on the attribute name and the three threshold values:
     *         low, mid, and upp in that order.
     */
    public Map<String, String[]> getThresholds(ffmpTable table) {
        Map<String, String[]> map = new HashMap<>();
        Map<String, String> low = getValues(table, TableFields.LOW);
        Map<String, String> mid = getValues(table, TableFields.MID);
        Map<String, String> upp = getValues(table, TableFields.UPP);
        Set<String> keys = low.keySet();
        Iterator<String> iter = keys.iterator();

        while (iter.hasNext()) {
            String key = iter.next();
            String[] values = new String[3];
            values[0] = low.get(key);
            values[1] = mid.get(key);
            values[2] = upp.get(key);
            map.put(key, values);
        }

        return map;
    }

    /**
     * Set the value of one attribute in the Cell table.
     *
     * @param attribute
     *            An attribute from the cellTable enum.
     * @param field
     *            An field from the tableFields enum.
     * @param value
     *            Value to save.
     */
    public void setValue(ffmpTable attribute, TableFields field, String value) {
        getStore().setValue(attribute.toString(field), value);
    }

    /**
     * @return String[] pluginName
     */
    public String[] getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            String[]
     */
    public void setPluginName(String[] pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * Get a boolean array representing the visibility of the attributes for a
     * given table.
     *
     * @param table
     *            FFMPTables
     *
     * @return boolean[]
     */
    public boolean[] getTableAttributes(ffmpTable table) {
        return ffmpTableAttributes.clone();
    }

    /**
     * Get the boolean array representing the visibility of the attributes for a
     * given table.
     *
     * @param table
     *            FFMPTables
     * @param tableAttributes
     *            boolean[]
     */
    public void setTableAttributes(ffmpTable table, boolean[] tableAttributes) {
        this.ffmpTableAttributes = tableAttributes.clone();
    }
}
