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
package com.raytheon.uf.common.stats.data;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Statistics Label Object.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2012    728     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class StatsLabelData {
    /** Group string */
    @DynamicSerializeElement
    private String group = "";

    /** List of group names */
    @DynamicSerializeElement
    private List<String> groupNames = new ArrayList<String>();

    /** Recursive reference */
    @DynamicSerializeElement
    private StatsLabelData statsLabelData = null;

    /**
     * Constructor.
     */
    public StatsLabelData() {

    }

    /**
     * Constructor.
     *
     * @param group
     * @param groupNames
     */
    public StatsLabelData(String group, List<String> groupNames) {
        this.groupNames = groupNames;
        this.group = group;
    }

    /**
     * Get the group.
     *
     * @return
     */
    public String getGroup() {
        return group;
    }

    /**
     * Set the group
     *
     * @param group
     */
    public void setGroup(String group) {
        this.group = group;
    }

    /**
     * Get the group names
     *
     * @return
     */
    public List<String> getGroupNames() {
        return groupNames;
    }

    /**
     * Set the group name list
     *
     * @param groupNames
     */
    public void setGroupNames(List<String> groupNames) {
        this.groupNames = groupNames;
    }

    /**
     * Get the StatsLabelData object.
     *
     * @return
     */
    public StatsLabelData getStatsLabelData() {
        return statsLabelData;
    }

    /**
     * Set the StatsLabelData object
     *
     * @param statsLabelData
     */
    public void setStatsLabelData(StatsLabelData statsLabelData) {
        this.statsLabelData = statsLabelData;
    }

    /**
     * Add a group name.
     *
     * @param name
     */
    public void addGroupName(String name) {
        groupNames.add(name);
    }

    /**
     * Make the data keys
     *
     * @return List of the data keys
     */
    public List<String> makeKeys() {
        ArrayList<String> keysArray = new ArrayList<String>();

        if (statsLabelData == null) {
            return groupNames;
        }

        List<String> subKeys = statsLabelData.makeKeys();

        for (String grpName : groupNames) {
            for (String subKey : subKeys) {
                keysArray.add(grpName + ":" + subKey);
            }
        }

        return keysArray;
    }

    /**
     * Get the group and group names map.
     *
     * @return
     */
    public LinkedHashMap<String, List<String>> getGroupAndNamesMap() {
        LinkedHashMap<String, List<String>> groupMap = new LinkedHashMap<String, List<String>>();

        groupMap.put(group, groupNames);

        if (statsLabelData != null) {
            LinkedHashMap<String, List<String>> tmpMap = statsLabelData
                    .getGroupAndNamesMap();
            for (String s : tmpMap.keySet()) {
                groupMap.put(s, tmpMap.get(s));
            }
        }

        return groupMap;
    }
}
