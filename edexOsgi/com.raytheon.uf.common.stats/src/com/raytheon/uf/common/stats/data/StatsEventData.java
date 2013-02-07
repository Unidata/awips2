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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Stats Event helper object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 08, 2012    728     mpduff      Initial creation.
 * Jan 23, 2013   1523     mpduff      Fix list length.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatsEventData {
    /** Event Type */
    private String type;

    /** Event display name */
    private String displayName;

    /** List of groups */
    private List<String> groupList = new ArrayList<String>();

    /** List of Attributes */
    private List<String> attributeList = new ArrayList<String>();

    /** Map of Display Name -> Name */
    private final Map<String, String> groupMap = new HashMap<String, String>();

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * @param displayName
     *            the displayName to set
     */
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    /**
     * @return the groupList
     */
    public List<String> getGroupList() {
        return groupList;
    }

    /**
     * @param groupList
     *            the groupList to set
     */
    public void setGroupList(List<String> groupList) {
        this.groupList = groupList;
    }

    /**
     * @return the attributeList
     */
    public List<String> getAttributeList() {
        return attributeList;
    }

    /**
     * @param attributeList
     *            the attributeList to set
     */
    public void setAttributeList(List<String> attributeList) {
        this.attributeList = attributeList;
    }

    /**
     * @param group
     *            displayName of the group to add
     */
    public void addGroup(String displayName, String name) {
        this.groupList.add(displayName);
        this.groupMap.put(displayName, name);
    }

    /**
     * @param attribute
     *            the attribute to add
     */
    public void addAttribute(String attribute) {
        this.attributeList.add(attribute);
    }

    /**
     * Get the group list
     * 
     * @return
     */
    public String[] getGroups() {
        return groupList.toArray(new String[groupList.size()]);
    }

    /**
     * Get the attribute list
     * 
     * @return
     */
    public String[] getAttributes() {
        return attributeList.toArray(new String[attributeList.size()]);
    }

    /**
     * Get the group name from the display name.
     * 
     * @param displayName
     * @return
     */
    public String getGroupNameFromDisplayName(String displayName) {
        return groupMap.get(displayName);
    }
}
