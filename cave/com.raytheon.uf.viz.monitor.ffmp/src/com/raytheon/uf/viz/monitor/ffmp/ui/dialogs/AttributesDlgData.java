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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.HashMap;

/**
 * State of the Attributes Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AttributesDlgData {
    private HashMap<String, Boolean> columnVisibilityMap = new HashMap<String, Boolean>();

    private HashMap<String, Boolean> guidanceMap = new HashMap<String, Boolean>();
    
    private String qpfType = null;
    
    public boolean isGuidColumnIncluded(String colName) {
        if (guidanceMap.containsKey(colName)) {
            return guidanceMap.get(colName);
        } else {
            return false;
        }
    }
    
    public boolean isColumnVisible(String colName) {
        return columnVisibilityMap.get(colName);
    }
    
    public void setGuidColumnIncluded(String colName, boolean visible) {
        guidanceMap.put(colName, visible);
    }
    
    public void setColumnVisible(String colName, boolean visible) {
        columnVisibilityMap.put(colName, visible);
    }
    
    /**
     * @return the guidanceMap
     */
    public HashMap<String, Boolean> getGuidanceList() {
        return guidanceMap;
    }

    /**
     * @param guidanceMap the guidanceMap to set
     */
    public void setGuidanceMap(HashMap<String, Boolean> guidanceMap) {
        this.guidanceMap = guidanceMap;
    }

    /**
     * @return the qpfType
     */
    public String getQpfType() {
        return qpfType;
    }

    /**
     * @param qpfType the qpfType to set
     */
    public void setQpfType(String qpfType) {
        this.qpfType = qpfType;
    }
}