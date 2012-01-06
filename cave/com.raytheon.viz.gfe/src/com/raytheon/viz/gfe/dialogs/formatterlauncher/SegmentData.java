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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.HashMap;

/**
 * This class contains segment data from the product text.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Jan 2008  1784       lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class SegmentData {
    
    /**
     * Segment map.
     */
    private HashMap<String, TextIndexPoints> segMap;

    /**
     * Head info map.
     */
    private HashMap<String, TextIndexPoints> headInfoMap;

    /**
     * Constructor.
     */
    public SegmentData() {
        segMap = new HashMap<String, TextIndexPoints>();
        headInfoMap = new HashMap<String, TextIndexPoints>();
    }

    /**
     * Add entry into the segment map.
     * @param key Map key.
     * @param tip Text index point data.
     */
    public void addToSegmentMap(String key, TextIndexPoints tip) {
        segMap.put(key, tip);
    }

    /**
     * Add entry into the head info map.
     * @param key Map key.
     * @param tip Text index point data.
     */
    public void addToHeadInfoMap(String key, TextIndexPoints tip) {
        headInfoMap.put(key, tip);
    }

    /**
     * Get a text index point data from the segment map.
     * @param key Map key.
     * @return Segment text index point data.
     */
    public TextIndexPoints getSegmentDataIndexPoints(String key) {
        return segMap.get(key);
    }

    /**
     * Get a text index point data from the head info map.
     * @param key Map key.
     * @return Segment text index point data.
     */
    public TextIndexPoints getHeadInfoDataIndexPoints(String key) {
        return headInfoMap.get(key);
    }

    /**
     * Get the segment map.
     * @return The segment map.
     */
    public HashMap<String, TextIndexPoints> getSementMap() {
        return segMap;
    }

    /**
     * Get the head info map.
     * @return The head info map.
     */
    public HashMap<String, TextIndexPoints> getHeadInfoMap() {
        return headInfoMap;
    }
}
