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
package com.raytheon.uf.viz.monitor.data;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Keeper of the obsData. It is stored in a keyed structure Area > Station >
 * Date > ObReport
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/07/09                  dhladky    Initial Creation.
 * Nov 11, 2012 1297        skorolev    Changed ArrayList to List
 * 
 * </pre>
 * 
 * @author dhladky
 * 
 */

public class ObsData {

    public ConcurrentHashMap<String, AreaContainer> tableData = null;

    /**
     * public construct
     */
    public ObsData() {
        tableData = new ConcurrentHashMap<String, AreaContainer>();
    }

    /**
     * Add an area
     * 
     * @param stations
     * @param areaId
     */
    public void addArea(String areaId, List<String> stations) {
        AreaContainer ac = new AreaContainer(stations, areaId);
        tableData.put(areaId, ac);
    }

    /**
     * Gets the Area Container
     * 
     * @param areaId
     * @return
     */
    public AreaContainer getArea(String areaId) {
        AreaContainer ac = null;

        if (tableData.containsKey(areaId)) {
            ac = tableData.get(areaId);
        }

        return ac;
    }

    /**
     * remove the area
     * 
     * @param areaId
     */
    public void removeArea(String areaId) {
        if (tableData.containsKey(areaId)) {
            tableData.remove(areaId);
        }
    }

    /**
     * Gets the container
     * 
     * @return
     */
    public ConcurrentHashMap<String, AreaContainer> getContainer() {
        return tableData;
    }

}
