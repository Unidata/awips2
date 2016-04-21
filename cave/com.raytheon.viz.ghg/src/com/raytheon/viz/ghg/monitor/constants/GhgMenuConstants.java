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
package com.raytheon.viz.ghg.monitor.constants;


/**
 * Contains constant values that describe the various menu actions.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 30May2008    1157       MW Fegan    Initial Creation
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0 
 */

public class GhgMenuConstants {
    /**
     * Constructor.
     */
    public GhgMenuConstants() {
        // intentionally empty.
    }
    /**
     * Enumeration defining the segments of the map menu.
     */
    public enum MapMenuActions {
        SHOW_MAP,
        ZOOM_MAP,
        SHOW_LABELS;
    }
    /**
     * Enumeration defining the available zoom level.
     */
    public enum ZoomLevel {
        ZOOM_NO_ZOOM(1),
        ZOOM_2(2),
        ZOOM_4(4),
        ZOOM_6(6),
        ZOOM_8(8),
        ZOOM_12(12),
        ZOOM_16(16);
        private int level;
        
        private ZoomLevel(int level) {
            this.level = level;
        }
        
        public int getZoomLevel() {
            return level;
        }
    }
    /**
     * Enumeration defining the available maps.
     */
    public enum ShowMap {
        SHOW_FIPS("counties/counties.shp"),
        SHOW_PUBLIC("zones/zones.shp"),
        SHOW_FIRE("FireWx"),
        SHOW_MARINE("Marine");
        
        private String map;
        
        private ShowMap(String map) {
            this.map = map;
        }
        
        public String getMap() {
            return map;
        }
    }
}
