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
package com.raytheon.viz.radar;

import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint.RadarProductType;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPoint;

/**
 * This holds all of the related radar data (i.e. Storm ID, TVS, Meso) for a
 * single point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 2000       askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class RadarDisplayData {
    private RadarDisplayPoint displayPoint;

    private boolean displayed;

    private boolean inForeground;

    private HashMap<RadarProductType, ArrayList<SymbologyPacket>> displayPacketData;

    private HashMap<RadarProductType, ArrayList<SymbologyPoint>> displayPointData;

    public RadarDisplayData() {
        displayPacketData = new HashMap<RadarProductType, ArrayList<SymbologyPacket>>();
        displayPointData = new HashMap<RadarProductType, ArrayList<SymbologyPoint>>();
        displayed = true;
        inForeground = true;
    }

    /**
     * @return the displayPoint
     */
    public RadarDisplayPoint getDisplayPoint() {
        return displayPoint;
    }

    /**
     * @param displayPoint
     *            the displayPoint to set
     */
    public void setDisplayPoint(RadarDisplayPoint displayPoint) {
        this.displayPoint = displayPoint;
    }

    /**
     * @return the displayed
     */
    public boolean isDisplayed() {
        return displayed;
    }

    /**
     * @param displayed
     *            the displayed to set
     */
    public void setDisplayed(boolean displayed) {
        this.displayed = displayed;
    }

    /**
     * @return the inForeground
     */
    public boolean isInForeground() {
        return inForeground;
    }

    /**
     * @param inForeground
     *            the inForeground to set
     */
    public void setInForeground(boolean inForeground) {
        this.inForeground = inForeground;
    }

    /**
     * @return the displayPacketData
     */
    public HashMap<RadarProductType, ArrayList<SymbologyPacket>> getDisplayPacketData() {
        return displayPacketData;
    }

    /**
     * @param displayPacketData
     *            the displayPacketData to set
     */
    public void setDisplayPacketData(
            HashMap<RadarProductType, ArrayList<SymbologyPacket>> displayPacketData) {
        this.displayPacketData = displayPacketData;
    }

    /**
     * @return the displayPointData
     */
    public HashMap<RadarProductType, ArrayList<SymbologyPoint>> getDisplayPointData() {
        return displayPointData;
    }

    /**
     * @param displayPointData
     *            the displayPointData to set
     */
    public void setDisplayPointData(
            HashMap<RadarProductType, ArrayList<SymbologyPoint>> displayPointData) {
        this.displayPointData = displayPointData;
    }

    public <T extends SymbologyPacket> void addDisplayData(
            RadarProductType type, T currData) {
        if (displayPacketData.containsKey(type)) {
            displayPacketData.get(type).add(currData);
        } else {
            ArrayList<SymbologyPacket> arr = new ArrayList<SymbologyPacket>();
            arr.add(currData);
            displayPacketData.put(type, arr);
        }
    }

    public <T extends SymbologyPoint> void addDisplayData(
            RadarProductType type, T point) {
        if (displayPointData.containsKey(type)) {
            displayPointData.get(type).add(point);
        } else {
            ArrayList<SymbologyPoint> arr = new ArrayList<SymbologyPoint>();
            arr.add(point);
            displayPointData.put(type, arr);
        }
    }
}
