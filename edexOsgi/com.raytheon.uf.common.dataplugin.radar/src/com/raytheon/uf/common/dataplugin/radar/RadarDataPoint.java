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
package com.raytheon.uf.common.dataplugin.radar;

import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.radar.level3.CellTrendDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket.GFMAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket.StormIDPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

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
 * 03/04/2013   DCS51      zwang        Handle GFM product
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */
@DynamicSerialize
public class RadarDataPoint implements ISerializableObject {
    public enum RadarProductType {
        MESO, STI, NOT_DEFINED, TEXT, STORM_ID, TVS, SCIT, HAIL, WIND_BARB, GENERAL, GENERIC
    }

    @DynamicSerializeElement
    private RadarDataKey displayPoint;

    @DynamicSerializeElement
    private boolean visible;

    @DynamicSerializeElement
    private boolean inForeground;

    @DynamicSerializeElement
    private RadarProductType dataType;

    @DynamicSerializeElement
    private String stormID = "";

    @DynamicSerializeElement
    private HashMap<Integer, HashMap<Integer, SymbologyPacket>> displayPacketData;

    @DynamicSerializeElement
    private HashMap<Integer, HashMap<Integer, SymbologyPoint>> displayPointData;

    @DynamicSerializeElement
    private HashMap<Integer, HashMap<Integer, GenericDataComponent>> displayGenericPointData;

    @DynamicSerializeElement
    private int dataIndex = 0;

    public RadarDataPoint() {
        displayPacketData = new HashMap<Integer, HashMap<Integer, SymbologyPacket>>();
        displayPointData = new HashMap<Integer, HashMap<Integer, SymbologyPoint>>();
        displayGenericPointData = new HashMap<Integer, HashMap<Integer, GenericDataComponent>>();
        visible = true;
        inForeground = true;
    }

    /**
     * @return the displayPoint
     */
    public RadarDataKey getDisplayPoint() {
        return displayPoint;
    }

    /**
     * @param displayPoint
     *            the displayPoint to set
     */
    public void setDisplayPoint(RadarDataKey displayPoint) {
        this.displayPoint = displayPoint;
    }

    /**
     * @return the visible
     */
    public boolean isVisible() {
        return visible;
    }

    /**
     * @param visible
     *            the visible to set
     */
    public void setVisible(boolean visible) {
        this.visible = visible;
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
    public HashMap<Integer, HashMap<Integer, SymbologyPacket>> getDisplayPacketData() {
        return displayPacketData;
    }

    /**
     * @param displayPacketData
     *            the displayPacketData to set
     */
    public void setDisplayPacketData(
            HashMap<Integer, HashMap<Integer, SymbologyPacket>> displayPacketData) {
        this.displayPacketData = displayPacketData;
    }

    /**
     * @return the displayPointData
     */
    public HashMap<Integer, HashMap<Integer, SymbologyPoint>> getDisplayPointData() {
        return displayPointData;
    }

    /**
     * @return the displayGenericPointData
     */
    public HashMap<Integer, HashMap<Integer, GenericDataComponent>> getDisplayGenericPointData() {
        return displayGenericPointData;
    }

    /**
     * @param displayGenericPointData
     *            the displayGenericPointData to set
     */
    public void setDisplayGenericPointData(
            HashMap<Integer, HashMap<Integer, GenericDataComponent>> displayGenericPointData) {
        this.displayGenericPointData = displayGenericPointData;
    }

    /**
     * @return the dataType
     */
    public RadarProductType getDataType() {
        return dataType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(RadarProductType dataType) {
        this.dataType = dataType;
    }

    /**
     * @return the stormID
     */
    public String getStormID() {
        return stormID;
    }

    /**
     * @param stormID
     *            the stormID to set
     */
    public void setStormID(String stormID) {
        this.stormID = stormID;
    }

    /**
     * @return the dataIndex
     */
    public int getDataIndex() {
        return dataIndex;
    }

    /**
     * @param dataIndex
     *            the dataIndex to set
     */
    public void setDataIndex(int dataIndex) {
        this.dataIndex = dataIndex;
    }

    /**
     * @param displayPointData
     *            the displayPointData to set
     */
    public void setDisplayPointData(
            HashMap<Integer, HashMap<Integer, SymbologyPoint>> displayPointData) {
        this.displayPointData = displayPointData;
    }

    /**
     * This adds packet based data to this geolocated reference object.
     * 
     * @param <T>
     * @param type
     * @param currData
     */
    public <T extends SymbologyPacket> void addDisplayData(int type, T currData) {
        // Get the Storm ID for the data at the location referred to by this
        // object
        if (currData instanceof StormIDPacket) {
            for (StormIDPoint currPt : ((StormIDPacket) currData).getPoints()) {
                this.stormID = currPt.getStormID();
            }
        } else if (currData instanceof CellTrendDataPacket) {
            this.stormID = ((CellTrendDataPacket) currData).getCellID();
        }

        if (displayPacketData.containsKey(type)) {
            displayPacketData.get(type).put(dataIndex++, currData);
        } else {
            HashMap<Integer, SymbologyPacket> arr = new HashMap<Integer, SymbologyPacket>();
            arr.put(dataIndex++, currData);
            displayPacketData.put(type, arr);
        }
    }

    /**
     * This adds point based (multiple points per packet) data to this
     * geolocated reference object.
     * 
     * @param <T>
     * @param type
     * @param point
     */
    public <T extends SymbologyPoint> void addDisplayData(int type, T point) {
        if (point instanceof StormIDPoint) {
            this.stormID = ((StormIDPoint) point).getStormID();
        }

        if (displayPointData.containsKey(type)) {
            displayPointData.get(type).put(dataIndex++, point);
        } else {
            HashMap<Integer, SymbologyPoint> arr = new HashMap<Integer, SymbologyPoint>();
            arr.put(dataIndex++, point);
            displayPointData.put(type, arr);
        }
    }

    /**
     * This adds Generic Packet Component point based (multiple points per
     * packet) data to this geolocated reference object.
     * 
     * @param <T>
     * @param type
     * @param point
     */
    public <T extends GenericDataComponent> void addDisplayData(int type,
            T point) {
    	
    	String stormID = "";
    	// DMD
    	if (type == 149) {
    		stormID = point.getValue(DMDAttributeIDs.ASSOCIATE_STORM_ID
    				         .toString());
    	}
    	// GFM
    	else if (type == 140) {
    		stormID = point.getValue(GFMAttributeIDs.DETECT_ID
                    .toString());
    		String deltaT = point.getValue(GFMAttributeIDs.FORECAST_DELTA_T
                    .toString());

    		stormID += ":";
    		stormID += deltaT;
    	}

        if (!"".equalsIgnoreCase(stormID)) {
            this.stormID = stormID;
        }

        if (displayGenericPointData.containsKey(type)) {
            displayGenericPointData.get(type).put(dataIndex++, point);
        } else {
            HashMap<Integer, GenericDataComponent> arr = new HashMap<Integer, GenericDataComponent>();
            arr.put(dataIndex++, point);
            displayGenericPointData.put(type, arr);
        }
    }
}
