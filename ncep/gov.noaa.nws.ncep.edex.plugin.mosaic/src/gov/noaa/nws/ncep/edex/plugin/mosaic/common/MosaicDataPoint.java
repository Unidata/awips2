
package gov.noaa.nws.ncep.edex.plugin.mosaic.common;

import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyPacket;
import gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3.SymbologyPoint;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * This holds all of the related mosaic data (i.e. Storm ID, TVS, Meso) for a
 * single point.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 10/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class MosaicDataPoint {
    public enum MosaicProductType {
        MESO, STI, NOT_DEFINED, TEXT, STORM_ID, TVS, SCIT, HAIL, WIND_BARB, GENERAL
    }

    private MosaicDataKey displayPoint;

    private boolean visible;

    private boolean inForeground;

    private MosaicProductType dataType;

    private HashMap<Integer, ArrayList<SymbologyPacket>> displayPacketData;

    private HashMap<Integer, ArrayList<SymbologyPoint>> displayPointData;

    public MosaicDataPoint() {
        displayPacketData = new HashMap<Integer, ArrayList<SymbologyPacket>>();
        displayPointData = new HashMap<Integer, ArrayList<SymbologyPoint>>();
        visible = true;
        inForeground = true;
    }

    /**
     * @return the displayPoint
     */
    public MosaicDataKey getDisplayPoint() {
        return displayPoint;
    }

    /**
     * @param displayPoint
     *            the displayPoint to set
     */
    public void setDisplayPoint(MosaicDataKey displayPoint) {
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
    public HashMap<Integer, ArrayList<SymbologyPacket>> getDisplayPacketData() {
        return displayPacketData;
    }

    /**
     * @param displayPacketData
     *            the displayPacketData to set
     */
    public void setDisplayPacketData(
            HashMap<Integer, ArrayList<SymbologyPacket>> displayPacketData) {
        this.displayPacketData = displayPacketData;
    }

    /**
     * @return the displayPointData
     */
    public HashMap<Integer, ArrayList<SymbologyPoint>> getDisplayPointData() {
        return displayPointData;
    }

    /**
     * @return the dataType
     */
    public MosaicProductType getDataType() {
        return dataType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(MosaicProductType dataType) {
        this.dataType = dataType;
    }

    /**
     * @param displayPointData
     *            the displayPointData to set
     */
    public void setDisplayPointData(
            HashMap<Integer, ArrayList<SymbologyPoint>> displayPointData) {
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
        if (displayPacketData.containsKey(type)) {
            displayPacketData.get(type).add(currData);
        } else {
            ArrayList<SymbologyPacket> arr = new ArrayList<SymbologyPacket>();
            arr.add(currData);
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
        if (displayPointData.containsKey(type)) {
            displayPointData.get(type).add(point);
        } else {
            ArrayList<SymbologyPoint> arr = new ArrayList<SymbologyPoint>();
            arr.add(point);
            displayPointData.put(type, arr);
        }
    }
}
