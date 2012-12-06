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
package com.raytheon.viz.radar.util;

import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2011            mnash     	Initial creation
 * Jun 04  2012	 14710	   Xiaochuan 	The rank value should take  
 * 										from STRENGTH_RANK instead  
 * 										of 2D_STRENGTH_RANK.
 * Nov 09  2012	 15586	   Xiaochuan	In dataRowFormat, set MSI maximum  
 * 										size to 5.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class GraphicDataUtil {

    private static DmdModifier modifier = null;

    private static Map<DMDAttributeIDs, Unit<?>> units;

    /**
     * @param stormID
     *            Storm ID of interest
     * @return The data for the given Storm ID
     */
    public static String getGraphicDataValue(RadarRecord record, String stormID) {
        RadarDataKey key = getStormKey(record, stormID);

        if (key != null) {
            return getGraphicDataValue(record,
                    new Coordinate(key.getLon(), key.getLat()));
        }

        return "";
    }

    public static String getGraphicDataValue(RadarRecord record,
            Coordinate latLon) {
        return getGraphicDataValue(record, latLon, true);
    }

    public static void setupModifier(DmdModifier mod) {
        if (modifier == null) {
            modifier = mod;
            units = new HashMap<DMDAttributeIDs, Unit<?>>();

            for (DmdAttribute id : modifier.getAttributes()) {
                Unit<?> firstUnit = null;
                if (id.getUnit() != null) {
                    try {
                        firstUnit = UnitFormat.getUCUMInstance().parseObject(
                                id.getUnit(), new ParsePosition(0));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    units.put(DMDAttributeIDs.getAlternateValue(id.getValue()),
                            firstUnit);
                }
            }
        }
    }

    public static String getDMDGraphicDataValue(DmdModifier mod,
            RadarRecord record, Coordinate latLon, boolean checkVis) {
        setupModifier(mod);
        return getGraphicDataValue(record, latLon, checkVis);
    }

    /**
     * @param latLon
     *            The lat/lon of the graphic data to be retrieved
     * @return The Storm ID from the packets located at the specified lat/lon
     */
    public static String getGraphicDataValue(RadarRecord record,
            Coordinate latLon, boolean checkVisibility) {
        StringBuffer rval = new StringBuffer();

        Coordinate c1 = new Coordinate(latLon.x + .025, latLon.y + .025);
        Coordinate c2 = new Coordinate(latLon.x - .025, latLon.y - .025);
        Envelope env = new Envelope(c1, c2);

        if (record.getProductCode() == 149) {
            Map<RadarDataKey, RadarDataPoint> symbologyData = record
                    .getSymbologyData();
            for (RadarDataKey key : symbologyData.keySet()) {
                Coordinate currStorm = new Coordinate(key.getLon(),
                        key.getLat());
                if (env.contains(currStorm)) {
                    // Get the data for the select feature
                    RadarDataPoint currPoint = symbologyData.get(key);

                    if (currPoint.isVisible() || !checkVisibility) {
                        String dataRowFormat = " %4.4s %7.7s@%3.3s %9.9s %7.7s   r%-2.2s   %5.5s %8.8s %8.8s %8.8s %8.8s";

                        AreaComponent currFeature;
                        HashMap<Integer, HashMap<Integer, GenericDataComponent>> currPointData = currPoint
                                .getDisplayGenericPointData();

                        for (Integer type : currPointData.keySet()) {
                            for (GenericDataComponent currComp : currPointData
                                    .get(type).values()) {
                                currFeature = (AreaComponent) currComp;

                                // Feature ID not user unit definable
                                String mid = currFeature
                                        .getValue(DMDAttributeIDs.MESO_ID
                                                .toString());

                                // Range @ Azimuth
                                String range = setupConverter(currFeature,
                                        DMDAttributeIDs.BASE_RANGE.toString(),
                                        0, true);
                                String azimuth = currFeature
                                        .getValue(DMDAttributeIDs.BASE_AZIMUTH
                                                .toString());
                                // Get rid of decimal point
                                azimuth = azimuth.contains(".") ? azimuth
                                        .split("\\.")[0] : azimuth;

                                // Base, user unit definable
                                String baseOnLowestElev = currFeature
                                        .getValue(DMDAttributeIDs.BASE_ON_LOWEST_ELEV
                                                .toString());
                                String baseHeight = "";
                                if (baseOnLowestElev.equalsIgnoreCase("Y")) {
                                    baseHeight += "<";
                                }
                                baseHeight = setupConverter(currFeature,
                                        DMDAttributeIDs.BASE_HEIGHT.toString(),
                                        1, true);
                                
                                // Depth, user unit definable
                                String depth = setupConverter(currFeature,
                                        DMDAttributeIDs.DEPTH.toString(), 1,
                                        true);

                                // Rank, not user unit definable
                                String rank = currFeature
                                        .getValue(DMDAttributeIDs.STRENGTH_RANK
                                                .toString());
            
                                // MSI, not user definable
                                String msi = currFeature
                                        .getValue(DMDAttributeIDs.MSI
                                                .toString());

                                // llrotv, user unit definable
                                String llrotv = setupConverter(currFeature,
                                        DMDAttributeIDs.BASE_ROTATIONAL_VEL
                                                .toString(), 1, true);

                                // llg2g, user unit definable
                                String llg2g = setupConverter(currFeature,
                                        DMDAttributeIDs.BASE_GTG_VEL_DIFF
                                                .toString(), 1, true);

                                // mxrotv, user unit definable
                                String mxrotv = setupConverter(currFeature,
                                        DMDAttributeIDs.MAX_ROTATIONAL_VEL
                                                .toString(), 1, true);

                                // htmxrv, user unit definable
                                String htmxrv = setupConverter(
                                        currFeature,
                                        DMDAttributeIDs.HEIGHT_MAX_ROTATIONAL_VEL
                                                .toString(), 1, true);
                                
                                // put together the final string to display in
                                // the table
                                String fnlString = String.format(dataRowFormat,
                                        mid, range, azimuth, baseHeight, depth,
                                        rank, msi, llrotv, llg2g, mxrotv,
                                        htmxrv);

                                rval.append(fnlString);
                            }
                        }
                    }
                    break;
                }
            }
        }

        return rval.toString();
    }

    private static RadarDataKey getStormKey(RadarRecord record, String storm) {
        // get the data key for the storm id
        for (Entry<RadarDataKey, RadarDataPoint> entry : record
                .getSymbologyData().entrySet()) {
            if (entry.getValue().getStormID().equals(storm)) {
                // get the data based on the key
                return entry.getKey();
            }
        }

        return null;
    }

    public static String setupConverter(AreaComponent currFeature,
            String attributeID, int decimalPlaces, boolean showUnits) {
        double val = 0;
        try {
            val = Double.parseDouble(currFeature.getValue(attributeID));
        } catch (NumberFormatException e) {
            return "";
        }
        Unit<?> unit = UnitFormat.getUCUMInstance().parseObject(
                currFeature.getUnits(attributeID), new ParsePosition(0));
        String unitString = "";
        if (units.get(DMDAttributeIDs.getAttribute(attributeID)) != null) {
            UnitConverter converter = unit.getConverterTo(units
                    .get(DMDAttributeIDs.getAttribute(attributeID)));
            val = converter.convert(val);
            unitString = units.get(DMDAttributeIDs.getAttribute(attributeID))
                    .toString();
        }
        String finalString = "";
        if (decimalPlaces != -1) {
            finalString = String.format("%." + decimalPlaces + "f", val);
        } else {
            finalString = String.valueOf(val);
        }

        if (showUnits) {
            // handle the most common unit that comes out in the unit framework
            // as a weird unit
            if (unitString.equals("m*1524/5")) {
                unitString = "kft";
            } else if (unitString.equals("kn")) {
                unitString = "kt";
            }
            return finalString + unitString;
        } else {
            return finalString;
        }
    }

    /**
     * @return the units
     */
    public static Map<DMDAttributeIDs, Unit<?>> getUnits() {
        return units;
    }

    public static Unit<?> getUnit(DMDAttributeIDs id) {
        return units.get(id.toString());
    }

}
