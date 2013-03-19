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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent.AreaPoint;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.GraphicBlockValues;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Functions on radar record for retrieval and manipulation of data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2010            mnash     Initial creation
 * Dec 28, 2011 11705	   gzhang	 Fix SCAN missing Rows error	
 * Mar 19, 2013 1804       bsteffen    Reduce useless data stored in radar hdf5
 * Mar 19, 2013 1804       bsteffen    Remove empty data structures from radar
 *                                     hdf5.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarRecordUtil {
    private static final String NO_STORMS_DETECTED = "NO STORMS DETECTED";

    public static Map<String, Map<GraphicBlockValues, String>> parseGraphicBlock(
            RadarRecord record) {

        Map<String, Map<GraphicBlockValues, String>> values = new LinkedHashMap<String, Map<GraphicBlockValues, String>>();

        if (record != null) {
            GraphicBlock gb = record.getGraphicBlock();
            if (gb != null) {
                Layer[] pages = gb.getPages();
                for (int i = 0; i < pages.length; i++) {
                    SymbologyPacket[] packets = pages[i].getPackets();
                    for (int j = 1; j < packets.length; j++) {
                        if (packets[j] instanceof TextSymbolPacket) {
                            if (!NO_STORMS_DETECTED
                                    .equals(((TextSymbolPacket) packets[j])
                                            .getTheText())) {
                                Map<GraphicBlockValues, String> map = new HashMap<GraphicBlockValues, String>();
                                Matcher m = RadarConstants.graphic_block_pattern
                                        .matcher(getNormalizedGBText(((TextSymbolPacket) packets[j])
                                                .getTheText()));
                                if (m.find()) {
                                    String storm_id = m.group(1).trim();
                                    map.put(GraphicBlockValues.AZIMUTH, m
                                            .group(2).trim());
                                    map.put(GraphicBlockValues.RANGE, m
                                            .group(3).trim());
                                    map.put(GraphicBlockValues.TVS, m.group(4)
                                            .trim());
                                    map.put(GraphicBlockValues.MDA, m.group(5)
                                            .trim());
                                    String temp = m.group(6).trim();
                                    if ("UNKNOWN".equals(temp)) {
                                        map.put(GraphicBlockValues.POSH, temp);
                                        map.put(GraphicBlockValues.POH, temp);
                                        map.put(GraphicBlockValues.MXHAILSIZE,
                                                temp);
                                    } else {
                                        map.put(GraphicBlockValues.POSH,
                                                temp.split("/")[0].trim());
                                        map.put(GraphicBlockValues.POH,
                                                temp.split("/")[1].trim());
                                        map.put(GraphicBlockValues.MXHAILSIZE,
                                                temp.split("/")[2].trim());
                                    }
                                    map.put(GraphicBlockValues.VIL, m.group(7)
                                            .trim());
                                    map.put(GraphicBlockValues.DBZM, m.group(8)
                                            .trim());
                                    map.put(GraphicBlockValues.HT, m.group(9)
                                            .trim());
                                    map.put(GraphicBlockValues.TOP, m.group(10)
                                            .trim());
                                    temp = m.group(11).trim();
                                    if ("NEW".equals(temp)) {
                                        map.put(GraphicBlockValues.FCSTDIR,
                                                temp);
                                        map.put(GraphicBlockValues.FCSTRAN,
                                                temp);
                                    } else {
                                        map.put(GraphicBlockValues.FCSTDIR,
                                                temp.split("/")[0].trim());
                                        map.put(GraphicBlockValues.FCSTRAN,
                                                temp.split("/")[1].trim());
                                    }
                                    values.put(storm_id, map);
                                }
                            }
                        }
                    }
                }
            }
        }

        return values;
    }

    public static String getFeatureValue(RadarRecord record, String featureId,
            String property) {
        String rval = "";

        for (RadarDataKey curLatLon : record.getSymbologyData().keySet()) {
            RadarDataPoint currPoint = record.getSymbologyData().get(curLatLon);

            for (Integer type : currPoint.getDisplayGenericPointData().keySet()) {
                for (GenericDataComponent currComponent : currPoint
                        .getDisplayGenericPointData().get(type).values()) {
                    if (featureId.equalsIgnoreCase(currComponent
                            .getValue(DMDAttributeIDs.MESO_ID.toString()))) {
                        rval = currComponent.getValue(property);
                        return rval;
                    }
                }
            }
        }
        return rval;
    }

    /**
     * Get the GenericDataComponent.
     * 
     * @param record
     *            The RadarRecord
     * @param featureId
     *            The featureId
     * 
     * @return The GenericDataComponent, or null if no matches
     */
    public static GenericDataComponent getFeatureValues(RadarRecord record,
            String featureId) {

        for (RadarDataKey curLatLon : record.getSymbologyData().keySet()) {
            RadarDataPoint currPoint = record.getSymbologyData().get(curLatLon);
            for (Integer type : currPoint.getDisplayGenericPointData().keySet()) {
                for (GenericDataComponent currComponent : currPoint
                        .getDisplayGenericPointData().get(type).values()) {
                    if (featureId.equalsIgnoreCase(currComponent
                            .getValue(DMDAttributeIDs.MESO_ID.toString()))) {
                        return currComponent;
                    }
                }
            }
        }
        return null;
    }

    public static List<String> getDMDFeatureIDs(RadarRecord record) {
        List<String> rval = new ArrayList<String>();
        SymbologyBlock sb = record.getSymbologyBlock();
        if (sb != null) {
            for (Layer layer : sb.getLayers()) {
                for (SymbologyPacket packet : layer.getPackets())
                    rval.addAll(((DMDPacket) packet).getFeatureIDs());
            }
        }
        return rval;
    }

    public static Coordinate getDMDLonLatFromFeatureID(RadarRecord record,
            String featureId) {
        Coordinate rval = null;
        AreaComponent currFeature = null;
        for (RadarDataKey curLatLon : record.getSymbologyData().keySet()) {
            RadarDataPoint currPoint = record.getSymbologyData().get(curLatLon);

            for (Integer type : currPoint.getDisplayGenericPointData().keySet()) {
                for (GenericDataComponent currComponent : currPoint
                        .getDisplayGenericPointData().get(type).values()) {
                    currFeature = (AreaComponent) currComponent;
                    if (featureId.equalsIgnoreCase(currFeature
                            .getValue(DMDAttributeIDs.MESO_ID.toString()))) {
                        List<AreaPoint> points = currFeature.getPoints();
                        rval = new Coordinate(points.get(0).getCoordinate2(),
                                points.get(0).getCoordinate1());
                        return rval;
                    }
                }
            }
        }
        return rval;
    }

    /**
     * Search in the map for the storm id and send back the feature if that is
     * the case
     * 
     * @param type
     * @param id
     * @return
     */
    public List<String> getFeatures(RadarRecord record,
            RadarConstants.MapValues type, String id) {
        List<String> list = new ArrayList<String>();
        for (Map.Entry<String, Map<RadarConstants.MapValues, String>> entry : record
                .getMapProductVals().get(type).entrySet()) {
            String fid = entry.getValue().get(
                    RadarConstants.MapValues.MESO_STORM_ID);
            if (fid != null) {
                fid = fid.trim();
                if (id.trim().equals(fid)) {
                    list.add(entry.getKey());
                }
            }
        }
        return list;
    }

    public static Coordinate getAzRangeLatLon(RadarRecord record, String id,
            float lat, float lng, String dir, String range) {

        double az = Double.parseDouble(dir);
        // translate degrees to +-180
        double calcDir = 0.0;

        if (az > 0) {
            if (az > 180.0) {
                // offset should be positive
                double offset = az - 180.0;
                calcDir = -180.0 + offset;
            } else {
                calcDir = az;
            }
        }
        if (az < 0) {
            if (az < -180.0) {
                // offset should be negative
                double offset = az + 180.0;
                calcDir = 180.0 + offset;
            } else {
                calcDir = az;
            }
        }

        double rng = Double.parseDouble(range);
        GeodeticCalculator gd = new GeodeticCalculator();
        gd.setStartingGeographicPoint(lng, lat);
        gd.setDirection(calcDir, rng);
        Coordinate coor = new Coordinate(gd.getDestinationGeographicPoint()
                .getX(), gd.getDestinationGeographicPoint().getY());
        return coor;
    }

    /**
     * 
     * Calculates the 8-bit SRM product from 8-bit base velocity. SRM is found
     * by subtracting the overall wind direction from the radial data. The
     * resulting data would show the velocity data as if the wind was
     * stationary.
     * 
     * @param record
     * @param direction
     * @param speed
     * @param numberOfPixels
     * @param numberOfGates
     * @return
     */
    public static void calculateSRM8(RadarRecord record) {
        if (record.getRawData() != null) {
            int deltaInt = 0;
            double delta;
            byte[] radialData = record.getRawData();

            // Need to make copy
            record.srmData = new byte[record.getRawData().length];

            int currBinPtr = 0;
            int maxBin = 0;

            // Loop through each bin, in each radial
            for (int currRadial = 0; currRadial < record.getNumRadials(); currRadial++) {
                // If it is moving, find the Integer delta value
                if (record.srmSpeed != 0) {
                    // Get the delta value for the current radial
                    delta = record.srmSpeed
                            * Math.cos((Math.PI / 180)
                                    * (record.srmDirection - record
                                            .getAngleData()[currRadial]))
                            / 1.944;

                    if (!record.isExpandedMode()) {
                        delta *= 2;
                    }

                    // Round delta to next higher whole number
                    if (delta < 0) {
                        deltaInt = (int) (delta - 0.5);
                    } else {
                        deltaInt = (int) (delta + 0.5);
                    }
                }

                maxBin += record.getNumBins();

                if (deltaInt == 0) {
                    // If there is not difference, just assign the original
                    // value
                    for (int currBin = currBinPtr; currBin < maxBin; currBin++) {
                        record.srmData[currBin] = radialData[currBin];
                    }
                } else {
                    for (int currBin = currBinPtr; currBin < maxBin; currBin++) {
                        // Threshold to not give "NO DATA" pixels a non-zero
                        // value and keep current RF values
                        if (radialData[currBin] == 1) {
                            // RF value
                            record.srmData[currBin] = (byte) 1;
                        } else if (radialData[currBin] != 0) {
                            // Add delta to the radialPixel
                            record.srmData[currBin] = (byte) (radialData[currBin] + deltaInt);
                        }
                    }
                }

                currBinPtr = maxBin;
            }
        }
    }

    public static double getSRMSpeed(RadarRecord record) {
        return record.srmSpeed;
    }

    public static double getSRMDirection(RadarRecord record) {
        return record.srmDirection;
    }

    public static Date getSRMMovement(RadarRecord record) {
        return record.srmMovement;
    }

    public static String getSRMSourceName(RadarRecord record) {
        return record.srmSourceName;
    }

    /**
     * Sets the wind vector for 8-bit SRM and creates the product if the current
     * record is 8-bit Velocity that is the basis for it.
     */
    public static void setSRMData(RadarRecord record, double direction,
            double speed) {
        record.srmSpeed = speed;
        record.srmDirection = direction;

        RadarRecordUtil.calculateSRM8(record);
    }

    /**
     * Sets the wind vector for 8-bit SRM and creates the product if the current
     * record is 8-bit Velocity that is the basis for it.
     */
    public static void setSRMData(RadarRecord record, double direction,
            double speed, Date movement, String sourceName) {
        record.srmSpeed = speed;
        record.srmDirection = direction;
        record.srmMovement = movement;
        record.srmSourceName = sourceName;
        record.setMnemonic("SRM");
        RadarRecordUtil.calculateSRM8(record);
    }

    public static byte getSRMDataValue(RadarRecord record, int radial, int bin) {
        if (record.srmData == null) {
            RadarRecordUtil.calculateSRM8(record);
        }

        return (record.srmData != null) ? record.srmData[radial
                * record.getNumBins() + bin] : 0;
    }

    public static boolean hasSRM(RadarRecord record) {
        return record.srmSourceName != null;
    }

    private static final DHRValues[] ADAP32_VALUES = { DHRValues.BEAMWIDTH,
            DHRValues.BLOCKAGETHRESHOLD, DHRValues.CLUTTERTHRESHOLD,
            DHRValues.WEIGHTTHRESHOLD, DHRValues.FULLHYBRIDSCANTHRESH,
            DHRValues.LOWREFLTHRESHOLD, DHRValues.RAINDETREFLTHRESHOLD,
            DHRValues.RAINDETAREATHRESHOLD, DHRValues.RAINDETTIMETHRESHOLD,
            DHRValues.ZRMULTCOEFF, DHRValues.ZRPOWERCOEFF,
            DHRValues.MINREFLTORATE, DHRValues.MAXREFLTORATE,
            DHRValues.NUMEXCLZONE, DHRValues.RANGECUTOFF,
            DHRValues.RANGEEFFCOEFF1, DHRValues.RANGEEFFCOEFF2,
            DHRValues.RANGEEFFCOEFF3, DHRValues.MINPRECIPRATEINCL,
            DHRValues.MAXPRECIPRATEALLOW, DHRValues.THRESHELAPSEDTIME,
            DHRValues.MAXTIMEFORINTERP, DHRValues.MINTIMEHOURLYPERIOD,
            DHRValues.THRESHOLDHROUTLIER, DHRValues.ENDTIMEGAGEACCUM,
            DHRValues.MAXPERIODACCUMVAL, DHRValues.MAXHOURLYACCUMVAL,
            DHRValues.TIMEBIASEST, DHRValues.THRESHNOGAGERADAR,
            DHRValues.RESETBIASVALUE, DHRValues.LONGESTALLOWLAG,
            DHRValues.BIASAPPLIEDFLAG };

    public static Map<DHRValues, Double> getDHRValues(RadarRecord record) {
        Map<DHRValues, Double> map = new HashMap<DHRValues, Double>();
        String text = null;
        SymbologyBlock sb = record.getSymbologyBlock();
        if (sb != null) {
            // According to the ICD the alphanumeric data for DHR can be found
            // in the second layer of the symbology block in a TextSymbolPacketn
            // with code 1.
            for (Layer layer : sb.getLayers()) {
                for (SymbologyPacket packet : layer.getPackets()) {
                    if (packet instanceof TextSymbolPacket) {
                        TextSymbolPacket tsp = (TextSymbolPacket) packet;
                        if (tsp.getTheText().contains("PSM")) {
                            text = tsp.getTheText();
                        }
                    }
                }
            }
        }
        if (text == null) {
            return map;
        }
        int vi = 0;
        int nv = text.length() / 8;
        int precipCat = 0;
        boolean biasApplied = false;
        double biasCalculated = 1.0;
        Integer flagZeroHybrid = null;

        String[] v = new String[nv];
        for (vi = 0; vi < nv; ++vi)
            v[vi] = text.substring(vi * 8, (vi + 1) * 8);

        vi = 0;
        while (vi < nv) {
            String s = v[vi++];
            if (s.equals("PSM ( 6)")) {
                precipCat = (int) parseDHRValue(v[vi + 4]);
                map.put(DHRValues.PRECIPCAT, (double) precipCat);
                vi += 6;
            } else if (s.equals("ADAP(32)")) {
                for (int i = 0; i < ADAP32_VALUES.length; ++i)
                    map.put(ADAP32_VALUES[i], parseDHRValue(v[vi++]));
                biasApplied = map.get(DHRValues.BIASAPPLIEDFLAG) > 0;
                while (vi < nv) {
                    s = v[vi++];
                    if (s.equals("SUPL(15)")) {
                        /*
                         * // average scan date/time are never used...
                         * map.put(DHRValues.AVGSCANDATE, parseDHRValue(text, vi
                         * + 0)); map.put(DHRValues.AVGSCANTIME,
                         * parseDHRValue(text, vi + 1));
                         */
                        flagZeroHybrid = (int) parseDHRValue(v[vi + 2]);
                        if (flagZeroHybrid != 0 && flagZeroHybrid != 1)
                            flagZeroHybrid = 0; // should print warning
                        vi += 15;
                    } else if (s.equals("BIAS(11)")) {
                        biasCalculated = parseDHRValue(v[vi + 8]);
                        vi += 11;
                    }
                }
            } else if (s.equals("ADAP(38)")) {
                // Don't have documentation for older formats, so copying logic
                // from A1 decodeDHR.C.
                map.put(DHRValues.ZRMULTCOEFF, parseDHRValue(v[vi + 9]));
                map.put(DHRValues.ZRPOWERCOEFF, parseDHRValue(v[vi + 10]));
                map.put(DHRValues.MAXPRECIPRATEALLOW, parseDHRValue(v[vi + 25]));
                map.put(DHRValues.BIASAPPLIEDFLAG, parseDHRValue(v[vi + 37]));
                s = v[46];
                if (s.equals("SUPL(15)")) {
                    biasCalculated = parseDHRValue(v[71]);
                    flagZeroHybrid = (int) parseDHRValue(v[49]);
                    if (flagZeroHybrid != 0 && flagZeroHybrid != 1)
                        flagZeroHybrid = 0; // should print warning
                } else if (s.equals("SUPL(13)")) {
                    biasCalculated = parseDHRValue(v[69]);
                }
                vi = nv;
            } else if (s.equals("ADAP(46)")) {
                map.put(DHRValues.ZRMULTCOEFF, parseDHRValue(v[vi + 9]));
                map.put(DHRValues.ZRPOWERCOEFF, parseDHRValue(v[vi + 10]));
                map.put(DHRValues.MAXPRECIPRATEALLOW, parseDHRValue(v[vi + 25]));
                s = v[68];
                if (s.equals("BIAS(11)")) {
                    map.put(DHRValues.BIASAPPLIEDFLAG, parseDHRValue(v[53]));
                    biasCalculated = parseDHRValue(v[77]);
                } else if (s.equals("BIAS( 9)")) {
                    map.put(DHRValues.BIASAPPLIEDFLAG, parseDHRValue(v[53]));
                    biasCalculated = parseDHRValue(v[73]);
                }
                vi = nv;
            }
        }
        if (flagZeroHybrid != null)
            map.put(DHRValues.FLAGZEROHYBRID, (double) flagZeroHybrid);
        if (!biasApplied) {
            biasCalculated = 1.0;
        }
        map.put(DHRValues.BIAS, biasCalculated);

        // Also include logic from A1 FFMPContainer::read(), FFMP_ORPG case
        boolean havePrecip;
        if (flagZeroHybrid == null) {
            havePrecip = precipCat > 0;
        } else {
            havePrecip = flagZeroHybrid == 0;
        }
        map.put(DHRValues.HAVE_PRECIP, havePrecip ? 1.0 : 0);
        map.put(DHRValues.BIAS_TO_USE, havePrecip ? biasCalculated : 1.0);

        return map;
    }

    private static double parseDHRValue(String text) {
        String s = text.trim();
        if (s.equals("T"))
            return 1;
        else if (s.equals("F"))
            return 0;
        else
            return Double.parseDouble(s);
    }

/**
     * DR#11705: SCAN missing row(s) comparing to radar Comb Att Table.
     * 
     * Error cause: RadarConstants.GRAPHIC_BLOCK as a Regular Expression
     * pattern can not match some variations in Graphic Block Texts with  
     * "<" and ">" having spaces between them and their associated numbers
     * ( MXHAILSIZE and TOP ).
     * 
     * Fix: replace all "<" and ">" with space: " "
     * 
     * @param : Graphic Block Text that may contain ">" and/or "<".
     * @return: String with ">" and/or "<" replaced by space.      
     */
    private static String getNormalizedGBText(String text) {

        if (text == null || text.isEmpty()
                || ((!text.contains(">")) && (!text.contains("<"))))
            return text;

        /*
         * contains only ">"
         */
        if (!text.contains("<"))
            return text.replaceAll(">", " ");

        /*
         * contains only "<"
         */
        if (!text.contains(">"))
            return text.replaceAll("<", " ");

        /*
         * contains both "<" and ">"
         */
        return text.replaceAll(">", " ").replaceAll("<", " ");

    }
}
