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
package com.raytheon.viz.radar.interrogators;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Set;

import javax.measure.Quantity;
import javax.measure.UnitConverter;
import javax.measure.quantity.Angle;
import javax.measure.quantity.Speed;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.GFMPacket.GFMAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.quantity.Quantities;

/**
 * Interrogator class for Radar GFM sampling.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2013   DCS51      zwang       Initial creation
 * 06/18/2013   DR16162    zwang       Remove "wind behind"
 * Sep 13, 2016 3239       nabowle     Use the Interrogatable API.
 *
 * </pre>
 *
 * @author zwang
 */

public class RadarGFMInterrogator extends RadarGraphicInterrogator
        implements IRadarInterrogator {

    public static final InterrogationKey<Quantity<Speed>> WS_HAZARD = new InterrogationKey<>();

    public static final InterrogationKey<Quantity<Angle>> DIRECTION = new InterrogationKey<>();

    public static final InterrogationKey<Quantity<Speed>> SPEED = new InterrogationKey<>();

    public RadarGFMInterrogator() {
        super();
    }

    @Override
    public InterrogateMap sample(RadarRecord radarRecord, Coordinate latLon,
            ColorMapParameters params, Set<InterrogationKey<?>> keys) {
        InterrogateMap dataMap = new InterrogateMap();
        sample(radarRecord, latLon, keys, dataMap);
        return dataMap;
    }

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {

        String valueString = getDataValues(radarRecord, latLon, dataMap, keys);
        addValueToMap(dataMap, keys, VALUE_STRING, valueString);
        return 0;
    }

    private String getDataValues(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        StringBuffer rval = new StringBuffer();

        Coordinate c1 = new Coordinate(latLon.x + .025, latLon.y + .025);
        Coordinate c2 = new Coordinate(latLon.x - .025, latLon.y - .025);
        Envelope env = new Envelope(c1, c2);

        UnitConverter metersPerSecondToKnots = SI.METRE_PER_SECOND
                .getConverterTo(USCustomary.KNOT);

        NumberFormat formatter = new DecimalFormat("###");

        String windX, windY;
        double pU = 0.0;
        double pV = 0.0;
        double pSpd = 0.0;
        double pDir = 0.0;
        double wX = 0.0;
        double wY = 0.0;

        if (radarRecord.getProductCode() == 140) {

            for (RadarDataKey key : radarRecord.getSymbologyData().keySet()) {

                // Get the data for the select feature
                RadarDataPoint currPoint = radarRecord.getSymbologyData()
                        .get(key);

                AreaComponent currFeature;
                HashMap<Integer, HashMap<Integer, GenericDataComponent>> currPointData = currPoint
                        .getDisplayGenericPointData();

                for (Integer type : currPointData.keySet()) {
                    for (GenericDataComponent currComp : currPointData.get(type)
                            .values()) {
                        currFeature = (AreaComponent) currComp;

                        int numParam = currFeature.getParameters().size();

                        if (numParam == 11) {
                            windX = currFeature.getValue(
                                    GFMPacket.GFMAttributeIDs.WINDBEHINDX
                                            .getName());
                            if ((windX != null) && (windX.length() > 0)) {
                                wX = Float.parseFloat(windX);
                            }
                            windY = currFeature.getValue(
                                    GFMPacket.GFMAttributeIDs.WINDBEHINDY
                                            .getName());
                            if ((windY != null) && (windY.length() > 0)) {
                                wY = Float.parseFloat(windY);
                            }

                            // windX/windY unit is 1km, convert to 1/4km
                            Coordinate currStorm = radarRecord
                                    .convertStormLatLon(wX * 4.0, wY * 4.0);

                            if (env.contains(currStorm)
                                    && currPoint.isVisible()) {

                                // propU
                                String pUStr = currFeature.getValue(
                                        GFMAttributeIDs.PROPU.toString());
                                if ((pUStr != null) && (pUStr.length() > 0)) {
                                    pU = metersPerSecondToKnots
                                            .convert(Double.parseDouble(pUStr));
                                }

                                // propV
                                String pVStr = currFeature.getValue(
                                        GFMAttributeIDs.PROPV.toString());
                                if ((pVStr != null) && (pVStr.length() > 0)) {
                                    pV = metersPerSecondToKnots
                                            .convert(Double.parseDouble(pVStr));
                                }
                                pSpd = getSpeed(pU, pV);
                                pDir = getDir(pU, pV);
                                addValueToMap(dataMap, keys, SPEED,
                                        Quantities.getQuantity(pSpd, USCustomary.KNOT));
                                addValueToMap(dataMap, keys, DIRECTION, Quantities
                                        .getQuantity(pDir, NonSI.DEGREE_ANGLE));

                                // wsHazard
                                String wsStr = currFeature.getValue(
                                        GFMAttributeIDs.WSHAZARD.toString());
                                if ((wsStr != null) && (wsStr.length() > 0)) {
                                    double ws = metersPerSecondToKnots
                                            .convert(Double.parseDouble(wsStr));
                                    wsStr = formatter.format(ws);
                                    addValueToMap(dataMap, keys, WS_HAZARD,
                                            Quantities.getQuantity(ws, USCustomary.KNOT));
                                }

                                rval.append("Movement " + formatter.format(pSpd)
                                        + "kts@" + formatter.format(pDir)
                                        + "\n");
                                rval.append(
                                        "Wind Shear Hazard " + wsStr + "kts ");

                            }
                            break;
                        }
                    }
                }

            }
        }

        return rval.toString();
    }

    // Get wind speed from (u,v)
    private double getSpeed(double u, double v) {
        double spd = 0.0;
        spd = Math.sqrt(u * u + v * v);

        return spd;
    }

    // Get wind direction from (u,v)
    private double getDir(double u, double v) {
        double R_EQ_ZERO = 1.0E-10;
        double DEG_HALF_PI = 180.0;

        double spd = 0.0;
        double dir = 0.0;
        spd = Math.sqrt(u * u + v * v);

        if (spd < R_EQ_ZERO) {
            dir = 0.0;
        } else {
            dir = (Math.toDegrees(Math.atan2(u, v)) + DEG_HALF_PI + 1.0E-3);
        }

        return dir;
    }

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> keys = super.getInterrogationKeys();
        keys.add(DIRECTION);
        keys.add(SPEED);
        keys.add(WS_HAZARD);
        return keys;
    }

    @Override
    public Set<InterrogationKey<?>> getValueStringKeys() {
        Set<InterrogationKey<?>> keys = super.getValueStringKeys();
        keys.add(DIRECTION);
        keys.add(SPEED);
        keys.add(WS_HAZARD);
        return keys;
    }
}
