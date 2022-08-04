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

import java.util.Set;

import javax.measure.Quantity;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.topo.CachedTopoQuery;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.raytheon.uf.viz.core.rsc.interrogation.StringInterrogationKey;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.quantity.Quantities;

/**
 * Interrogator for radial radar products.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2010            mnash       Initial creation
 * Feb 15, 2013 1638       mschenke    Got rid of viz/edex topo classes  and
 *                                     moved into common
 * Jul 31, 2013 2190       mschenke    Made "msl" interrogate key return
 *                                     height unformatted height data value
 * Aug 06, 2013 2235       bsteffen    Added Caching version of TopoQuery.
 * Sep 13, 2016 3239       nabowle     Use the Interrogatable API.
 * Sep 25, 2018 17574      kshrestha   Display appropriate temperatures for Radar/Tilts levels.
 * Nov 26, 2018 20926      jdynina     Display MSL/AGL for below 0 degree elevs.
 * Jul 17, 2019 21421      jdynina     Bring MSL calculation in line with RPG
 *
 * </pre>
 *
 * @author mnash
 */

public class RadarRadialInterrogator extends RadarDefaultInterrogator
        implements IRadarInterrogator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarRadialInterrogator.class);

    private static final double EFFECTIVE_EARTH_RADIUS = 1.21 * 6371000; // meters

    public static final InterrogationKey<Quantity<Length>> ELEVATION = new StringInterrogationKey<>(
            "Elevation", Interrogator.getTypedQuantityClass());

    public static final InterrogationKey<Number> LATITUDE = new StringInterrogationKey<>(
            "Latitude", Number.class);

    public static final InterrogationKey<Number> LONGITUDE = new StringInterrogationKey<>(
            "Longitude", Number.class);

    protected RadarDataInterrogator interrogator = new RadarDataInterrogator(
            null);

    private boolean useTopo = true;

    @Override
    public int addParameters(RadarRecord radarRecord, Coordinate latLon,
            InterrogateMap dataMap, Set<InterrogationKey<?>> keys) {
        int dataValue = 0;
        // Unit Converters
        UnitConverter feetToMeters = USCustomary.FOOT.getConverterTo(SI.METRE);
        UnitConverter metersToFeet = SI.METRE.getConverterTo(USCustomary.FOOT);
        UnitConverter metersToNm = SI.METRE.getConverterTo(USCustomary.NAUTICAL_MILE);

        // Get value for Radial
        dataValue = interrogator.getDataValue(radarRecord, latLon);

        // Find MSL and AGL
        double range = interrogator.getLastRangeInterrogated();
        double azimuth = interrogator.getLastAzimuthInterrogated();
        double radarElevation = feetToMeters
                .convert(radarRecord.getElevation().doubleValue());
        addValueToMap(dataMap, keys, IRadarInterrogator.AZIMUTH,
                Quantities.getQuantity((int) azimuth, NonSI.DEGREE_ANGLE));
        addValueToMap(dataMap, keys, IRadarInterrogator.RANGE, Quantities.getQuantity(
                Math.round(metersToNm.convert(range)), USCustomary.NAUTICAL_MILE));
        addValueToMap(dataMap, keys, ELEVATION,
                Quantities.getQuantity(radarElevation, SI.METRE));
        addValueToMap(dataMap, keys, LATITUDE, radarRecord.getLatitude());
        addValueToMap(dataMap, keys, LONGITUDE, radarRecord.getLongitude());

        if (keys.contains(MSL) || keys.contains(AGL)) {
            double tiltAngle = Math
                    .toRadians(radarRecord.getTrueElevationAngle());
            if (tiltAngle > -0.1) {
                double heightAboveRadar = range * Math.sin(tiltAngle)
                        + (range * range) / (2.0 * EFFECTIVE_EARTH_RADIUS);
                double msl = radarElevation + heightAboveRadar;

                double topoHeight = Double.NaN;
                if (useTopo) {
                    CachedTopoQuery topoQuery = CachedTopoQuery.getInstance();
                    if (topoQuery != null) {
                        topoHeight = topoQuery.getHeight(latLon);
                    } else {
                        useTopo = false;
                        statusHandler.handle(Priority.PROBLEM,
                                "Topo Error: Radar AGL sampling has been disabled");
                    }
                }
                double agl = msl - topoHeight;
                // modified msl and agl based on A1 code
                addValueToMap(dataMap, keys, MSL,
                        Quantities.getQuantity(metersToFeet.convert(msl), USCustomary.FOOT));
                addValueToMap(dataMap, keys, AGL,
                        Quantities.getQuantity(metersToFeet.convert(agl), USCustomary.FOOT));
            }
        }
        return dataValue;
    }

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> keys = super.getInterrogationKeys();
        keys.add(ELEVATION);
        keys.add(LATITUDE);
        keys.add(LONGITUDE);
        return keys;
    }
}
