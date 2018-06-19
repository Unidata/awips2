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

import java.util.HashMap;
import java.util.Set;

import javax.measure.Measure;
import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.MBAPacket.MBAAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.MBAPacket.MBACategory;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Interrogator class for Radar MBA sampling.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/06/2014   DCS 16776  zwang       Initial creation
 * Sep 13, 2016 3239       nabowle     Use the Interrogatable API.
 *
 * </pre>
 *
 * @author zwang
 */

public class RadarMBAInterrogator extends RadarGraphicInterrogator
        implements IRadarInterrogator {

    public static final InterrogationKey<Measure<? extends Number, Velocity>> MAX_SPEED = new InterrogationKey<>();

    public static final InterrogationKey<Measure<? extends Number, Velocity>> STRENGTH = new InterrogationKey<>();

    public static final InterrogationKey<String> CATEGORY = new InterrogationKey<>();

    public RadarMBAInterrogator() {
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

        UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
                .getConverterTo(NonSI.KNOT);

        // Determine if the feature should be sampled
        RadarDisplayControls currentSettings = RadarDisplayManager.getInstance()
                .getCurrentSettings();

        if (radarRecord.getProductCode() == 196) {

            for (RadarDataKey key : radarRecord.getSymbologyData().keySet()) {

                Coordinate currStorm = new Coordinate(key.getLon(),
                        key.getLat());

                if (env.contains(currStorm)) {
                    // Get the data for the select feature
                    RadarDataPoint currPoint = radarRecord.getSymbologyData()
                            .get(key);
                    AreaComponent currFeature;
                    HashMap<Integer, HashMap<Integer, GenericDataComponent>> currPointData = currPoint
                            .getDisplayGenericPointData();

                    for (Integer type : currPointData.keySet()) {
                        for (GenericDataComponent currComp : currPointData
                                .get(type).values()) {
                            currFeature = (AreaComponent) currComp;

                            // Category: CATEGORY
                            String category = currFeature.getValue(
                                    MBAAttributeIDs.CATEGORY.toString());

                            // if MBA is filtered out by category, do not sample
                            int catValue = category.equals("") ? 0
                                    : Integer.parseInt(category);

                            // By default, do not show MBA Wind Shear
                            int minCat = 1;
                            if (currentSettings.isMbaShowWindShear()) {
                                minCat = 0;
                            }

                            if (catValue >= minCat) {

                                // Microburst strength: DELTAV
                                String strength = currFeature.getValue(
                                        MBAAttributeIDs.DELTAV.toString());
                                if ((strength != null)
                                        && (strength.length() > 0)) {
                                    double strengthValue = metersPerSecondToKnots
                                            .convert(new Double(strength));
                                    strength = String.format("%dkts",
                                            (int) strengthValue);
                                    addValueToMap(dataMap, keys, STRENGTH,
                                            Measure.valueOf(strengthValue,
                                                    NonSI.KNOT));
                                }

                                // Maximum wind speed: MAXWINDSPEED
                                String maxSpeed = currFeature
                                        .getValue(MBAAttributeIDs.MAXWINDSPEED
                                                .toString());
                                if ((maxSpeed != null)
                                        && (maxSpeed.length() > 0)) {
                                    double spdValue = metersPerSecondToKnots
                                            .convert(new Double(maxSpeed));
                                    maxSpeed = String.format("%dkts",
                                            (int) spdValue);
                                    addValueToMap(dataMap, keys, MAX_SPEED,
                                            Measure.valueOf(spdValue,
                                                    NonSI.KNOT));
                                }

                                // Maximum shear: MAXSHEAR
                                String maxShear = currFeature.getValue(
                                        MBAAttributeIDs.MAXSHEAR.toString());
                                if ((maxShear != null)
                                        && (maxShear.length() > 0)) {
                                    double shearValue = new Double(maxShear);
                                    maxShear = String.format("%.4f/s",
                                            shearValue);
                                    addValueToMap(dataMap, keys, SHEAR,
                                            shearValue);
                                }

                                String catName = MBACategory
                                        .getCatName(catValue);
                                addValueToMap(dataMap, keys, CATEGORY, catName);

                                rval.append(catName);
                                rval.append(" " + maxShear);
                                rval.append(" maxV " + maxSpeed);
                                rval.append(" deltaV " + strength);
                            }
                        }
                    }
                }
            }
        }
        return rval.toString();
    }

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> keys = super.getInterrogationKeys();
        keys.add(CATEGORY);
        keys.add(MAX_SPEED);
        keys.add(STRENGTH);
        return keys;
    }

    @Override
    public Set<InterrogationKey<?>> getValueStringKeys() {
        Set<InterrogationKey<?>> keys = super.getValueStringKeys();
        keys.add(SHEAR);
        keys.add(CATEGORY);
        keys.add(MAX_SPEED);
        keys.add(STRENGTH);
        return keys;
    }

}
