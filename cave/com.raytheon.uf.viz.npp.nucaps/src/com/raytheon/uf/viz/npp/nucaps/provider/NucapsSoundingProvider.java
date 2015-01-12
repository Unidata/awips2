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
package com.raytheon.uf.viz.npp.nucaps.provider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.npp.nucaps.NucapsRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.nsharp.SoundingLayerBuilder;
import com.raytheon.uf.viz.npp.NPPTimeUtility;
import com.raytheon.uf.viz.sounding.providers.AbstractVerticalSoundingProvider;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

/**
 * NUCAPS implementation of {@link IVerticalSoundingProvider}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2013 2190       mschenke    Initial creation
 * Aug 15, 2013 2260       bsteffen    Switch poessounding to NSharp.
 * Aug 27, 2013 2190       mschenke    Fixed point query request
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NucapsSoundingProvider extends
        AbstractVerticalSoundingProvider<STRtree> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(NucapsSoundingProvider.class);

    private static String[] SOUNDING_PARAMS = { NucapsRecord.LATITUDE,
            NucapsRecord.LONGITUDE, NucapsRecord.PDV_SURFACE_PRESSURE,
            NucapsRecord.PDV_PRESSURE, NucapsRecord.PDV_TEMPERATURE,
            NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO };

    private static final double ENVELOPE_DISTANCE_DEG = 1.0;

    private static final double MAX_SOUNDING_DISTANCE_METERS = 100 * 1000;

    private static final long GROUP_TIMERANGE_MILLIS = 15 * TimeUtil.MILLIS_PER_MINUTE;

    private static class NucapsVerticalSounding {

        private final PointDataView pdv;

        private final Coordinate soundingLocation;

        private VerticalSounding sounding;

        public NucapsVerticalSounding(PointDataView pdv) {
            this.pdv = pdv;
            soundingLocation = new Coordinate(
                    pdv.getFloat(NucapsRecord.LONGITUDE),
                    pdv.getFloat(NucapsRecord.LATITUDE));
        }

        public Coordinate getSoundingLocation() {
            return soundingLocation;
        }

        public synchronized VerticalSounding getSounding() {
            if (sounding == null) {
                VerticalSounding sounding = new VerticalSounding();
                sounding.setStationId(NucapsRecord.PLUGIN_NAME);
                Coordinate location = getSoundingLocation();
                sounding.setName(GeoUtil.formatCoordinate(location));
                sounding.setLongitude((float) location.x);
                sounding.setLongitude((float) location.y);

                // Get pressure values
                Number[] pressures = pdv
                        .getNumberAllLevels(NucapsRecord.PDV_PRESSURE);
                Unit<Pressure> pressureUnit = pdv.getUnit(
                        NucapsRecord.PDV_PRESSURE).asType(Pressure.class);

                // Get temperature values
                Number[] temperatures = pdv
                        .getNumberAllLevels(NucapsRecord.PDV_TEMPERATURE);
                Unit<Temperature> tempUnit = pdv.getUnit(
                        NucapsRecord.PDV_TEMPERATURE).asType(Temperature.class);

                // Water-vapor mixing ratios
                Number[] wvMixingRatios = pdv
                        .getNumberAllLevels(NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO);
                Unit<Dimensionless> wvUnit = pdv.getUnit(
                        NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO).asType(
                        Dimensionless.class);

                if (pressures.length == temperatures.length) {
                    int length = pressures.length;
                    List<SoundingLayer> layers = new ArrayList<SoundingLayer>(
                            length);

                    float surfacePressure = pdv
                            .getFloat(NucapsRecord.PDV_SURFACE_PRESSURE);
                    for (int idx = 0; idx < length; ++idx) {
                        double pressure = pressures[idx].doubleValue();
                        // Don't add entries where pressure below surface
                        if (pressure <= surfacePressure) {
                            SoundingLayerBuilder builder = new SoundingLayerBuilder();
                            builder.addPressure(pressure, pressureUnit);
                            builder.addTemperature(
                                    temperatures[idx].doubleValue(), tempUnit);
                            builder.addSpecificHumidity(
                                    wvMixingRatios[idx].doubleValue(), wvUnit);

                            layers.add(builder.toSoundingLayer());
                        }
                    }
                    sounding.addLayers(layers);
                }
                this.sounding = sounding;
            }
            return sounding;
        }

    }

    private GeodeticCalculator gc = new GeodeticCalculator();

    @Override
    protected void populateBaseConstraints(
            Map<String, RequestConstraint> constraints) {
        super.populateBaseConstraints(constraints);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider#
     * getSoundingSource()
     */
    @Override
    public String getSoundingSource() {
        return NucapsRecord.PLUGIN_NAME;
    }

    @Override
    protected DataTime[] queryForSoundingTimes(
            Map<String, RequestConstraint> constraints) {
        return NPPTimeUtility.groupTimes(
                Arrays.asList(super.queryForSoundingTimes(constraints)),
                GROUP_TIMERANGE_MILLIS).toArray(new DataTime[0]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.sounding.providers.AbstractVerticalSoundingProvider
     * #queryForData(java.util.Map, com.raytheon.uf.common.time.DataTime,
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected STRtree queryForData(Map<String, RequestConstraint> constraints,
            DataTime time, Coordinate location) {
        STRtree tree = new STRtree();

        try {
            PointDataContainer pdc = PointDataRequest
                    .requestPointDataAllLevels(time.getValidPeriod(),
                            NucapsRecord.PLUGIN_NAME, SOUNDING_PARAMS, null,
                            constraints);
            int size = pdc.getCurrentSz();
            for (int i = 0; i < size; ++i) {
                NucapsVerticalSounding sounding = new NucapsVerticalSounding(
                        pdc.readRandom(i));
                Envelope env = buildEnvelope(sounding.getSoundingLocation());
                tree.insert(env, sounding);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return tree;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.sounding.providers.AbstractVerticalSoundingProvider
     * #createSounding(com.raytheon.uf.common.time.DataTime,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[],
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected VerticalSounding createSounding(DataTime time, STRtree tree,
            Coordinate location) {
        Envelope env = buildEnvelope(location);
        List<?> results = tree.query(env);
        double minDist = Double.MAX_VALUE;
        NucapsVerticalSounding soundingToUse = null;
        for (Object result : results) {
            NucapsVerticalSounding nucapsSounding = (NucapsVerticalSounding) result;
            gc.setStartingGeographicPoint(location.x, location.y);
            Coordinate soundingLocation = nucapsSounding.getSoundingLocation();
            gc.setDestinationGeographicPoint(soundingLocation.x,
                    soundingLocation.y);
            double distance = gc.getOrthodromicDistance();
            if (distance < minDist && distance < MAX_SOUNDING_DISTANCE_METERS) {
                minDist = distance;
                soundingToUse = nucapsSounding;
            }
        }

        VerticalSounding sounding = null;
        if (soundingToUse != null) {
            sounding = soundingToUse.getSounding();
        }
        return sounding;
    }

    private static Envelope buildEnvelope(Coordinate location) {
        double longitude = location.x;
        double latitude = location.y;
        return new Envelope(new Coordinate(longitude - ENVELOPE_DISTANCE_DEG,
                latitude - ENVELOPE_DISTANCE_DEG), new Coordinate(longitude
                + ENVELOPE_DISTANCE_DEG, latitude + ENVELOPE_DISTANCE_DEG));
    }

}
