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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.edex.meteolibrary.Meteolibrary;
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
import com.raytheon.uf.viz.npp.NPPTimeUtility;
import com.raytheon.uf.viz.npp.sounding.math.NPPSoundingCalculations;
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
 * Jul 25, 2013       2190 mschenke    Initial creation
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

    private static String[] SOUNDING_PARAMS = {
            NucapsRecord.PDV_SURFACE_PRESSURE, NucapsRecord.PDV_PRESSURE,
            NucapsRecord.PDV_TEMPERATURE,
            NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO };

    private static final double ENVELOPE_DISTANCE_DEG = 1.0;

    private static final double MAX_SOUNDING_DISTANCE_METERS = 50 * 1000;

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
                sounding.setLongitude(location.x);
                sounding.setLongitude(location.y);

                // Get pressure values
                Number[] pressures = pdv
                        .getNumberAllLevels(NucapsRecord.PDV_PRESSURE);
                Unit<?> pressureUnit = pdv.getUnit(NucapsRecord.PDV_PRESSURE);
                UnitConverter dataToSLPressureUnit = pressureUnit
                        .getConverterTo(SoundingLayer.DATA_TYPE.PRESSURE
                                .getUnit());

                // Get temperature values
                Number[] temperatures = pdv
                        .getNumberAllLevels(NucapsRecord.PDV_TEMPERATURE);
                Unit<?> tempUnit = pdv.getUnit(NucapsRecord.PDV_TEMPERATURE);
                UnitConverter dataToSLTempUnit = tempUnit
                        .getConverterTo(SoundingLayer.DATA_TYPE.TEMPERATURE
                                .getUnit());

                // Water-vapor mixing ratios
                Number[] wvMixingRatios = pdv
                        .getNumberAllLevels(NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO);
                Unit<?> wvUnit = pdv
                        .getUnit(NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO);
                UnitConverter wvUnitConverter = wvUnit
                        .getConverterTo(NPPSoundingCalculations.H2O_UNIT);

                if (pressures.length == temperatures.length) {
                    int length = pressures.length;
                    List<SoundingLayer> layers = new ArrayList<SoundingLayer>(
                            length);

                    float surfacePressure = (float) dataToSLPressureUnit
                            .convert(pdv
                                    .getFloat(NucapsRecord.PDV_SURFACE_PRESSURE));
                    for (int idx = 0; idx < length; ++idx) {
                        float pressure = (float) dataToSLPressureUnit
                                .convert(pressures[idx].doubleValue());
                        // Don't add entries where pressure below surface
                        if (pressure <= surfacePressure) {
                            // Pressure to height
                            float gh = Meteolibrary.ptozsa(
                                    new float[] { (float) pressure }, 0);
                            // Temperature
                            float temperature = (float) dataToSLTempUnit
                                    .convert(temperatures[idx].doubleValue());
                            // Water vapor mixing ratio
                            float h20 = (float) wvUnitConverter
                                    .convert(wvMixingRatios[idx].doubleValue());
                            // Calculate dewpoint and RH from pressure and h20
                            float dewpoint = NPPSoundingCalculations
                                    .convertH2OtoDewpoint(h20, pressure);

                            layers.add(new SoundingLayer(pressure, gh,
                                    temperature, dewpoint,
                                    SoundingLayer.MISSING,
                                    SoundingLayer.MISSING,
                                    SoundingLayer.MISSING));
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
            PointDataContainer pdc = PointDataRequest.requestPointData(
                    time.getValidPeriod(), NucapsRecord.PLUGIN_NAME,
                    SOUNDING_PARAMS, null, constraints);
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
