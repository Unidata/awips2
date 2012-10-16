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
package com.raytheon.uf.viz.npp.crimss;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.npp.crimss.CrimssRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 5, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class CrimssNSharpResourceData extends D2DNSharpResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrimssNSharpResourceData.class);

    private static final String PLUGIN = "crimss";

    private static final String[] PARAMETERS = { CrimssRecord.LONGITUDE,
            CrimssRecord.LATITUDE, CrimssRecord.PDV_SURFACE_PRESSURE,
            CrimssRecord.PDV_ALTITUDE, CrimssRecord.PDV_P_ALTITUDE,
            CrimssRecord.PDV_H2O, CrimssRecord.PDV_P_H2O,
            CrimssRecord.PDV_TEMPERATURE, CrimssRecord.PDV_P_TEMPERATURE };

    private static final Unit<?> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    private static final Unit<?> HEIGHT_UNIT = SI.METER;

    private static final Unit<?> TEMPERATURE_UNIT = SI.CELSIUS;

    private static final Unit<?> H2O_UNIT = SI.GRAM.divide(SI.KILOGRAM);

    private static final Unit<?> DEWPOINT_UNIT = SI.CELSIUS;

    public CrimssNSharpResourceData() {
        super("CRiMSS");
    }

    @Override
    protected void preparePointInfo() throws VizException {
        // everything should already be set
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData#getSoundingCube
     * (gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo)
     */
    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        DataTime time = new DataTime(stnInfo.getReftime());
        try {
            PointDataContainer pdc = PointDataRequest
                    .requestPointDataAllLevels(time, PLUGIN, PARAMETERS, null,
                            getMetadataMap());
            PointDataView pdv = null;
            Coordinate closest = null;
            for (int i = 0; i < pdc.getCurrentSz(); i++) {
                PointDataView testPdv = pdc.readRandom(i);
                Coordinate p = new Coordinate(
                        testPdv.getFloat(CrimssRecord.LONGITUDE),
                        testPdv.getFloat(CrimssRecord.LATITUDE));
                if (closest == null
                        || coordinate.distance(p) < coordinate
                                .distance(closest)) {
                    pdv = testPdv;
                    closest = p;
                }
            }
            if (pdv == null) {
                return null;
            }
            List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
            layers.add(getSurfacePressureLayer(pdv));
            layers.addAll(getHeightLayers(pdv));
            layers.addAll(getTemperatureLayers(pdv));
            layers.addAll(getDewpointLayers(pdv));
            Collections.sort(layers,
                    NsharpDataHandling.reversePressureHeightWindComparator());
            mergeDuplicates(layers);
            // We have to interpolate everything so that height,temperature, and
            // dewpoint are available on all levels.
            interpolateHeight(layers);
            interpolateTemperature(layers);
            interpolateDewpoint(layers);
            Iterator<NcSoundingLayer> iter = layers.iterator();
            while (iter.hasNext()) {
                NcSoundingLayer layer = iter.next();
                if (layer.getPressure() < 0) {
                    iter.remove();
                } else if (layer.getGeoHeight() < 0) {
                    iter.remove();

                } else if (layer.getTemperature() < -300) {
                    iter.remove();

                } else if (layer.getDewpoint() < -300) {
                    iter.remove();
                }
            }
            NcSoundingProfile profile = new NcSoundingProfile();
            profile.setSoundingLyLst(layers);
            // TODO populate other fields in profile
            NcSoundingCube cube = new NcSoundingCube(Arrays.asList(profile));
            cube.setRtnStatus(QueryStatus.OK);
            return cube;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return null;
    }

    private static NcSoundingLayer getSurfacePressureLayer(PointDataView pdv) {
        float surfacePressure = pdv.getFloat(CrimssRecord.PDV_SURFACE_PRESSURE);
        Unit<?> surfacePressureUnit = pdv
                .getUnit(CrimssRecord.PDV_SURFACE_PRESSURE);
        if (PRESSURE_UNIT.isCompatible(surfacePressureUnit)) {
            UnitConverter converter = surfacePressureUnit
                    .getConverterTo(PRESSURE_UNIT);
            surfacePressure = (float) converter.convert(surfacePressure);
        }
        return new NcSoundingLayer(surfacePressure, NcSoundingLayer.MISSING,
                NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                NcSoundingLayer.MISSING);
    }

    private static List<NcSoundingLayer> getHeightLayers(PointDataView pdv) {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        UnitConverter heightConverter = UnitConverter.IDENTITY;
        Unit<?> heightUnit = pdv.getUnit(CrimssRecord.PDV_ALTITUDE);
        if (HEIGHT_UNIT.isCompatible(heightUnit)) {
            heightConverter = heightUnit.getConverterTo(HEIGHT_UNIT);
        }
        UnitConverter pressureConverter = UnitConverter.IDENTITY;
        Unit<?> pressureUnit = pdv.getUnit(CrimssRecord.PDV_P_ALTITUDE);
        if (PRESSURE_UNIT.isCompatible(pressureUnit)) {
            pressureConverter = pressureUnit.getConverterTo(PRESSURE_UNIT);
        }
        Number[] altitudeArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_ALTITUDE);
        Number[] pressureArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_P_ALTITUDE);
        for (int j = 0; j < altitudeArray.length; j++) {
            float altitude = altitudeArray[j].floatValue();
            altitude = (float) heightConverter.convert(altitude);
            float pressure = pressureArray[j].floatValue();
            pressure = (float) pressureConverter.convert(pressure);
            NcSoundingLayer layer = new NcSoundingLayer(pressure, altitude,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING);
            layers.add(layer);
        }
        return layers;
    }

    private static List<NcSoundingLayer> getTemperatureLayers(PointDataView pdv) {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        UnitConverter temperatureConverter = UnitConverter.IDENTITY;
        Unit<?> temperatureUnit = pdv.getUnit(CrimssRecord.PDV_TEMPERATURE);
        if (TEMPERATURE_UNIT.isCompatible(temperatureUnit)) {
            temperatureConverter = temperatureUnit
                    .getConverterTo(TEMPERATURE_UNIT);
        }
        UnitConverter pressureConverter = UnitConverter.IDENTITY;
        Unit<?> pressureUnit = pdv.getUnit(CrimssRecord.PDV_P_TEMPERATURE);
        if (PRESSURE_UNIT.isCompatible(pressureUnit)) {
            pressureConverter = pressureUnit.getConverterTo(PRESSURE_UNIT);
        }
        Number[] temperatureArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_TEMPERATURE);
        Number[] pressureArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_P_TEMPERATURE);

        for (int j = 0; j < temperatureArray.length; j++) {
            float temperature = temperatureArray[j].floatValue();
            temperature = (float) temperatureConverter.convert(temperature);
            float pressure = pressureArray[j].floatValue();
            pressure = (float) pressureConverter.convert(pressure);
            NcSoundingLayer layer = new NcSoundingLayer(pressure,
                    NcSoundingLayer.MISSING, temperature,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING);
            layers.add(layer);
        }
        return layers;
    }

    private static List<NcSoundingLayer> getDewpointLayers(PointDataView pdv) {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        UnitConverter h2oConverter = UnitConverter.IDENTITY;
        Unit<?> h2oUnit = pdv.getUnit(CrimssRecord.PDV_H2O);
        if (H2O_UNIT.isCompatible(h2oUnit)) {
            h2oConverter = h2oUnit.getConverterTo(H2O_UNIT);
        }
        UnitConverter dewpointConverter = UnitConverter.IDENTITY;
        Unit<?> dewpointUnit = SI.KELVIN;
        if (DEWPOINT_UNIT.isCompatible(dewpointUnit)) {
            dewpointConverter = dewpointUnit.getConverterTo(DEWPOINT_UNIT);
        }
        UnitConverter pressureConverter = UnitConverter.IDENTITY;
        Unit<?> pressureUnit = pdv.getUnit(CrimssRecord.PDV_P_H2O);
        if (PRESSURE_UNIT.isCompatible(pressureUnit)) {
            pressureConverter = pressureUnit.getConverterTo(PRESSURE_UNIT);
        }
        Number[] h2oArray = pdv.getNumberAllLevels(CrimssRecord.PDV_H2O);
        Number[] pressureArray = pdv.getNumberAllLevels(CrimssRecord.PDV_P_H2O);

        for (int j = 0; j < h2oArray.length; j++) {
            float pressure = pressureArray[j].floatValue();
            pressure = (float) pressureConverter.convert(pressure);
            float h2o = h2oArray[j].floatValue();
            h2o = (float) h2oConverter.convert(h2o);
            float dpt = convertH2OtoDewpoint(h2o, pressure);
            dpt = (float) dewpointConverter.convert(dpt);
            NcSoundingLayer layer = new NcSoundingLayer(pressure,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    (float) dpt, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                    NcSoundingLayer.MISSING, NcSoundingLayer.MISSING, h2o,
                    NcSoundingLayer.MISSING);
            layers.add(layer);
        }
        return layers;
    }

    // convert h2o in g/kg and pressure in hPa to dewpoint in kelvin.
    private static float convertH2OtoDewpoint(float h2o, float pressure) {
        double eee = pressure * h2o / (622.0 + 0.378 * h2o);
        double b = 26.66082 - Math.log(eee);
        return (float) ((b - Math.sqrt(b * b - 223.1986)) / 0.0182758048);
    }

    private static void mergeDuplicates(List<NcSoundingLayer> layers) {
        // Merge any soundings at same pressure.
        Iterator<NcSoundingLayer> iter = layers.iterator();
        NcSoundingLayer prev = iter.next();
        while (iter.hasNext()) {
            NcSoundingLayer next = iter.next();
            if (prev.getPressure() == next.getPressure()) {
                if (next.getGeoHeight() > 0) {
                    prev.setGeoHeight(next.getGeoHeight());
                }
                if (next.getDewpoint() > -300) {
                    prev.setDewpoint(next.getDewpoint());
                }
                if (next.getTemperature() > -300) {
                    prev.setTemperature(next.getTemperature());
                }
                iter.remove();
            } else {
                prev = next;
            }
        }
    }

    private static void interpolateHeight(List<NcSoundingLayer> layers) {
        int belowIndex = -1;
        for (int i = 0; i < layers.size(); i++) {
            NcSoundingLayer layerAbove = layers.get(i);
            if (layerAbove.getGeoHeight() < 0) {
                continue;
            }
            if (belowIndex == -1) {
                belowIndex = i;
                continue;
            }
            NcSoundingLayer layerBelow = layers.get(belowIndex);
            double diff = layerAbove.getGeoHeight() - layerBelow.getGeoHeight();
            double pchg1 = Math.log(layerBelow.getPressure()
                    / layerAbove.getPressure());
            for (int j = belowIndex + 1; j < i; j += 1) {
                NcSoundingLayer layer = layers.get(j);
                double pchg2 = Math.log(layerBelow.getPressure()
                        / layer.getPressure());
                layer.setGeoHeight((float) (layerBelow.getGeoHeight() + ((pchg2 / pchg1) * diff)));
            }
            belowIndex = i;
        }
    }

    private static void interpolateTemperature(List<NcSoundingLayer> layers) {
        int belowIndex = -1;
        for (int i = 0; i < layers.size(); i++) {
            NcSoundingLayer layerAbove = layers.get(i);
            if (layerAbove.getTemperature() < -300) {
                continue;
            }
            if (belowIndex == -1) {
                belowIndex = i;
                continue;
            }
            NcSoundingLayer layerBelow = layers.get(belowIndex);
            double diff = layerAbove.getTemperature()
                    - layerBelow.getTemperature();
            double pchg1 = Math.log(layerBelow.getPressure()
                    / layerAbove.getPressure());
            for (int j = belowIndex + 1; j < i; j += 1) {
                NcSoundingLayer layer = layers.get(j);
                double pchg2 = Math.log(layerBelow.getPressure()
                        / layer.getPressure());
                layer.setTemperature((float) (layerBelow.getTemperature() + ((pchg2 / pchg1) * diff)));
            }
            belowIndex = i;
        }
    }

    private static void interpolateDewpoint(List<NcSoundingLayer> layers) {
        int belowIndex = -1;
        for (int i = 0; i < layers.size(); i++) {
            NcSoundingLayer layerAbove = layers.get(i);
            if (layerAbove.getDewpoint() < -300) {
                continue;
            }
            if (belowIndex == -1) {
                belowIndex = i;
                continue;
            }
            NcSoundingLayer layerBelow = layers.get(belowIndex);
            double diff = layerAbove.getDewpoint() - layerBelow.getDewpoint();
            double pchg1 = Math.log(layerBelow.getPressure()
                    / layerAbove.getPressure());
            for (int j = belowIndex + 1; j < i; j += 1) {
                NcSoundingLayer layer = layers.get(j);
                double pchg2 = Math.log(layerBelow.getPressure()
                        / layer.getPressure());
                layer.setDewpoint((float) (layerBelow.getDewpoint() + ((pchg2 / pchg1) * diff)));
            }
            belowIndex = i;
        }
    }

}
