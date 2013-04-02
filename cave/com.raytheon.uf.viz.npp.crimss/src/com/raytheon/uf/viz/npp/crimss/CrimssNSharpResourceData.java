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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.npp.crimss.CrimssRecord;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.npp.sounding.rsc.AbstractNPPNSharpResourceData;

/**
 * NSharp resource data capable of loading CrIMSS data
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
public class CrimssNSharpResourceData extends AbstractNPPNSharpResourceData {

    private static final String PLUGIN = "crimss";

    private static final String[] PARAMETERS = { CrimssRecord.LONGITUDE,
            CrimssRecord.LATITUDE, CrimssRecord.PDV_SURFACE_PRESSURE,
            CrimssRecord.PDV_ALTITUDE, CrimssRecord.PDV_P_ALTITUDE,
            CrimssRecord.PDV_H2O, CrimssRecord.PDV_P_H2O,
            CrimssRecord.PDV_TEMPERATURE, CrimssRecord.PDV_P_TEMPERATURE };

    public CrimssNSharpResourceData() {
        super("CrIMSS", PLUGIN, PARAMETERS);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.npp.sounding.rsc.AbstractNPPNSharpResourceData#
     * getSoundingLayers(com.raytheon.uf.common.pointdata.PointDataView)
     */
    @Override
    protected List<NcSoundingLayer> getSoundingLayers(PointDataView pdv)
            throws VizException {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        layers.add(getSurfacePressureLayer(pdv));
        layers.addAll(getHeightLayers(pdv));
        layers.addAll(getTemperatureLayers(pdv));
        layers.addAll(getDewpointLayers(pdv));
        Collections.sort(layers,
                NsharpDataHandling.reversePressureHeightWindComparator());
        mergeDuplicates(layers);
        return layers;
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

}
