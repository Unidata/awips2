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
package com.raytheon.uf.viz.npp.nucaps;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.edex.meteolibrary.Meteolibrary;
import com.raytheon.uf.common.dataplugin.npp.nucaps.NucapsRecord;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.npp.sounding.rsc.AbstractNPPNSharpResourceData;

/**
 * NSharp resource data capable of loading NUCAPS data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013            mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class NucapsNSharpResourceData extends AbstractNPPNSharpResourceData {

    private static final String PLUGIN = "nucaps";

    private static final String[] PARAMETERS = { NucapsRecord.LONGITUDE,
            NucapsRecord.LATITUDE, NucapsRecord.PDV_SURFACE_PRESSURE,
            NucapsRecord.PDV_PRESSURE, NucapsRecord.PDV_TEMPERATURE,
            NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO };

    public NucapsNSharpResourceData() {
        super("NUCAPS", PLUGIN, PARAMETERS);
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
        Number[] pressures = pdv.getNumberAllLevels(NucapsRecord.PDV_PRESSURE);
        UnitConverter pressureConverter = getConverter(
                pdv.getUnit(NucapsRecord.PDV_PRESSURE), PRESSURE_UNIT);
        Number[] temperatures = pdv
                .getNumberAllLevels(NucapsRecord.PDV_TEMPERATURE);
        UnitConverter temperatureConverter = getConverter(
                pdv.getUnit(NucapsRecord.PDV_TEMPERATURE), TEMPERATURE_UNIT);
        UnitConverter temperatureCalcConverter = getConverter(
                pdv.getUnit(NucapsRecord.PDV_TEMPERATURE),
                TEMPERATURE_CALC_UNIT);
        Number[] wvMixingRatios = pdv
                .getNumberAllLevels(NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO);
        UnitConverter wvMixingRatioConverter = getConverter(
                pdv.getUnit(NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO),
                H2O_UNIT);
        UnitConverter dewPointConverter = getConverter(SI.KELVIN, SI.CELSIUS);

        if (pressures.length != temperatures.length
                || pressures.length != wvMixingRatios.length) {
            throw new VizException("NUCAPS PointData sizes incorrect");
        }
        int length = pressures.length;

        List<NcSoundingLayer> soundingLayers = new ArrayList<NcSoundingLayer>(
                length);
        float surfacePressure = pdv.getFloat(NucapsRecord.PDV_SURFACE_PRESSURE);
        for (int i = 0; i < length; ++i) {
            int idx = i;
            float pressure = (float) pressureConverter.convert(pressures[idx]
                    .doubleValue());
            if (pressure <= surfacePressure) {
                // Don't add entries where pressure indicates below ground
                float gh = Meteolibrary.ptozsa(new float[] { pressure }, 0);
                Number temperature = temperatures[idx];
                float h20 = (float) wvMixingRatioConverter
                        .convert(wvMixingRatios[idx].doubleValue());
                float dewpoint = convertH2OtoDewpoint(h20, pressure);
                float rh = convertH20ToRelativeHumidity(h20,
                        (float) temperatureCalcConverter.convert(temperature
                                .doubleValue()), pressure);
                soundingLayers.add(new NcSoundingLayer(pressure, gh,
                        (float) temperatureConverter.convert(temperature
                                .doubleValue()), (float) dewPointConverter
                                .convert(dewpoint), NcSoundingLayer.MISSING,
                        NcSoundingLayer.MISSING, NcSoundingLayer.MISSING,
                        NcSoundingLayer.MISSING, NcSoundingLayer.MISSING, h20,
                        rh));
            }
        }

        return soundingLayers;
    }

    private UnitConverter getConverter(Unit<?> fromUnit, Unit<?> toUnit)
            throws VizException {
        if (fromUnit.isCompatible(toUnit)) {
            return fromUnit.getConverterTo(toUnit);
        }
        throw new VizException("Unable to convert " + fromUnit + " to "
                + toUnit);
    }

}
