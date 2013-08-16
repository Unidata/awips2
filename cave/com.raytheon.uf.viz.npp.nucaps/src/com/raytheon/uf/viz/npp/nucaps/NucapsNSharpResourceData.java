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

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.npp.nucaps.NucapsRecord;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.nsharp.SoundingLayerBuilder;
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
 * Aug 15, 2013 2260       bsteffen    Switch poessounding to NSharp.
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
        Unit<Pressure> pressureUnit = pdv.getUnit(NucapsRecord.PDV_PRESSURE)
                .asType(Pressure.class);
        Number[] temperatures = pdv
                .getNumberAllLevels(NucapsRecord.PDV_TEMPERATURE);
        Unit<Temperature> temperatureUnit = pdv.getUnit(
                NucapsRecord.PDV_TEMPERATURE).asType(Temperature.class);
        Number[] wvMixingRatios = pdv
                .getNumberAllLevels(NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO);
        Unit<Dimensionless> wvMixingRatioUnit = pdv.getUnit(
                NucapsRecord.PDV_WATER_VAPOR_MIXING_RATIO).asType(
                Dimensionless.class);

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
            double pressure = pressures[idx].doubleValue();
            if (pressure <= surfacePressure) {
                // Don't add entries where pressure indicates below ground
                SoundingLayerBuilder builder = new SoundingLayerBuilder();
                builder.addPressure(pressure, pressureUnit);
                builder.addTemperature(temperatures[idx].doubleValue(),
                        temperatureUnit);
                builder.addSpecificHumidity(wvMixingRatios[idx].doubleValue(),
                        wvMixingRatioUnit);
                soundingLayers.add(builder.toNcSoundingLayer());
            }
        }

        return soundingLayers;
    }

}
