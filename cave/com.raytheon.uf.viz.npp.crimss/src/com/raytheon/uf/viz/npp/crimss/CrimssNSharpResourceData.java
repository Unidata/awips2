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

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.npp.crimss.CrimssRecord;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.nsharp.SoundingLayerBuilder;
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
 * Dec 05, 2011            bsteffen    Initial creation
 * Aug 15, 2013 2260       bsteffen    Switch poessounding to NSharp.
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
        Unit<Pressure> surfacePressureUnit = pdv.getUnit(
                CrimssRecord.PDV_SURFACE_PRESSURE).asType(Pressure.class);
        SoundingLayerBuilder builder = new SoundingLayerBuilder();
        builder.addPressure(surfacePressure, surfacePressureUnit);
        return builder.toNcSoundingLayer();
    }

    private static List<NcSoundingLayer> getHeightLayers(PointDataView pdv) {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        Unit<Length> heightUnit = pdv.getUnit(CrimssRecord.PDV_ALTITUDE)
                .asType(Length.class);
        Unit<Pressure> pressureUnit = pdv.getUnit(CrimssRecord.PDV_P_ALTITUDE)
                .asType(Pressure.class);
        Number[] altitudeArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_ALTITUDE);
        Number[] pressureArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_P_ALTITUDE);
        for (int j = 0; j < altitudeArray.length; j++) {
            SoundingLayerBuilder builder = new SoundingLayerBuilder();
            builder.addPressure(pressureArray[j].doubleValue(), pressureUnit);
            builder.addHeight(altitudeArray[j].doubleValue(), heightUnit);
            layers.add(builder.toNcSoundingLayer());
        }
        return layers;
    }

    private static List<NcSoundingLayer> getTemperatureLayers(PointDataView pdv) {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        Unit<Temperature> temperatureUnit = pdv.getUnit(
                CrimssRecord.PDV_TEMPERATURE).asType(Temperature.class);
        Unit<Pressure> pressureUnit = pdv.getUnit(
                CrimssRecord.PDV_P_TEMPERATURE).asType(Pressure.class);
        Number[] temperatureArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_TEMPERATURE);
        Number[] pressureArray = pdv
                .getNumberAllLevels(CrimssRecord.PDV_P_TEMPERATURE);

        for (int j = 0; j < temperatureArray.length; j++) {
            SoundingLayerBuilder builder = new SoundingLayerBuilder();
            builder.addPressure(pressureArray[j].doubleValue(), pressureUnit);
            builder.addTemperature(temperatureArray[j].doubleValue(),
                    temperatureUnit);
            layers.add(builder.toNcSoundingLayer());
        }
        return layers;
    }

    private static List<NcSoundingLayer> getDewpointLayers(PointDataView pdv) {
        List<NcSoundingLayer> layers = new ArrayList<NcSoundingLayer>();
        Unit<Dimensionless> h2oUnit = pdv.getUnit(CrimssRecord.PDV_H2O).asType(
                Dimensionless.class);
        Unit<Pressure> pressureUnit = pdv.getUnit(CrimssRecord.PDV_P_H2O)
                .asType(Pressure.class);
        Number[] h2oArray = pdv.getNumberAllLevels(CrimssRecord.PDV_H2O);
        Number[] pressureArray = pdv.getNumberAllLevels(CrimssRecord.PDV_P_H2O);

        for (int j = 0; j < h2oArray.length; j++) {
            SoundingLayerBuilder builder = new SoundingLayerBuilder();
            builder.addPressure(pressureArray[j].doubleValue(), pressureUnit);
            builder.addSpecificHumidity(h2oArray[j].doubleValue(), h2oUnit);
            layers.add(builder.toNcSoundingLayer());
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
