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
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.nsharp.SoundingLayerBuilder;
import com.raytheon.viz.pointdata.PointDataRequest;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube.QueryStatus;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;

/**
 * Provides sounding data to nsharp from poes sounding satellite data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2013 2260       bsteffen    Initial creation
 * Jul 23, 2014 3410       bclement     preparePointInfo() calls unpackResultLocation()
 * Mar 28, 2018 6800       bsteffen     Extend PointDataNSharpResourceData.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PoesSndNSharpResourceData extends PointDataNSharpResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PoesSndNSharpResourceData.class);

    private static final String NUM_LEVELS = "numLevels";

    private static final String PRESSURE = "pressure";

    private static final String TEMPERATURE = "temperature";

    private static final String SPEC_HUM = "mixingRatio";

    private static final String[] PARAMETERS = { NUM_LEVELS, PRESSURE, SPEC_HUM,
            TEMPERATURE };

    public PoesSndNSharpResourceData() {
        super("POES");
    }

    @Override
    protected NcSoundingCube getSoundingCube(NsharpStationInfo stnInfo) {
        DataTime time = new DataTime(stnInfo.getReftime());
        try {
            PointDataContainer pdc = PointDataRequest.requestPointDataAllLevels(
                    time, "poes", PARAMETERS, null, getMetadataMap());
            PointDataView pdv = pdc.readRandom(0);
            int numLevels = pdv.getInt(NUM_LEVELS);
            Number[] pressure = pdv.getNumberAllLevels(PRESSURE);
            Unit<Pressure> pressureUnit = pdv.getUnit(PRESSURE)
                    .asType(Pressure.class);
            Number[] temperature = pdv.getNumberAllLevels(TEMPERATURE);
            Unit<Temperature> temperatureUnit = pdv.getUnit(TEMPERATURE)
                    .asType(Temperature.class);
            Number[] specHumidity = pdv.getNumberAllLevels(SPEC_HUM);
            Unit<Dimensionless> specHumidityUnit = pdv.getUnit(SPEC_HUM)
                    .asType(Dimensionless.class);

            List<NcSoundingLayer> layers = new ArrayList<>(numLevels);
            for (int i = 0; i < numLevels; i += 1) {
                SoundingLayerBuilder builder = new SoundingLayerBuilder();
                builder.addPressure(pressure[i].doubleValue(), pressureUnit);
                builder.addTemperature(temperature[i].doubleValue(),
                        temperatureUnit);
                builder.addSpecificHumidity(specHumidity[i].doubleValue(),
                        specHumidityUnit);
                layers.add(builder.toNcSoundingLayer());
            }

            NcSoundingProfile profile = new NcSoundingProfile();
            profile.setSoundingLyLst(layers);
            NcSoundingCube cube = new NcSoundingCube(Arrays.asList(profile));
            cube.setRtnStatus(QueryStatus.OK);
            return cube;

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return null;
    }

    @Override
    protected String getPointNameKey() {
        return null;
    }

}
