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
package com.raytheon.uf.viz.satellite.goesr.legacyprofile;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;

import java.lang.reflect.Array;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Temperature;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.d2d.nsharp.SoundingLayerBuilder;

/**
 * Common code to build {@link VerticalSounding} or a {@link NcSoundingProfile}
 * from satellite records which contain GOESR Legacy Moisture/Temperature
 * profile information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 30, 2015  4335     bsteffen    Initial creation
 * May 13, 2015  4445     bsteffen    Remove GOESR sounding layer builder.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GoesrProfileBuilder {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GoesrLegacySoundingProvider.class);

    private final Pattern physicalElementPattern = Pattern
            .compile("^V([TM])P-(\\d{1,4}\\.\\d{2})hPa$");

    private NavigableMap<Double, SoundingLayerBuilder> map = new TreeMap<>();

    public boolean isEmpty() {
        return map.isEmpty();
    }

    public void addRecord(SatelliteRecord record, IDataRecord dataRecord,
            int index) {
        Matcher paramMatcher = physicalElementPattern.matcher(record
                .getPhysicalElement());
        if (!paramMatcher.matches()) {
            statusHandler.warn("Cannot add " + record + " to the sounding");
            return;
        }
        double pressure = Double.parseDouble(paramMatcher.group(2));
        Unit<?> unit;
        try {
            unit = getDataUnit(record, dataRecord.getDataAttributes());
        } catch (ParseException e) {
            statusHandler.error("Cannot add " + record + " to the sounding", e);
            return;
        }
        int value = (Array.getInt(dataRecord.getDataObject(), index));
        if (value == dataRecord.getFillValue().doubleValue()) {
            return;
        } else {
            value = value & 0xFFFF;
        }
        SoundingLayerBuilder layer = map.get(pressure);
        if (layer == null) {
            layer = new SoundingLayerBuilder();
            layer.addPressure(pressure, SI.HECTO(SI.PASCAL));
            map.put(pressure, layer);
        }
        if ("T".equals(paramMatcher.group(1))) {
            layer.addTemperature(value, unit.asType(Temperature.class));
        } else {
            layer.addRelativeHumidity(value, unit.asType(Dimensionless.class));
        }
    }

    public VerticalSounding toVerticalSounding() {
        VerticalSounding sounding = new VerticalSounding();
        for (SoundingLayerBuilder builder : map.values()) {
            sounding.addLayer(builder.toSoundingLayer());
        }
        Collections.reverse(sounding.getLayerData());
        return sounding;
    }

    public NcSoundingProfile toNcSoundingProfile() {
        List<NcSoundingLayer> ncLayers = new ArrayList<>(map.size());
        for (SoundingLayerBuilder builder : map.descendingMap().values()) {
            ncLayers.add(builder.toNcSoundingLayer());
        }
        NcSoundingProfile profile = new NcSoundingProfile();
        profile.setSoundingLyLst(ncLayers);
        return profile;
    }

    private static Unit<?> getDataUnit(SatelliteRecord record,
            Map<String, Object> dataAttributes) throws ParseException {

        Unit<?> recordUnit = UnitFormat.getUCUMInstance().parseProductUnit(
                record.getUnits(), new ParsePosition(0));
        Unit<?> units = recordUnit != null ? recordUnit : Unit.ONE;
        if (dataAttributes == null) {
            return units;
        }
        Number offset = (Number) dataAttributes
                .get(SatelliteRecord.SAT_ADD_OFFSET);
        Number scale = (Number) dataAttributes
                .get(SatelliteRecord.SAT_SCALE_FACTOR);

        if (offset != null) {
            double offsetVal = offset.doubleValue();
            if (offsetVal != 0.0) {
                units = units.plus(offsetVal);
            }
        }
        if (scale != null) {
            double scaleVal = scale.doubleValue();
            if (scaleVal != 0.0) {
                units = units.times(scaleVal);
            }
        }
        return units;
    }

}
