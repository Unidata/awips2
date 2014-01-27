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
package com.raytheon.uf.viz.d2d.nsharp;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import javax.measure.Measure;
import javax.measure.quantity.Angle;
import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.wxmath.Dewpoint;
import com.raytheon.uf.common.wxmath.PToZsa;
import com.raytheon.uf.common.wxmath.RelativeHumidity;
import com.raytheon.uf.common.wxmath.ZToPsa;

/**
 * Builder for conveniently making NcSoundingLayers with the correct units.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2013 2260       bsteffen    Initial creation
 * Aug 27, 2013 2190       mschenke    Fixed unit for VerticalSounding creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class SoundingLayerBuilder {

    private static final Unit<Pressure> PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    private static final Unit<Length> HEIGHT_UNIT = SI.METER;

    private static final Unit<Temperature> TEMPERATURE_UNIT = SI.KELVIN;

    private static final Unit<Temperature> DEWPOINT_UNIT = SI.KELVIN;

    private static final Unit<Angle> WIND_DIRECTION_UNIT = NonSI.DEGREE_ANGLE;

    private static final Unit<Velocity> WIND_SPEED_UNIT = SI.METERS_PER_SECOND;

    private static final Unit<Pressure> NC_PRESSURE_UNIT = SI.HECTO(SI.PASCAL);

    private static final Unit<Length> NC_HEIGHT_UNIT = SI.METER;

    private static final Unit<Temperature> NC_TEMPERATURE_UNIT = SI.CELSIUS;

    private static final Unit<Temperature> NC_DEWPOINT_UNIT = SI.CELSIUS;

    private static final Unit<Angle> NC_WIND_DIRECTION_UNIT = NonSI.DEGREE_ANGLE;

    private static final Unit<Velocity> NC_WIND_SPEED_UNIT = NonSI.KNOT;

    private static final Unit<Dimensionless> NC_RELATIVE_HUMIDITY_UNIT = NonSI.PERCENT;

    private static final Unit<Dimensionless> NC_SPECIFIC_HUMIDITY_UNIT = SI.KILOGRAM
            .divide(SI.KILOGRAM).asType(Dimensionless.class);

    private Measure<?, Pressure> pressure;

    private Measure<?, Length> height;

    private Measure<?, Temperature> temperature;

    private Measure<?, Temperature> dewpoint;

    private Measure<?, Angle> windDirection;

    private Measure<?, Velocity> windSpeed;

    private Measure<?, Dimensionless> relativeHumidity;

    private Measure<?, Dimensionless> specificHumidity;

    public SoundingLayerBuilder addPressure(Measure<?, Pressure> pressure) {
        this.pressure = pressure;
        return this;
    }

    public SoundingLayerBuilder addPressure(double pressure, Unit<Pressure> unit) {
        this.pressure = Measure.valueOf(pressure, unit);
        return this;
    }

    public SoundingLayerBuilder addPressure(float pressure, Unit<Pressure> unit) {
        this.pressure = Measure.valueOf(pressure, unit);
        return this;
    }

    public SoundingLayerBuilder addHeight(Measure<?, Length> height) {
        this.height = height;
        return this;
    }

    public SoundingLayerBuilder addHeight(double height, Unit<Length> unit) {
        this.height = Measure.valueOf(height, unit);
        return this;
    }

    public SoundingLayerBuilder addTemperature(
            Measure<?, Temperature> temperature) {
        this.temperature = temperature;
        return this;
    }

    public SoundingLayerBuilder addTemperature(double temperature,
            Unit<Temperature> unit) {
        this.temperature = Measure.valueOf(temperature, unit);
        return this;
    }

    public SoundingLayerBuilder addDewpoint(Measure<?, Temperature> dewpoint) {
        this.dewpoint = dewpoint;
        return this;
    }

    public SoundingLayerBuilder addDewpoint(double dewpoint,
            Unit<Temperature> unit) {
        this.dewpoint = Measure.valueOf(dewpoint, unit);
        return this;
    }

    public SoundingLayerBuilder addWindDirection(Measure<?, Angle> windDirection) {
        this.windDirection = windDirection;
        return this;
    }

    public SoundingLayerBuilder addWindDirection(double windDirection,
            Unit<Angle> unit) {
        this.windDirection = Measure.valueOf(windDirection, unit);
        return this;
    }

    public SoundingLayerBuilder addWindSpeed(Measure<?, Velocity> windSpeed) {
        this.windSpeed = windSpeed;
        return this;
    }

    public SoundingLayerBuilder addWindSpeed(double windSpeed,
            Unit<Velocity> unit) {
        this.windSpeed = Measure.valueOf(windSpeed, unit);
        return this;
    }

    public SoundingLayerBuilder addSpecificHumidity(
            Measure<?, Dimensionless> specificHumidity) {
        this.specificHumidity = specificHumidity;
        return this;
    }

    public SoundingLayerBuilder addSpecificHumidity(double specificHumidity,
            Unit<Dimensionless> unit) {
        this.specificHumidity = Measure.valueOf(specificHumidity, unit);
        return this;
    }

    public SoundingLayerBuilder addRelativeHumidity(
            Measure<?, Dimensionless> relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
        return this;
    }

    public SoundingLayerBuilder addRelativeHumidity(double relativeHumidity,
            Unit<Dimensionless> unit) {
        this.specificHumidity = Measure.valueOf(relativeHumidity, unit);
        return this;
    }

    public NcSoundingLayer toNcSoundingLayer() {
        NcSoundingLayer layer = new NcSoundingLayer();
        if (pressure != null) {
            layer.setPressure(pressure.floatValue(NC_PRESSURE_UNIT));
        } else if (height != null) {
            layer.setPressure(ZToPsa.ztopsa(height.floatValue(SI.METER)));
        }
        if (height != null) {
            layer.setGeoHeight(height.floatValue(NC_HEIGHT_UNIT));
        } else if (pressure != null) {
            layer.setGeoHeight(PToZsa.ptozsa(pressure.floatValue(SI
                    .HECTO(SI.PASCAL))));
        }
        if (temperature != null) {
            layer.setTemperature(temperature.floatValue(NC_TEMPERATURE_UNIT));
        }
        if (dewpoint != null) {
            layer.setDewpoint(dewpoint.floatValue(NC_DEWPOINT_UNIT));
        } else if (specificHumidity != null && pressure != null) {
            Measure<?, Temperature> dewpoint = Dewpoint.calculate(pressure,
                    specificHumidity);
            layer.setDewpoint(dewpoint.floatValue(NC_DEWPOINT_UNIT));
        }
        if (windDirection != null) {
            layer.setWindDirection(windDirection
                    .floatValue(NC_WIND_DIRECTION_UNIT));
        }
        if (windSpeed != null) {
            layer.setWindSpeed(windSpeed.floatValue(NC_WIND_SPEED_UNIT));
        }
        if (relativeHumidity != null) {
            layer.setRelativeHumidity(relativeHumidity
                    .floatValue(NC_RELATIVE_HUMIDITY_UNIT));
        } else if (specificHumidity != null && temperature != null
                && pressure != null) {
            Measure<?, Dimensionless> relativeHumidity = RelativeHumidity
                    .calculate(pressure, temperature, specificHumidity);
            layer.setRelativeHumidity(relativeHumidity
                    .floatValue(NC_RELATIVE_HUMIDITY_UNIT));
        }
        if (specificHumidity != null) {
            layer.setSpecHumidity(specificHumidity
                    .floatValue(NC_SPECIFIC_HUMIDITY_UNIT));
        }
        return layer;
    }

    public SoundingLayer toSoundingLayer() {
        SoundingLayer layer = new SoundingLayer();
        if (pressure != null) {
            layer.setPressure(pressure.floatValue(PRESSURE_UNIT));
        } else if (height != null) {
            layer.setPressure(ZToPsa.ztopsa(height.floatValue(SI.METER)));
        }
        if (height != null) {
            layer.setGeoHeight(height.floatValue(HEIGHT_UNIT));
        } else if (pressure != null) {
            layer.setGeoHeight(PToZsa.ptozsa(pressure.floatValue(SI
                    .HECTO(SI.PASCAL))));
        }
        if (temperature != null) {
            layer.setTemperature(temperature.floatValue(TEMPERATURE_UNIT));
        }
        if (dewpoint != null) {
            layer.setDewpoint(dewpoint.floatValue(DEWPOINT_UNIT));
        } else if (specificHumidity != null && pressure != null) {
            Measure<?, Temperature> dewpoint = Dewpoint.calculate(pressure,
                    specificHumidity);
            layer.setDewpoint(dewpoint.floatValue(DEWPOINT_UNIT));
        }
        if (windDirection != null) {
            layer.setWindDirection(windDirection
                    .floatValue(WIND_DIRECTION_UNIT));
        }
        if (windSpeed != null) {
            layer.setWindSpeed(windSpeed.floatValue(WIND_SPEED_UNIT));
        }
        return layer;
    }

}
