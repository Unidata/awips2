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

import javax.measure.Quantity;
import javax.measure.Unit;
import javax.measure.quantity.Angle;
import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Speed;
import javax.measure.quantity.Temperature;

import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.wxmath.Dewpoint;
import com.raytheon.uf.common.wxmath.PToZsa;
import com.raytheon.uf.common.wxmath.RelativeHumidity;
import com.raytheon.uf.common.wxmath.ZToPsa;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.quantity.Quantities;
import tec.uom.se.unit.MetricPrefix;
import tec.uom.se.unit.Units;

/**
 * Builder for conveniently making NcSoundingLayers with the correct units.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 14, 2013  2260     bsteffen    Initial creation
 * Aug 27, 2013  2190     mschenke    Fixed unit for VerticalSounding creation
 * May 04, 2015  4444     bsteffen    Fix typo in addRelativeHumidity
 * May 11, 2015  4445     bsteffen    Add another way to calculate dewpoint
 * Apr 15, 2019  7596     lsingh      Upgraded units framework to JSR-363.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class SoundingLayerBuilder {

    private static final Unit<Pressure> PRESSURE_UNIT = MetricPrefix.HECTO(SI.PASCAL);

    private static final Unit<Length> HEIGHT_UNIT = SI.METRE;

    private static final Unit<Temperature> TEMPERATURE_UNIT = SI.KELVIN;

    private static final Unit<Temperature> DEWPOINT_UNIT = SI.KELVIN;

    private static final Unit<Angle> WIND_DIRECTION_UNIT = NonSI.DEGREE_ANGLE;

    private static final Unit<Speed> WIND_SPEED_UNIT = SI.METRE_PER_SECOND;

    private static final Unit<Pressure> NC_PRESSURE_UNIT = MetricPrefix.HECTO(SI.PASCAL);

    private static final Unit<Length> NC_HEIGHT_UNIT = SI.METRE;

    private static final Unit<Temperature> NC_TEMPERATURE_UNIT = SI.CELSIUS;

    private static final Unit<Temperature> NC_DEWPOINT_UNIT = SI.CELSIUS;

    private static final Unit<Angle> NC_WIND_DIRECTION_UNIT = NonSI.DEGREE_ANGLE;

    private static final Unit<Speed> NC_WIND_SPEED_UNIT = USCustomary.KNOT;

    private static final Unit<Dimensionless> NC_RELATIVE_HUMIDITY_UNIT = Units.PERCENT;

    private static final Unit<Dimensionless> NC_SPECIFIC_HUMIDITY_UNIT = SI.KILOGRAM
            .divide(SI.KILOGRAM).asType(Dimensionless.class);

    private Quantity<Pressure> pressure;

    private Quantity<Length> height;

    private Quantity<Temperature> temperature;

    private Quantity<Temperature> dewpoint;

    private Quantity<Angle> windDirection;

    private Quantity<Speed> windSpeed;

    private Quantity<Dimensionless> relativeHumidity;

    private Quantity<Dimensionless> specificHumidity;

    public SoundingLayerBuilder addPressure(Quantity<Pressure> pressure) {
        this.pressure = pressure;
        return this;
    }

    public SoundingLayerBuilder addPressure(double pressure, Unit<Pressure> unit) {
        this.pressure = Quantities.getQuantity(pressure, unit);
        return this;
    }

    public SoundingLayerBuilder addPressure(float pressure, Unit<Pressure> unit) {
        this.pressure = Quantities.getQuantity(pressure, unit);
        return this;
    }

    public SoundingLayerBuilder addHeight(Quantity<Length> height) {
        this.height = height;
        return this;
    }

    public SoundingLayerBuilder addHeight(double height, Unit<Length> unit) {
        this.height = Quantities.getQuantity(height, unit);
        return this;
    }

    public SoundingLayerBuilder addTemperature(
            Quantity<Temperature> temperature) {
        this.temperature = temperature;
        return this;
    }

    public SoundingLayerBuilder addTemperature(double temperature,
            Unit<Temperature> unit) {
        this.temperature = Quantities.getQuantity(temperature, unit);
        return this;
    }

    public SoundingLayerBuilder addDewpoint(Quantity<Temperature> dewpoint) {
        this.dewpoint = dewpoint;
        return this;
    }

    public SoundingLayerBuilder addDewpoint(double dewpoint,
            Unit<Temperature> unit) {
        this.dewpoint = Quantities.getQuantity(dewpoint, unit);
        return this;
    }

    public SoundingLayerBuilder addWindDirection(Quantity<Angle> windDirection) {
        this.windDirection = windDirection;
        return this;
    }

    public SoundingLayerBuilder addWindDirection(double windDirection,
            Unit<Angle> unit) {
        this.windDirection = Quantities.getQuantity(windDirection, unit);
        return this;
    }

    public SoundingLayerBuilder addWindSpeed(Quantity<Speed> windSpeed) {
        this.windSpeed = windSpeed;
        return this;
    }

    public SoundingLayerBuilder addWindSpeed(double windSpeed,
            Unit<Speed> unit) {
        this.windSpeed = Quantities.getQuantity(windSpeed, unit);
        return this;
    }

    public SoundingLayerBuilder addSpecificHumidity(
            Quantity<Dimensionless> specificHumidity) {
        this.specificHumidity = specificHumidity;
        return this;
    }

    public SoundingLayerBuilder addSpecificHumidity(double specificHumidity,
            Unit<Dimensionless> unit) {
        this.specificHumidity = Quantities.getQuantity(specificHumidity, unit);
        return this;
    }

    public SoundingLayerBuilder addRelativeHumidity(
            Quantity<Dimensionless> relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
        return this;
    }

    public SoundingLayerBuilder addRelativeHumidity(double relativeHumidity,
            Unit<Dimensionless> unit) {
        this.relativeHumidity = Quantities.getQuantity(relativeHumidity, unit);
        return this;
    }

    public NcSoundingLayer toNcSoundingLayer() {
        NcSoundingLayer layer = new NcSoundingLayer();
        if (pressure != null) {
            layer.setPressure(pressure.to(NC_PRESSURE_UNIT).getValue().floatValue());
        } else if (height != null) {
            layer.setPressure(ZToPsa.ztopsa(height.to(SI.METRE).getValue().floatValue()));
        }
        if (height != null) {
            layer.setGeoHeight(height.to(NC_HEIGHT_UNIT).getValue().floatValue());
        } else if (pressure != null) {
            layer.setGeoHeight(PToZsa.ptozsa(pressure.to(MetricPrefix.HECTO(SI.PASCAL)).getValue().floatValue()));
        }
        if (temperature != null) {
            layer.setTemperature(temperature.to(NC_TEMPERATURE_UNIT).getValue().floatValue());
        }
        if (dewpoint != null) {
            layer.setDewpoint(dewpoint.to(NC_DEWPOINT_UNIT).getValue().floatValue());
        } else if (specificHumidity != null && pressure != null) {
            Quantity<Temperature> dewpoint = Dewpoint.calculateFromPandSH(
                    pressure, specificHumidity);
            layer.setDewpoint(dewpoint.to(NC_DEWPOINT_UNIT).getValue().floatValue());
        } else if (temperature != null && relativeHumidity != null) {
            Quantity<Temperature> dewpoint = Dewpoint.calculateFromTandRH(
                    temperature, relativeHumidity);
            layer.setDewpoint(dewpoint.to(NC_DEWPOINT_UNIT).getValue().floatValue());
        }
        if (windDirection != null) {
            layer.setWindDirection(windDirection.to(NC_WIND_DIRECTION_UNIT).getValue()
                    .floatValue());
        }
        if (windSpeed != null) {
            layer.setWindSpeed(windSpeed.to(NC_WIND_SPEED_UNIT).getValue().floatValue());
        }
        if (relativeHumidity != null) {
            layer.setRelativeHumidity(relativeHumidity.to(NC_RELATIVE_HUMIDITY_UNIT).getValue()
                    .floatValue());
        } else if (specificHumidity != null && temperature != null
                && pressure != null) {
            Quantity<Dimensionless> relativeHumidity = RelativeHumidity
                    .calculate(pressure, temperature, specificHumidity);
            layer.setRelativeHumidity(relativeHumidity.to(NC_RELATIVE_HUMIDITY_UNIT).getValue()
                    .floatValue());
        }
        if (specificHumidity != null) {
            layer.setSpecHumidity(specificHumidity.to(NC_SPECIFIC_HUMIDITY_UNIT).getValue()
                    .floatValue());
        }
        return layer;
    }

    public SoundingLayer toSoundingLayer() {
        SoundingLayer layer = new SoundingLayer();
        if (pressure != null) {
            layer.setPressure(pressure.to(PRESSURE_UNIT).getValue().floatValue());
        } else if (height != null) {
            layer.setPressure(ZToPsa.ztopsa(height.to(SI.METRE).getValue().floatValue()));
        }
        if (height != null) {
            layer.setGeoHeight(height.to(HEIGHT_UNIT).getValue().floatValue());
        } else if (pressure != null) {
            layer.setGeoHeight(PToZsa.ptozsa(pressure.to(MetricPrefix.HECTO(SI.PASCAL)).getValue()
                    .floatValue()));
        }
        if (temperature != null) {
            layer.setTemperature(temperature.to(TEMPERATURE_UNIT).getValue().floatValue());
        }
        if (dewpoint != null) {
            layer.setDewpoint(dewpoint.to(DEWPOINT_UNIT).getValue().floatValue());
        } else if (specificHumidity != null && pressure != null) {
            Quantity<Temperature> dewpoint = Dewpoint.calculateFromPandSH(
                    pressure, specificHumidity);
            layer.setDewpoint(dewpoint.to(DEWPOINT_UNIT).getValue().floatValue());
        } else if (temperature != null && relativeHumidity != null) {
            Quantity<Temperature> dewpoint = Dewpoint.calculateFromTandRH(
                    temperature, relativeHumidity);
            layer.setDewpoint(dewpoint.to(NC_DEWPOINT_UNIT).getValue().floatValue());
        }
        if (windDirection != null) {
            layer.setWindDirection(windDirection.to(WIND_DIRECTION_UNIT).getValue()
                    .floatValue());
        }
        if (windSpeed != null) {
            layer.setWindSpeed(windSpeed.to(WIND_SPEED_UNIT).getValue().floatValue());
        }
        return layer;
    }

}
