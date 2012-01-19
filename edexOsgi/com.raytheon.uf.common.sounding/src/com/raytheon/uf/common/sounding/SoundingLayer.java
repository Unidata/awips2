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
package com.raytheon.uf.common.sounding;

import java.io.Serializable;
import java.util.Comparator;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * SoundingLayer contains the data for a single layer of upper air data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * 20071127            382 jkorman     Moved from Cave graphing.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SoundingLayer implements Serializable, Cloneable {
    private static UnitConverter KtoC = SI.KELVIN.getConverterTo(SI.CELSIUS);

    private static UnitConverter CtoK = SI.CELSIUS.getConverterTo(SI.KELVIN);

    public enum DATA_TYPE {
        PRESSURE("UA_PRESSURE", SI.HECTO(SI.PASCAL)), TEMPERATURE(
                "UA_TEMPERATURE", SI.KELVIN), DEWPOINT("UA_DEWPOINT", SI.KELVIN), WINDSPEED(
                "WINDSPEED", SI.METERS_PER_SECOND), WINDDIRECTION(
                "WINDDIRECTION", NonSI.DEGREE_ANGLE), GEO_HEIGHT("GEO_HEIGHT",
                SI.METER), OMEGA("OMEGA", SI.HECTO(SI.PASCAL).divide(SI.SECOND)), WIND_U(
                "WIND_U", SI.METERS_PER_SECOND), WIND_V("WIND_V",
                SI.METERS_PER_SECOND);

        private final String dataName;

        private final Unit<?> unit;

        private DATA_TYPE(String name, Unit<?> unit) {
            dataName = name;
            this.unit = unit;
        }

        public String getDataName() {
            return dataName;
        }

        public Unit<?> getUnit() {
            return unit;
        }
    }

    private static final long serialVersionUID = 1324632465L;

    public static final float MISSING = 99999f;

    public static final float NODATA = 99998f;

    private LayerType layerType = LayerType.GENERIC;

    // Pressure of the layer, in hectoPascals (milliBars), if known.
    private float pressure;

    // Geopotential height of the layer, in meters, if known.
    private float geoHeight;

    // Temperature of the layer, in Kelvin.
    private float temperature;

    // Dewpoint of the layer, in Kelvin.
    private float dewpoint;

    // Wind speed, in meters per second.
    private float windSpeed;

    // Wind direction, in degrees.
    private float windDirection;

    // U component of wind speed, in meters per second.
    private float windU;

    // V component of wind speed, in meters per second.
    private float windV;

    // Vertical velocity, in millibars/sec??
    private float omega;

    private boolean preInterpolated = false;

    private boolean hgtInterpolated = false;

    private boolean tmpInterpolated = false;

    private boolean dptInterpolated = false;

    private boolean spdInterpolated = false;

    private boolean dirInterpolated = false;

    public SoundingLayer() {
        layerType = LayerType.GENERIC;
        pressure = MISSING;
        geoHeight = MISSING;
        temperature = MISSING;
        dewpoint = MISSING;
        windSpeed = MISSING;
        windDirection = MISSING;
        windU = MISSING;
        windV = MISSING;
        omega = MISSING;
    }

    /**
     * Construct a sounding layer. With wind speed and direction.
     * 
     * @param pressure
     *            The layer pressure in hectoPascals (milliBars)
     * @param geoHeight
     *            The geopotential height in meters.
     * @param temperature
     *            The air temperature in Kelvin.
     * @param dewpoint
     *            The dewpoint temperature in Kelvin.
     * @param windSpeed
     *            The wind speed in m/s.
     * @param windDirection
     *            The wind direction in angular degrees.
     * 
     * @param omega
     *            The vertical velocity in millibars/second
     */
    public SoundingLayer(float pressure, float geoHeight, float temperature,
            float dewpoint, float windSpeed, float windDirection, float omega) {
        layerType = LayerType.GENERIC;
        this.pressure = pressure;
        this.geoHeight = geoHeight;
        this.temperature = temperature;
        this.dewpoint = dewpoint;
        this.windSpeed = windSpeed;
        this.windDirection = windDirection;
        this.omega = omega;
    }

    /**
     * Get the layer type.
     * 
     * @return The layer type.
     */
    public LayerType getLayerType() {
        return layerType;
    }

    /**
     * Set the layer type.
     * 
     * @param type
     *            The layer type.
     */
    public void setLayerType(LayerType type) {
        layerType = type;
    }

    /**
     * @return the pressure in hectoPascals (milliBars)
     */
    public float getPressure() {
        return pressure;
    }

    /**
     * @param pressure
     *            the pressure in hectoPascals (milliBars)
     */
    public void setPressure(float pressure) {
        this.pressure = pressure;
    }

    /**
     * @return the geoHeight in meters
     */
    public float getGeoHeight() {
        return geoHeight;
    }

    /**
     * @param geoHeight
     *            the geoHeight in meters
     */
    public void setGeoHeight(float geoHeight) {
        this.geoHeight = geoHeight;
    }

    /**
     * @return the temperature in Kelvin
     */
    public float getTemperature() {
        return temperature;
    }

    /**
     * @param temperature
     *            the temperature in Kelvin
     */
    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    /**
     * @return the dewpoint in Kelvin
     */
    public float getDewpoint() {
        return dewpoint;
    }

    /**
     * @param dewpoint
     *            the dewpoint in Kelvin
     */
    public void setDewpoint(float dewpoint) {
        this.dewpoint = dewpoint;
    }

    /**
     * @return the windSpeed in meters per second
     */
    public float getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed
     *            the windSpeed in meters per second
     */
    public void setWindSpeed(float windSpeed) {
        this.windSpeed = windSpeed;
        computeUV();
    }

    /**
     * @return the windDirection in angular degrees
     */
    public float getWindDirection() {
        return windDirection;
    }

    /**
     * @param windDirection
     *            the windDirection in angular degrees
     */
    public void setWindDirection(float windDirection) {
        this.windDirection = windDirection;
        computeUV();
    }

    /**
     * @return the U component of windSpeed in meters per second
     */
    public float getWindU() {
        return windU;
    }

    /**
     * @param windU
     *            the U component windSpeed in meters per second
     */
    public void setWindU(float windU) {
        this.windU = windU;
        computeSpdDir();
    }

    /**
     * @return the V component of windSpeed in meters per second
     */
    public float getWindV() {
        return windV;
    }

    /**
     * @param windV
     *            the V component windSpeed in meters per second
     */
    public void setWindV(float windV) {
        this.windV = windV;
        computeSpdDir();
    }

    /**
     * @return the vertical velocity in millibars/sec??
     */
    public float getOmega() {
        return omega;
    }

    /**
     * @param omega
     *            the vertical velocity in millibars/sec??
     */
    public void setOmega(float omega) {
        this.omega = omega;
    }

    /**
     * @return the preInterpolated
     */
    public boolean isPreInterpolated() {
        return preInterpolated;
    }

    /**
     * @param preInterpolated
     *            the preInterpolated to set
     */
    public void setPreInterpolated(boolean preInterpolated) {
        this.preInterpolated = preInterpolated;
    }

    /**
     * @return the hgtInterpolated
     */
    public boolean isHgtInterpolated() {
        return hgtInterpolated;
    }

    /**
     * @param hgtInterpolated
     *            the hgtInterpolated to set
     */
    public void setHgtInterpolated(boolean hgtInterpolated) {
        this.hgtInterpolated = hgtInterpolated;
    }

    /**
     * @return the tmpInterpolated
     */
    public boolean isTmpInterpolated() {
        return tmpInterpolated;
    }

    /**
     * @param tmpInterpolated
     *            the tmpInterpolated to set
     */
    public void setTmpInterpolated(boolean tmpInterpolated) {
        this.tmpInterpolated = tmpInterpolated;
    }

    /**
     * @return the dptInterpolated
     */
    public boolean isDptInterpolated() {
        return dptInterpolated;
    }

    /**
     * @param dptInterpolated
     *            the dptInterpolated to set
     */
    public void setDptInterpolated(boolean dptInterpolated) {
        this.dptInterpolated = dptInterpolated;
    }

    /**
     * @return the spdInterpolated
     */
    public boolean isSpdInterpolated() {
        return spdInterpolated;
    }

    /**
     * @param spdInterpolated
     *            the spdInterpolated to set
     */
    public void setSpdInterpolated(boolean spdInterpolated) {
        this.spdInterpolated = spdInterpolated;
    }

    /**
     * @return the dirInterpolated
     */
    public boolean isDirInterpolated() {
        return dirInterpolated;
    }

    /**
     * @param dirInterpolated
     *            the dirInterpolated to set
     */
    public void setDirInterpolated(boolean dirInterpolated) {
        this.dirInterpolated = dirInterpolated;
    }

    public float getValue(DATA_TYPE dataType) {
        float value = MISSING;

        switch (dataType) {
        case GEO_HEIGHT:
            value = getGeoHeight();
            break;

        case TEMPERATURE:
            value = getTemperature();
            break;

        case DEWPOINT:
            value = getDewpoint();
            break;

        case PRESSURE:
            value = getPressure();
            break;

        case WINDDIRECTION:
            value = getWindDirection();
            break;

        case WINDSPEED:
            value = getWindSpeed();
            break;

        case OMEGA:
            value = getOmega();
            break;

        case WIND_U:
            value = getWindU();
            break;

        case WIND_V:
            value = getWindV();
            break;
        } // switch

        return value;
    }

    private void computeUV() {
        if (windSpeed < MISSING && windDirection < MISSING) {
            Coordinate uv = WxMath.uvComp(windSpeed, windDirection);
            windU = (float) uv.x;
            windV = (float) uv.y;
        } else {
            windU = MISSING;
            windV = MISSING;
        }
    }

    private void computeSpdDir() {
        if (windU < MISSING && windV < MISSING) {
            Coordinate sd = WxMath.speedDir(windU, windV);
            windSpeed = (float) sd.x;
            windDirection = (float) sd.y;
        } else {
            windSpeed = MISSING;
            windDirection = MISSING;
        }
    }

    /**
     * Is this layer lower (higher pressure, or lower height) than a specified
     * layer?
     * 
     * @param layer
     *            A layer to compare to.
     * @return Is this layer lower than a specified layer?
     */
    public boolean isLowerThan(SoundingLayer layer) {
        boolean retValue = isLowerThan(layer.pressure, pressure);
        // if (!retValue) {
        // retValue = isHigherThan(layer.geoHeight, this.geoHeight);
        // }
        return retValue;
    }

    /**
     * Is this layer higher (lower pressure, or greater height) than a specified
     * layer?
     * 
     * @param layer
     *            A layer to compare to.
     * @return Is this layer higher than a specified layer?
     */
    public boolean isHigherThan(SoundingLayer layer) {
        boolean retValue = isHigherThan(layer.pressure, pressure);
        // if (!retValue) {
        // retValue = isLowerThan(layer.geoHeight, this.geoHeight);
        // }
        return retValue;
    }

    private boolean isLowerThan(Float level1, Float level2) {
        boolean retValue = false;

        if ((level1 != null) && (level1 < SoundingLayer.MISSING)) {
            if ((level2 != null) && (level2 < SoundingLayer.MISSING)) {
                retValue = level1 < level2;
            }
        }
        return retValue;
    }

    private boolean isHigherThan(Float level1, Float level2) {
        boolean retValue = false;

        if ((level1 != null) && (level1 < SoundingLayer.MISSING)) {
            if ((level2 != null) && (level2 < SoundingLayer.MISSING)) {
                retValue = level1 > level2;
            }
        }
        return retValue;
    }

    /**
     * Get the virtual temperature of this layer.
     * 
     * @return The layer virtual temperature.
     */
    public float getVirtualTemp() {
        float virtTemp = MISSING;
        if (temperature < SoundingLayer.MISSING) {
            if (dewpoint < SoundingLayer.MISSING) {
                virtTemp = (float) WxMath.virttemp(temperature, dewpoint,
                        pressure);
            } else {
                virtTemp = temperature;
            }
        }
        return virtTemp;
    }

    /**
     * Create a clone of this object.
     * 
     * @return The cloned data.
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    @Override
    public String toString() {
        return "LayerType=" + layerType + ":P=" + getPressure() + "mb:Z="
                + getGeoHeight() + "m:T=" + getTemperature() + "\u00B0C:Td="
                + getDewpoint() + "\u00B0C:WS=" + getWindSpeed() + "m/s:WD="
                + getWindDirection() + '\u00B0';

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Float.floatToIntBits(dewpoint);
        result = prime * result + Float.floatToIntBits(geoHeight);
        result = prime * result
                + ((layerType == null) ? 0 : layerType.hashCode());
        result = prime * result + Float.floatToIntBits(omega);
        result = prime * result + Float.floatToIntBits(pressure);
        result = prime * result + Float.floatToIntBits(temperature);
        result = prime * result + Float.floatToIntBits(windDirection);
        result = prime * result + Float.floatToIntBits(windSpeed);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        SoundingLayer other = (SoundingLayer) obj;
        if (Float.floatToIntBits(dewpoint) != Float
                .floatToIntBits(other.dewpoint)) {
            return false;
        }
        if (Float.floatToIntBits(geoHeight) != Float
                .floatToIntBits(other.geoHeight)) {
            return false;
        }
        if (layerType == null) {
            if (other.layerType != null) {
                return false;
            }
        } else if (!layerType.equals(other.layerType)) {
            return false;
        }
        if (Float.floatToIntBits(omega) != Float.floatToIntBits(other.omega)) {
            return false;
        }
        if (Float.floatToIntBits(pressure) != Float
                .floatToIntBits(other.pressure)) {
            return false;
        }
        if (Float.floatToIntBits(temperature) != Float
                .floatToIntBits(other.temperature)) {
            return false;
        }
        if (Float.floatToIntBits(windDirection) != Float
                .floatToIntBits(other.windDirection)) {
            return false;
        }
        if (Float.floatToIntBits(windSpeed) != Float
                .floatToIntBits(other.windSpeed)) {
            return false;
        }
        return true;
    }

    public static Comparator<SoundingLayer> getPressureComparator() {

        return new Comparator<SoundingLayer>() {

            @Override
            public int compare(SoundingLayer layerA, SoundingLayer layerB) {
                int retValue = 0;
                if (layerA != layerB) {
                    // reverse sort relative to height!
                    retValue = Double.compare(layerB.getPressure(), layerA
                            .getPressure());
                }
                return retValue;
            }
        };
    }

    public static Comparator<SoundingLayer> getHeightComparator() {

        return new Comparator<SoundingLayer>() {

            @Override
            public int compare(SoundingLayer layerA, SoundingLayer layerB) {
                int retValue = 0;
                if (layerA != layerB) {
                    // reverse sort relative to pressure!
                    retValue = Double.compare(layerA.getGeoHeight(), layerB
                            .getGeoHeight());
                }
                return retValue;
            }
        };
    }
}