package gov.noaa.nws.ncep.edex.common.sounding;

/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingLayer contain one layer of sounding information (pressure, height, temp, dewpt, windS, windD) for
 * one point (lat/lon) at a particular time (timeLine) and particular height.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 09/13/2010	 229		Chin Chen	Initial coding
 * 11/2010		 301		T. Lee		Moved Comparator to SoundingUtil
 * 03/2014      1116        T. Lee      Added constructor argument for DPD
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcSoundingLayer implements ISerializableObject, Cloneable {
    @DynamicSerializeElement
    private static final long serialVersionUID = 1324632468L;

    public static final float MISSING = IDecoderConstantsN.UAIR_FLOAT_MISSING;// -9999.f;

    public static final float LEGACY_MISSING = -999.f;

    // UAIR (standard) data type definitions
    public static enum DataType {
        TTAA, TTBB, TTCC, TTDD, PPAA, PPBB, PPCC, PPDD, UUAA, UUBB, UUCC, UUDD, MAXWIND_A, MAXWIND_C, TROPOPAUSE_A, TROPOPAUSE_C, ALLDATA
    }

    // mapping from UAIR (standard) data type to BUFRUA data type.
    public static HashMap<String, Integer> dataTypeMap = new HashMap<String, Integer>();
    static {
        dataTypeMap.put(DataType.TTAA.toString(), 2020); // Mandatory,
                                                         // Troppause, MaxWind
        dataTypeMap.put(DataType.TTCC.toString(), 2030); // Mandatory,
                                                         // Troppause, MaxWind
        dataTypeMap.put(DataType.TTBB.toString(), 2022); // SigT
        dataTypeMap.put(DataType.TTDD.toString(), 2032); // SigT
        dataTypeMap.put(DataType.PPBB.toString(), 2021); // SigW
        dataTypeMap.put(DataType.PPDD.toString(), 2031); // SigW
        dataTypeMap.put(DataType.PPAA.toString(), 0000); // TBD, BUFRUA does not
                                                         // support this now
        dataTypeMap.put(DataType.PPCC.toString(), 0000); // TBD, BUFRUA does not
                                                         // support this now
        dataTypeMap.put(DataType.MAXWIND_A.toString(), 2020);
        dataTypeMap.put(DataType.TROPOPAUSE_A.toString(), 2020);
        dataTypeMap.put(DataType.MAXWIND_C.toString(), 2030);
        dataTypeMap.put(DataType.TROPOPAUSE_C.toString(), 2030);

    }

    /*
     * Chin's Note: user should use either the following string or, for example,
     * DataType.TTAA.toString() for "data type" coding.
     * 
     * public static String TTAA = "TTAA"; public static String TTBB = "TTBB";
     * public static String TTCC = "TTCC"; public static String TTDD = "TTDD";
     * public static String PPAA = "PPAA"; public static String PPBB = "PPBB";
     * public static String PPCC = "PPCC"; public static String PPDD = "PPDD";
     * public static String MAXWIND = "MAXWIND"; public static String TROPOPAUSE
     * = "TROPOPAUSE"; public static String ALLDATA = "ALLDATA"; public static
     * String UUAA = "UUAA"; public static String UUBB = "UUBB"; public static
     * String UUCC = "UUCC"; public static String UUDD = "UUDD";
     */
    public static String DATATYPE_MISSING = "";

    // Pressure of the layer, in hectoPascals (milliBars), if known.
    @DynamicSerializeElement
    private float pressure;

    // Geopotential height of the layer, in meters, if known.
    @DynamicSerializeElement
    private float geoHeight;

    // Temperature of the layer, in Celsius.
    @DynamicSerializeElement
    private float temperature;

    // Dewpoint of the layer, in Celsius.
    @DynamicSerializeElement
    private float dewpoint;

    // Wind speed, in Knots
    @DynamicSerializeElement
    private float windSpeed;

    // Wind direction, in degrees.
    @DynamicSerializeElement
    private float windDirection;

    // U component of wind speed, in knots.
    @DynamicSerializeElement
    private float windU;

    // V component of wind speed
    @DynamicSerializeElement
    private float windV;

    // Vertical velocity,
    @DynamicSerializeElement
    private float omega;

    // specific humidity
    @DynamicSerializeElement
    private float specHumidity;

    // relative humidity
    @DynamicSerializeElement
    private float relativeHumidity;

    // dewpoint depression
    @DynamicSerializeElement
    private float dpd;

    public NcSoundingLayer() {
        this.pressure = MISSING;
        this.geoHeight = MISSING;
        this.temperature = MISSING;
        this.dewpoint = MISSING;
        this.windSpeed = MISSING;
        this.windDirection = MISSING;
        this.windU = MISSING;
        this.windV = MISSING;
        this.omega = MISSING;
        this.specHumidity = MISSING;
        this.relativeHumidity = MISSING;
        this.dpd = MISSING;
    }

    /**
     * 
     * @param pressure
     * @param geoHeight
     * @param temperature
     * @param dewpoint
     * @param windSpeed
     * @param windDirection
     * @param windU
     * @param windV
     * @param omega
     * @param specHumidity
     * @param dpd
     */
    public NcSoundingLayer(float pressure, float geoHeight, float temperature,
            float dewpoint, float windSpeed, float windDirection, float windU,
            float windV, float omega, float specHumidity,
            float relativeHumidity, float dpd) {
        super();
        this.pressure = pressure;
        this.geoHeight = geoHeight;
        this.temperature = temperature;
        this.dewpoint = dewpoint;
        this.windSpeed = windSpeed;
        this.windDirection = windDirection;
        this.windU = windU;
        this.windV = windV;
        this.omega = omega;
        this.specHumidity = specHumidity;
        this.relativeHumidity = relativeHumidity;
        this.dpd = dpd;

    }

    public NcSoundingLayer(float pressure, float geoHeight, float temperature,
            float dewpoint, float windSpeed, float windDirection, float windU,
            float windV, float omega, float specHumidity, float relativeHumidity) {
        super();
        this.pressure = pressure;
        this.geoHeight = geoHeight;
        this.temperature = temperature;
        this.dewpoint = dewpoint;
        this.windSpeed = windSpeed;
        this.windDirection = windDirection;
        this.windU = windU;
        this.windV = windV;
        this.omega = omega;
        this.specHumidity = specHumidity;
        this.relativeHumidity = relativeHumidity;
        this.dpd = MISSING;

    }

    /*
     * public String getDataType() { return dataType; }
     * 
     * public void setDataType(String dataType) { this.dataType = dataType; }
     */
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
        if (pressure == LEGACY_MISSING)
            this.pressure = MISSING;
        else
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
        if (geoHeight == LEGACY_MISSING)
            this.geoHeight = MISSING;
        else
            this.geoHeight = geoHeight;
    }

    /**
     * @return the temperature in Celsius
     */
    public float getTemperature() {
        return temperature;
    }

    /**
     * @param temperature
     *            the temperature in Celsius
     */
    public void setTemperature(float temperature) {
        if (temperature == LEGACY_MISSING)
            this.temperature = MISSING;
        else
            this.temperature = temperature;
    }

    /**
     * @return the dpd
     */
    public float getDpd() {
        return dpd;
    }

    /**
     * @param dpd
     *            the dpd to set
     */
    public void setDpd(float dpd) {
        if (dpd == LEGACY_MISSING)
            this.dpd = MISSING;
        else
            this.dpd = dpd;
    }

    /**
     * @return the dewpoint in Celsius
     */
    public float getDewpoint() {
        return dewpoint;
    }

    /**
     * @param dewpoint
     *            the dewpoint in Celsius
     */
    public void setDewpoint(float dewpoint) {
        if (dewpoint == LEGACY_MISSING)
            this.dewpoint = MISSING;
        else
            this.dewpoint = dewpoint;
    }

    /**
     * @return the windSpeed Knots
     */
    public float getWindSpeed() {
        return windSpeed;
    }

    /**
     * @param windSpeed
     *            the windSpeed inKnots
     */
    public void setWindSpeed(float windSpeed) {
        if (windSpeed == LEGACY_MISSING)
            this.windSpeed = MISSING;
        else
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
        if (windDirection == LEGACY_MISSING)
            this.windDirection = MISSING;
        else
            this.windDirection = windDirection;
        computeUV();
    }

    /**
     * @return the U component of windSpeed in Knots
     */
    public float getWindU() {
        return windU;
    }

    /**
     * @param windU
     *            the U component windSpeed in Knots
     */
    public void setWindU(float windU) {
        this.windU = windU;
        computeSpdDir();
    }

    /**
     * @return the V component of windSpeed in Knots
     */
    public float getWindV() {
        return windV;
    }

    /**
     * @param windV
     *            the V component windSpeed in Knots
     */
    public void setWindV(float windV) {
        this.windV = windV;
        computeSpdDir();
    }

    /**
     * @return the vertical velocity in millibars/sec??
     */
    public float getOmega() {
        return this.omega;
    }

    /**
     * @param omega
     *            the vertical velocity in millibars/sec??
     */
    public void setOmega(float omega) {
        if (omega == LEGACY_MISSING)
            this.omega = MISSING;
        else
            this.omega = omega;
    }

    public float getSpecHumidity() {
        return specHumidity;
    }

    public void setSpecHumidity(float specHumidity) {
        this.specHumidity = specHumidity;
    }

    public float getRelativeHumidity() {
        return relativeHumidity;
    }

    public void setRelativeHumidity(float relativeHumidity) {
        this.relativeHumidity = relativeHumidity;
    }

    private void computeUV() {
        if (windSpeed > MISSING && windDirection > MISSING) {
            Coordinate uv = uvComp(windSpeed, windDirection);
            windU = (float) uv.x;
            windV = (float) uv.y;
        } else {
            windU = MISSING;
            windV = MISSING;
        }
    }

    private void computeSpdDir() {
        if (windU > MISSING && windV > MISSING) {
            Coordinate sd = speedDir(windU, windV);
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
    public boolean isLowerThan(NcSoundingLayer layer) {
        boolean retValue = isLowerThan(layer.pressure, this.pressure);
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
    public boolean isHigherThan(NcSoundingLayer layer) {
        boolean retValue = isHigherThan(layer.pressure, this.pressure);
        // if (!retValue) {
        // retValue = isLowerThan(layer.geoHeight, this.geoHeight);
        // }
        return retValue;
    }

    private boolean isLowerThan(Float level1, Float level2) {
        boolean retValue = false;

        if ((level1 != null) && (level1 < NcSoundingLayer.MISSING)) {
            if ((level2 != null) && (level2 < NcSoundingLayer.MISSING)) {
                retValue = level1 < level2;
            }
        }
        return retValue;
    }

    private boolean isHigherThan(Float level1, Float level2) {
        boolean retValue = false;

        if ((level1 != null) && (level1 < NcSoundingLayer.MISSING)) {
            if ((level2 != null) && (level2 < NcSoundingLayer.MISSING)) {
                retValue = level1 > level2;
            }
        }
        return retValue;
    }

    /**
     * Create a clone of this object.
     * 
     * @return The cloned data.
     */

    @Override
    public String toString() {

        return "Pressure=" + getPressure() + "mb:Z=" + getGeoHeight() + "m:T="
                + getTemperature() + "\u00B0C:Td=" + getDewpoint()
                + "\u00B0C:WS=" + getWindSpeed() + "m/s:WD="
                + getWindDirection() + "\u00B0:SH=" + getSpecHumidity()
                + "\u00B0C" + getDpd();

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
        result = prime * result + Float.floatToIntBits(omega);
        result = prime * result + Float.floatToIntBits(pressure);
        result = prime * result + Float.floatToIntBits(temperature);
        result = prime * result + Float.floatToIntBits(windDirection);
        result = prime * result + Float.floatToIntBits(windSpeed);
        result = prime * result + Float.floatToIntBits(specHumidity);
        result = prime * result + Float.floatToIntBits(relativeHumidity);
        result = prime * result + Float.floatToIntBits(dpd);
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
        NcSoundingLayer other = (NcSoundingLayer) obj;
        if (Float.floatToIntBits(dewpoint) != Float
                .floatToIntBits(other.dewpoint)) {
            return false;
        }
        if (Float.floatToIntBits(geoHeight) != Float
                .floatToIntBits(other.geoHeight)) {
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
        if (Float.floatToIntBits(specHumidity) != Float
                .floatToIntBits(other.specHumidity)) {
            return false;
        }
        if (Float.floatToIntBits(relativeHumidity) != Float
                .floatToIntBits(other.relativeHumidity)) {
            return false;
        }
        if (Float.floatToIntBits(dpd) != Float.floatToIntBits(other.dpd)) {
            return false;
        }
        return true;
    }

    /**
     * Convert wind speed and direction to U/V components.
     * 
     * @param windSpeed
     * @param windDirection
     * @return coordinate containing u,v
     */
    public static Coordinate uvComp(float windSpeed, float windDirection) {
        double angle = Math.toRadians(windDirection);
        double u = -windSpeed * Math.sin(angle);
        double v = -windSpeed * Math.cos(angle);

        return new Coordinate(u, v);
    }

    public static Coordinate speedDir(float u, float v) {
        double spd = Math.hypot(u, v);
        double dir = Math.toDegrees(Math.atan2(-u, -v));
        if (dir < 0) {
            dir += 360;
        }
        return new Coordinate(spd, dir);
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

}
