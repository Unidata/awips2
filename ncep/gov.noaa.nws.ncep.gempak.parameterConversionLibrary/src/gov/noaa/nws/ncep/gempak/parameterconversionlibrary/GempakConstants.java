/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameterconversionlibrary;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket# Engineer    Description
 * ------------ ------- ----------- --------------------------
 *        2011	398		Archana		Initial Creation
 * 04 Apr 2011	398		F. J. Yen	Change fields in METERS_TO_KILOMETERS. FEET_TO_MILES,
 * 									METERS_TO_DECAMETERS to floating point.
 * </pre>
 * 
 * @author archana
 */
public class GempakConstants {

    /** Missing Float value */
    public static final float RMISSD = -9999.0f;

    /** Missing Integer value */
    public static final float IMISSD = -9999;

    /** Missing value fuzziness */
    public static final float RDIFFD = 0.1f;

    /** Earth's radius */
    public static final float RADIUS = 6371200.f;

    /** Earth's angular velocity */
    public static final float OMEGA = 7.2921E-5f;

    /** Acceleration of gravity */
    public static final float GRAVTY = 9.80616f;

    public static final float RDGAS = 287.04f;

    /** Gas constant of dry air */
    public static final float RKAP = RDGAS / GRAVTY;

    /** Centigrade -> Kelvin */
    public static final float TMCK = 273.15f;

    /** US standard atmospheric lapse rate */
    public static final float GAMUSD = 6.5f;

    /** Poisson constant */
    public static final float RKAPPA = 2.0f / 7.0f;

    /** Inverse Poisson constant */
    public static final float AKAPPA = 7.0f / 2.0f;

    /** Used in conversion from Celsius to Farenheit */
    public static final float RPRM = 9.0f / 5.0f;

    public static final float PI = 3.14159265f;

    public static final float HALFPI = PI / 2.0f;

    public static final float TWOPI = 2 * PI;

    public static final float PI4TH = PI / 4;

    public static final float DTR = PI / 180;

    public static final float RTD = 180 / PI;

    public static final float FEET_TO_METERS = 0.3048f;

    public static final float METERS_TO_FEET = 3.28084f;

    public static final float MILES_PER_HOUR_TO_KNOTS = 0.868976f;

    public static final float KNOTS_TO_MILES_PER_HOUR = 1 / 0.868976f;

    public static final float METERS_PER_SEC_TO_KNOTS = 1.9425f;

    public static final float KNOTS_TO_METERS_PER_SEC = 1 / 1.9425f;

    public static final float FEET_TO_MILES = 1f / 5280f;

    public static final float MILES_TO_FEET = 5280;

    public static final float METERS_TO_KILOMETERS = 1 / 1000f;

    public static final float KILOMETERS_TO_METERS = 1000.0f;

    public static final float METERS_TO_DECAMETERS = 1f / 10f;

    public static final float DECAMETERS_TO_METERS = 10.0f;

    public static final float NAUTICAL_MILES_TO_METERS = 1852.0f;

    public static final float METERS_TO_NAUTICAL_MILES = 1 / 1852.0f;

    public static final float INCHES_TO_MILLIMETERS = 25.4f;

    public static final float MILLIMETERS_TO_INCHES = .0393701f;
}
