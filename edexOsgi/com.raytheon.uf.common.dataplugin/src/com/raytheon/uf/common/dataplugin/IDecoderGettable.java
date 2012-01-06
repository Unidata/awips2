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
package com.raytheon.uf.common.dataplugin;

import java.util.Collection;

import javax.measure.unit.Unit;

/**
 * This interface allows a class to expose a getter interface that allows
 * clients to access data by parameter name. The returned values carry both the
 * value and the units of the measurement. A String method allows non-numeric
 * values to be retrieved.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071129            472 jkorman     Initial Coding.
 * 20090402            952 jsanchez    Added PIREP constants.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface IDecoderGettable {

    public static class Amount {
        private final Number value;

        private final Unit<?> unit;

        public Amount(long value, Unit<?> unit) {
            this.value = new Long(value);
            this.unit = unit;
        }

        public Amount(double value, Unit<?> unit) {
            this.value = new Double(value);
            this.unit = unit;
        }

        /**
         * @return the value
         */
        public Number getValue() {
            return value;
        }

        /**
         * @return the unit
         */
        public Unit<?> getUnit() {
            return unit;
        }

        public double doubleValue() {
            return value.doubleValue();
        }

        public long longValue() {
            return value.longValue();
        }
    }

    /**
     * Get the value and units of a named parameter within this observation.
     * 
     * @param paramName
     *            The name of the parameter value to retrieve.
     * @return An Amount with value and units. If the parameter is unknown, a
     *         null reference is returned.
     */
    public Amount getValue(String paramName);

    /**
     * Get the value and units of a named parameter within this observation that
     * has a multiplicity greater than 1.
     * 
     * @param paramName
     *            The name of the parameter value to retrieve.
     * @return An Amount with value and units. If the parameter is unknown, a
     *         null reference is returned.
     */
    public Collection<Amount> getValues(String paramName);

    /**
     * Get the value of a parameter that is represented as a String.
     * 
     * @param paramName
     *            The name of the parameter value to retrieve.
     * @return The String value of the parameter. If the parameter is unknown, a
     *         null reference is returned.
     */
    public String getString(String paramName);

    /**
     * Get the values of a parameter that is represented as a String.
     * 
     * @param paramName
     *            The name of the parameter value to retrieve.
     * @return An array of String values of the parameter. If the parameter is
     *         unknown, a null reference is returned.
     */
    public String[] getStrings(String paramName);

    public static final String SFC_TEMP = "SFC.TEMP";

    public static final String SFC_DWPT = "SFC.DWPT";

    public static final String SFC_WNDSPD = "SFC.WND.SPD";

    public static final String SFC_WNDDIR = "SFC.WND.DIR";

    public static final String SFC_WNDGST = "SFC.WND.GUST";

    public static final String STA_LAT = "STA.LAT";

    public static final String STA_LON = "STA.LON";

    public static final String PRES_STATION = "SFC.PRESS.STATION";

    public static final String PRES_SLP = "SFC.PRESS.SLP";

    public static final String PRES_ALTSG = "SFC.PRESS.ALTIMETER";

    public static final String UA_PRESSURE = "UA.PRESSURE";

    public static final String UA_GEOHGT = "UA.GEOPOTENTIAL_HGT";
    
    public static final String UA_FLTLVL = "UA.FLT_LVL";
    
    public static final String UA_ICETYPE = "UA.ICE_TYPE";
    
    public static final String UA_ICEINTENSE = "UA.ICE_INTENSITY";
    
    public static final String UA_TURBFREQ = "UA.TURB_FREQ";
    
    public static final String UA_TURBINTENSE = "UA.TURB_INTENSITY";
    
    public static final String UA_TOPHGT = "UA.TOP_HGT";
    
    public static final String UA_BOTHGT = "UA.BOTTOM_HGT";

}
