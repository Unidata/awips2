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
package com.raytheon.uf.common.nc.bufr.time;

import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.DatatypeConverter;

/**
 * Parser utility for NetCDF BUFR time fields
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrTimeFieldParser {

    public static final String SECONDS_OFFSET = "seconds";

    public static final String MINUTES_OFFSET = "minutes";

    public static final String HOURS_OFFSET = "hours";

    public static final String DAYS_OFFSET = "days";

    public static final Map<String, Integer> CAL_FIELD_MAP;

    public static final Pattern TIME_UNIT_PATTERN = Pattern
            .compile("^\\s*(\\S+)\\s+since\\s+(\\S+)\\s*$");

    static {
        Map<String, Integer> map = new HashMap<String, Integer>();
        map.put(SECONDS_OFFSET, Calendar.SECOND);
        map.put(MINUTES_OFFSET, Calendar.MINUTE);
        map.put(HOURS_OFFSET, Calendar.HOUR_OF_DAY);
        map.put(DAYS_OFFSET, Calendar.DAY_OF_MONTH);
        CAL_FIELD_MAP = Collections.unmodifiableMap(map);
    }

    /**
     * 
     */
    private BufrTimeFieldParser() {
    }

    /**
     * Parse units for time reference and add offset value. Results returned in
     * calendar object.
     * 
     * @param value
     * @param fieldUnits
     * @return
     * @throws TimeFieldParseException
     */
    public static Calendar processTimeField(Object value, String fieldUnits)
            throws TimeFieldParseException {
        if (value == null) {
            return null;
        }
        if (!(value instanceof Number)) {
            throw new TimeFieldParseException(
                    "Time field with non-numeric value: " + value);
        }
        Number offset = (Number) value;
        Matcher m = TIME_UNIT_PATTERN.matcher(fieldUnits);
        if (m.matches()) {
            String offsetUnits = m.group(1).trim();
            Integer calField = CAL_FIELD_MAP.get(offsetUnits);
            if (calField == null) {
                throw new TimeFieldParseException(
                        "Unsupported time offset unit: " + offsetUnits);
            }
            String refString = m.group(2).trim();
            Calendar refCal;
            try {
                refCal = DatatypeConverter.parseDateTime(refString);
            } catch (Exception e) {
                throw new TimeFieldParseException(
                        "Unsupported time reference string format: "
                                + refString);
            }
            refCal.add(calField, offset.intValue());
            return refCal;
        } else {
            throw new TimeFieldParseException(
                    "Time field with unknown unit format: " + fieldUnits);
        }
    }
}
