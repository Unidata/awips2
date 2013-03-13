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

package com.raytheon.uf.common.time.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.beanutils.Converter;

/**
 * Custom converter implementation for converting DataTime objects from Strings
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 9/17/08      1531       bphillip    Initial Creation
 * Mar 13, 2013 1789       bsteffen    Move Calendar and Date parsing out of
 *                                     ConvertUtil and also fix date parsing.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DateConverter implements Converter {

    // Allows ConvertUtils to successfully convert:
    // 1) TimeRange.getStart().toString()
    // 2) TimeRange.getEnd().toString()
    // 3) "BinOffset usage"
    private static final String[] DATE_FORMATS = { "yyyy-MM-dd HH:mm:ss.S",
            "EEE MMM dd HH:mm:ss z yyyy", "yyyy-MM-dd HH:mm:ss" };

    private ThreadLocal<SimpleDateFormat[]> formatHolder = new ThreadLocal<SimpleDateFormat[]>() {

        @Override
        protected SimpleDateFormat[] initialValue() {
            SimpleDateFormat[] value = new SimpleDateFormat[DATE_FORMATS.length];
            for (int i = 0; i < value.length; i += 1) {
                value[i] = new SimpleDateFormat(DATE_FORMATS[i]);
            }
            return value;
        }

    };

    @Override
    public Object convert(Class clazz, Object value) {
        if (value instanceof String) {
            String valueString = (String) value;
            try {
                // see if string is in ISO 8601
                return DatatypeConverter.parseDateTime(valueString).getTime();
            } catch (Exception e) {
                // try the formats.
            }

            for (SimpleDateFormat format : formatHolder.get()) {
                try {
                    return format.parseObject(valueString);
                } catch (ParseException e) {
                    // try the next one.
                }
            }
            return null;
        }
        return null;
    }


}
