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
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.apache.commons.beanutils.Converter;

/**
 * Custom converter implementation for converting DataTime objects from Strings
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 9/17/08      1531       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DateConverter implements Converter {

    private static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";

    private static final int QUEUE_SIZE = 20;

    private static Queue<SimpleDateFormat> formatters = new ConcurrentLinkedQueue<SimpleDateFormat>();

    @SuppressWarnings("rawtypes")
    @Override
    public Object convert(Class clazz, Object value) {
        if (value instanceof String) {
            SimpleDateFormat formatter = getDateFormat();
            try {
                return formatter.parseObject((String) value);
            } catch (ParseException e) {
                e.printStackTrace();
                return null;
            } finally {
                if (formatter != null && formatters.size() < QUEUE_SIZE) {
                    formatters.add(formatter);
                }
            }
        }
        return null;
    }

    private static SimpleDateFormat getDateFormat() {
        SimpleDateFormat m = formatters.poll();
        if (m == null) {
            m = new SimpleDateFormat(DATE_FORMAT);
        }

        return m;
    }

}
