package com.raytheon.uf.common.registry.ebxml;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * A container Class for a queryable attributes that are of type Calendar.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class CalendarAttribute extends QueryableAttribute<Calendar> {

    /**
     * Constant for the date/time format used to store and query Calendar attributes.
     */
    public static final String DATE_TIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss:SSS'Z'";

    /**
     * Default Constructor required for serialization.
     */
    public CalendarAttribute() {

    }

    /**
     * Create a CalendarAttribute set with the provided Calendar.
     * 
     * @param calendar
     *            A Calendar Object set to the date and time to query for.
     */
    public CalendarAttribute(Calendar calendar) {
        super(calendar);
    }

    /**
     * Return the formatted text of this QueryableAttribute.
     * 
     * @return The String value of the Calendar used to create
     *         this QueryableAttribute, in <code>DATE_TIME_FORMAT</code> format.
     */
    @Override
    public String getQueryValue() {
        SimpleDateFormat df = new SimpleDateFormat(DATE_TIME_FORMAT);
        return df.format(value.getTime());
    }

    /**
     * Calendar Attributes are stored as Strings in the registry, so
     * return 'stringValue';
     * 
     * @return The type of value attribute the slot for this QueryableAttribute contains.
     * 
     */
    @Override
    public String getSlotValueType() {
        return "stringValue";
    }
}
