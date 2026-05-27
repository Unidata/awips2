package com.raytheon.uf.common.registry.ebxml;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * A container Class for a queryable attributes that are of type String.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Jan 20, 2014 2538       mpduff      Override toString
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class StringAttribute extends QueryableAttribute<String> {

    /**
     * Default Constructor required for serialization.
     */
    public StringAttribute() {
    }

    /**
     * Create an StringAttribute with an 'in list' relation.
     * 
     * @param values
     *            The values to query for.
     */
    public StringAttribute(List<String> values) {
        super(values);
    }

    /**
     * Create a StringAttribute with an equals relation.
     * 
     * @param value
     *            The value to query for.
     */
    public StringAttribute(String value) {
        super(value);
    }

    /**
     * Create a StringAttribute with a 'like' relation.
     * 
     * @param value
     *            The value to query for.
     * 
     * @param isLike
     *            Specify whether or not comparison should be like.
     */
    public StringAttribute(String value, boolean isLike) {
        super(value, isLike);
    }

    /**
     * Return the formatted text of this StringAttribute including the
     * processing of a List and like values.
     * 
     * @return The formatted value for this StringAttribute.
     */
    @Override
    public String getQueryValue() {
        if (this.like) {
            return "'%" + this.value + "%'";
        } else if (this.values != null) {
            StringBuilder sb = new StringBuilder();
            for (String t : this.values) {
                sb.append("'" + t + "',");
            }
            String items = sb.toString();
            return items.substring(0, items.length() - 1);
        } else {
            return "'" + this.value + "'";
        }
    }

    /**
     * Return the type of slot used to contain the value. For StringAttribute
     * that is "stringValue".
     * 
     * @return The type of value attribute the slot for this StringAttribute
     *         contains.
     * 
     * @see StringValueType
     */
    @Override
    public String getSlotValueType() {
        return "stringValue";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.value;
    }
}
