package com.raytheon.uf.common.registry.ebxml;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotValueType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * A container Class for a queryable attributes that are of type Boolean or
 * boolean.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2012 455        jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class BooleanAttribute extends QueryableAttribute<Boolean> {

    /**
     * Default Constructor required for serialization.
     */
    public BooleanAttribute() {
    }

    /**
     * Create an BooleanAttribute with an implied equals relation.
     * 
     * @param value
     *            The value to query for.
     */
    public BooleanAttribute(Boolean value) {
        super(value);
    }

    /**
     * Return the formatted text of this BooleanAttribute including 
     * the processing of a List of values.
     * 
     * @return The formatted value for this QueryableAttribute.
     */
    @Override
    public String getQueryValue() {
        return this.value.toString();
   }

    /**
     * Return the type of slot used to contain the value.  For
     * BooleanAttribute that is "booleanValue".
     * 
     * @return The type of value attribute the slot for this QueryableAttribute contains.
     * 
     * @see SlotValueType
     */
    @Override
    public String getSlotValueType() {
        return "booleanValue";
    }

}
