package com.raytheon.uf.common.registry.ebxml;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotValueType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * A container Class for a queryable attributes that are of type Double, Float,
 * double, or float.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2012 455        jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class DoubleAttribute extends QueryableAttribute<Double> {

    /**
     * Default Constructor required for serialization.
     */
    public DoubleAttribute() {

    }

    /**
     * Create an DoubleAttribute with an implied equals relation.
     * 
     * @param value
     *        The value to query for.
     */
    public DoubleAttribute(Double value) {
        super(value);
    }

    /**
     * Create an DoubleAttribute with an implied 'in list' relation.
     * 
     * @param values
     *        The values to query for.
     */
    public DoubleAttribute(List<Double> values) {
        super(values);
    }

    /**
     * Return the formatted text of this DoubleAttribute including 
     * the processing of a List of values.
     * 
     * @return The formatted value for this QueryableAttribute.
     */
    @Override
    public String getQueryValue() {
        if (this.values != null) {
           StringBuilder sb = new StringBuilder();
           for (Double t : this.values) {
                sb.append(t+",");
            }
            String items = sb.toString();
            return items.substring(0,items.length()-1);
        } else {
           return this.value.toString();
        }
   }

    /**
     * Return the type of slot used to contain the value.  For
     * DoubleAttribute that is "floatValue".
     * 
     * @return The type of value attribute the slot for this QueryableAttribute contains.
     * 
     * @see SlotValueType
     */
    @Override
    public String getSlotValueType() {
        return "floatValue";
    }

}
