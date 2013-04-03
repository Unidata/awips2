package com.raytheon.uf.common.registry.ebxml;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotValueType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.util.StringUtil;

/**
 * A container Class for a queryable attributes that are of type Integer, int or
 * BigInteger.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 20, 2012 0743       djohnson    Change to support {@link BigInteger}.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class IntegerAttribute extends QueryableAttribute<BigInteger> {

    /**
     * Default Constructor required for serialization.
     */
    public IntegerAttribute() {

    }

    /**
     * Create an IntegerAttribute with an implied equals relation.
     * 
     * @param value
     *            The value to query for.
     */
    public IntegerAttribute(int value) {
        this(BigInteger.valueOf(value));
    }

    /**
     * Create an IntegerAttribute with an implied equals relation.
     * 
     * @param value
     *        The value to query for.
     */
    public IntegerAttribute(BigInteger value) {
        super(value);
    }

    /**
     * Create an IntegerAttribute with an implied 'in list' relation.
     * 
     * @param values
     *        The values to query for.
     */
    public IntegerAttribute(List<BigInteger> values) {
        super(values);
    }

    /**
     * Create an {@link IntegerAttribute} from a long value.
     * 
     * @param value
     */
    public IntegerAttribute(long value) {
        this(BigInteger.valueOf(value));
    }

    /**
     * Return the formatted text of this IntegerAttribute including the
     * processing of a List of values.
     * 
     * @return The formatted value for this QueryableAttribute.
     */
    @Override
    public String getQueryValue() {
        return (values == null) ? value.toString() : StringUtil.join(
                this.values, ',');
   }

    /**
     * Return the type of slot used to contain the value.  For
     * IntegerAttribute that is "integerValue".
     * 
     * @return The type of value attribute the slot for this QueryableAttribute contains.
     * 
     * @see SlotValueType
     */
    @Override
    public String getSlotValueType() {
        return "integerValue";
    }

    /**
     * Retrieve an IntegerAttribute from {@link Integers}.
     * 
     * @param values
     *            the integers
     * @return the Integer Attribute
     */
    public static IntegerAttribute fromIntegers(
            List<Integer> values) {
        List<BigInteger> bigInts = new ArrayList<BigInteger>(values.size());
        for (Integer integer : values) {
            bigInts.add(BigInteger.valueOf(integer));
        }
        return new IntegerAttribute(bigInts);
    }
}
