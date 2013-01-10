package com.raytheon.uf.viz.datadelivery.system;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Test {@link OperatorTypes}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/04/2013     1420     mpduff      Initial Creation.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class OperatorTypesTest {

    @Test
    public void testGreaterThanWorks() {
        OperatorTypes operatorType = OperatorTypes.GREATER_THAN;
        assertTrue("operand1 not greater than operand2",
                operatorType.evaluate(10l, 5l));
    }

    @Test
    public void testGreaterThanFailsWhenValuesEqual() {
        OperatorTypes operatorType = OperatorTypes.GREATER_THAN;
        assertFalse("operand1 not greater than operand2",
                operatorType.evaluate(10l, 10l));
    }

    @Test
    public void testGreaterThanFailsWhenValuesNotGreaterThan() {
        OperatorTypes operatorType = OperatorTypes.GREATER_THAN;
        assertFalse("operand1 not greater than operand2",
                operatorType.evaluate(5l, 10l));
    }

    @Test
    public void testGreaterThanEqualsWorks() {
        OperatorTypes operatorType = OperatorTypes.GREATER_THAN_EQUAL;
        assertTrue("operand1 not greater than operand2",
                operatorType.evaluate(10l, 5l));
    }

    @Test
    public void testGreaterThanEqualsWorksWhenValuesEqual() {
        OperatorTypes operatorType = OperatorTypes.GREATER_THAN_EQUAL;
        assertTrue("operand1 not greater than or equal to operand2",
                operatorType.evaluate(10l, 10l));
    }

    @Test
    public void testGreaterThanEqualsFailsWhenValuesNotGreaterThan() {
        OperatorTypes operatorType = OperatorTypes.GREATER_THAN_EQUAL;
        assertFalse("operand1 not greater than operand2",
                operatorType.evaluate(5l, 10l));
    }

    // //////////////////////

    @Test
    public void testLessThanWorks() {
        OperatorTypes operatorType = OperatorTypes.LESS_THAN;
        assertTrue("operand1 not less than operand2",
                operatorType.evaluate(10l, 20l));
    }

    @Test
    public void testLessThanFailsWhenValuesEqual() {
        OperatorTypes operatorType = OperatorTypes.LESS_THAN;
        assertFalse("operand1 not less than operand2",
                operatorType.evaluate(10l, 10l));
    }

    @Test
    public void testLessThanFailsWhenValuesNotLessThan() {
        OperatorTypes operatorType = OperatorTypes.LESS_THAN;
        assertFalse("operand1 not less than operand2",
                operatorType.evaluate(20l, 10l));
    }

    @Test
    public void testLessThanEqualsWorks() {
        OperatorTypes operatorType = OperatorTypes.LESS_THAN_EQUAL;
        assertTrue("operand1 not less than operand2",
                operatorType.evaluate(10l, 20l));
    }

    @Test
    public void testLessThanEqualsWorksWhenValuesEqual() {
        OperatorTypes operatorType = OperatorTypes.LESS_THAN_EQUAL;
        assertTrue("operand1 not less than or equal to operand2",
                operatorType.evaluate(10l, 10l));
    }

    @Test
    public void testLessThanEqualsFailsWhenValuesNotLessThan() {
        OperatorTypes operatorType = OperatorTypes.LESS_THAN_EQUAL;
        assertFalse("operand1 not less than operand2",
                operatorType.evaluate(20l, 10l));
    }

    @Test
    public void testEqualsWorks() {
        OperatorTypes ot = OperatorTypes.EQUAL;
        assertTrue("operand1 not equal to operand2", ot.evaluate(1l, 1l));
    }

    @Test
    public void testEqualsFailsWhenNotEqual() {
        OperatorTypes ot = OperatorTypes.EQUAL;
        assertFalse("operand1 not equal to operand2", ot.evaluate(1l, 12l));
    }

    @Test
    public void testNotEqualsWorks() {
        OperatorTypes ot = OperatorTypes.NOT_EQUAL;
        assertTrue("operand1 equal to operand2", ot.evaluate(1l, 15l));
    }

    @Test
    public void testNotEqualsFailsWhenEqual() {
        OperatorTypes ot = OperatorTypes.NOT_EQUAL;
        assertFalse("operand1 equal to operand2", ot.evaluate(1l, 1l));
    }
}
