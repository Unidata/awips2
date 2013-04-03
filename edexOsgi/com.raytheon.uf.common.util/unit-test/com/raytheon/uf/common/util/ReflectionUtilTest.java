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
package com.raytheon.uf.common.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.junit.Test;

/**
 * Test {@link ReflectionUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2012 634        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class ReflectionUtilTest {

    private static final String NON_EXISTANT = "nonExistant";

    @Test
    public void testNewInstanceOfAssignableTypeWithInterface() {
        List<?> list = ReflectionUtil.newInstanceOfAssignableType(List.class,
                ArrayList.class.getName());
        assertTrue(list instanceof ArrayList<?>);
    }

    @Test(expected = ReflectionException.class)
    public void testNewInstanceOfAssignableTypeThrowsReflectionExceptionOnIncompatibleCast() {
        ReflectionUtil.newInstanceOfAssignableType(List.class,
                HashSet.class.getName());
    }

    @Test(expected = ReflectionException.class)
    public void testNewInstanceOfAssignableTypeThrowsReflectionExceptionOnInvalidClassname() {
        ReflectionUtil.newInstanceOfAssignableType(List.class, NON_EXISTANT);
    }

    @Test
    public void testGetterRetrievesGetPrefixedValue() {
        byte[] expected = new byte[] { 't', 'e', 's', 't' };
        String value = new String(expected);

        byte[] actual = (byte[]) ReflectionUtil.getter(value, "bytes");

        assertArrayEquals(
                "Did not receive the expected value from the getter!",
                expected, actual);
    }

    @Test
    public void testGetterWithCastResult() {
        byte[] expected = new byte[] { 't', 'e', 's', 't' };
        String value = new String(expected);

        byte[] actual = ReflectionUtil.getter(byte[].class, value, "bytes");

        assertArrayEquals(
                "Did not receive the expected value from the getter!",
                expected, actual);
    }

    @Test
    public void testGetterRetrievesIsPrefixedValue() {
        List<String> list = Collections.emptyList();

        boolean value = (Boolean) ReflectionUtil.getter(list, "empty");

        assertTrue(
                "Incorrect value retrieved from the getter using an is prefix!",
                value);
    }

    @Test(expected = ReflectionException.class)
    public void testGetterThrowsReflectionExceptionOnNoSuchMethod() {
        byte[] expected = new byte[] { 't', 'e', 's', 't' };
        String value = new String(expected);

        ReflectionUtil.getter(value, NON_EXISTANT);
    }

    @Test
    public void testSetterCanSetValueOnProperlyNamedMethod() {
        final String name = "blah";

        Thread thread = new Thread();
        ReflectionUtil.setter(thread, "name", name);

        assertEquals("Setter did not set the correct value!", name,
                thread.getName());
    }

    @Test(expected = ReflectionException.class)
    public void testSetterThrowsReflectionExceptionOnNoSuchMethod() {
        String string = "blah";

        Thread thread = new Thread();
        ReflectionUtil.setter(thread, string, NON_EXISTANT);
    }
}
