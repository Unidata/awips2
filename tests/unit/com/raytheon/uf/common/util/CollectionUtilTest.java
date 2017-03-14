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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.junit.Test;

/**
 * Test {@link CollectionUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012 740        djohnson     Initial creation
 * Jul 26, 2012 955        djohnson     Add isNullOrEmpty for {@link Collection}s.
 * Feb 12, 2013 1543       djohnson     Test combining of arrays.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class CollectionUtilTest {

    @Test
    public void testIsNullOrEmptyReturnsTrueForNullArray() {
        String[] array = null;
        assertTrue(CollectionUtil.isNullOrEmpty(array));
    }

    @Test
    public void testIsNullOrEmptyReturnsTrueForEmptyArray() {
        String[] array = new String[0];
        assertTrue(CollectionUtil.isNullOrEmpty(array));
    }

    @Test
    public void testIsNullOrEmptyReturnsFalseForNonEmptyArray() {
        String[] array = new String[] { "" };
        assertFalse(CollectionUtil.isNullOrEmpty(array));
    }

    @Test
    public void testIsNullOrEmptyReturnsTrueForNullCollection() {
        assertTrue(CollectionUtil.isNullOrEmpty((Collection<?>) null));
    }

    @Test
    public void testIsNullOrEmptyReturnsTrueForEmptyCollection() {
        assertTrue(CollectionUtil.isNullOrEmpty(Collections.emptyList()));
    }

    @Test
    public void testIsNullOrEmptyReturnsFalseForNonEmptyCollection() {
        assertFalse(CollectionUtil.isNullOrEmpty(Arrays.asList("not empty")));
    }

    @Test
    public void twoAndThreeElementArraysCanBeCombinedIntoFiveElementArray() {
        Object[] objArray1 = new Object[] { new Object(), new Object(),
                new Object() };
        Object[] objArray2 = new Object[] { new Object(), new Object() };

        Object[] combined = CollectionUtil.combine(Object.class, objArray1,
                objArray2);
        assertThat(combined.length, is(5));

        for (int i = 0; i < objArray1.length; i++) {
            assertThat(combined[i], is(sameInstance(objArray1[i])));
        }

        for (int i = 0; i < objArray2.length; i++) {
            assertThat(combined[i + 3], is(sameInstance(objArray2[i])));
        }
    }

    @Test
    public void twoEmptyArraysCanBeCombinedIntoEmptyArray() {
        Object[] objArray1 = new Object[0];
        Object[] objArray2 = new Object[0];

        Object[] combined = CollectionUtil.combine(Object.class, objArray1,
                objArray2);
        assertThat(combined.length, is(0));
    }

    @Test
    public void nullFirstArrayCanBeCombinedWithValidArray() {
        Object[] objArray1 = null;
        Object[] objArray2 = new Object[] { new Object(), new Object() };

        Object[] combined = CollectionUtil.combine(Object.class, objArray1,
                objArray2);
        assertThat(combined.length, is(2));

        for (int i = 0; i < objArray2.length; i++) {
            assertThat(combined[i], is(sameInstance(objArray2[i])));
        }
    }

    @Test
    public void nullSecondArrayCanBeCombinedWithValidArray() {
        Object[] objArray1 = new Object[] { new Object(), new Object() };
        Object[] objArray2 = null;

        Object[] combined = CollectionUtil.combine(Object.class, objArray1,
                objArray2);
        assertThat(combined.length, is(2));

        for (int i = 0; i < objArray1.length; i++) {
            assertThat(combined[i], is(sameInstance(objArray1[i])));
        }
    }

    @Test
    public void twoNullArraysCanBeCombinedToReturnNull() {
        Object[] objArray1 = null;
        Object[] objArray2 = null;

        Object[] combined = CollectionUtil.combine(Object.class, objArray1,
                objArray2);
        assertThat(combined, is(nullValue()));
    }
}
