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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.junit.Test;

/**
 * Test {@link TestUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012            djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TestUtilTest {

    @Test
    public void testAssertEqualsAndHashCodeContractDoesNotProvideAssertionErrorWhenEqualAndHashcodeCompliant() {
        String objectUnderTest = "someString";
        String equalObject = "someString";

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Arrays.asList(equalObject), Collections.<String> emptyList());
    }

    @Test(expected = AssertionError.class)
    public void testAssertEqualsAndHashCodeContractProvidesAssertionErrorWhenNotEquals() {
        String objectUnderTest = "someString";
        String unequalObject = "someOtherString";

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Arrays.asList(unequalObject), Collections.<String> emptyList());
    }

    @Test(expected = AssertionError.class)
    public void testAssertEqualsAndHashCodeContractProvidesAssertionErrorWhenNotHashcodeCompliant() {
        CustomHashcode objectUnderTest = new CustomHashcode(true, 1);
        CustomHashcode unequalObject = new CustomHashcode(true, 2);

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Arrays.asList(unequalObject),
                Collections.<CustomHashcode> emptyList());
    }

    @Test
    public void testAssertEqualsAndHashCodeContractDoesNotProvideAssertionErrorWhenUnequalObjectsAreNotEqualNorHashcodeCompliant() {
        String objectUnderTest = "someString";
        String unequalObject = "someOtherString";

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Collections.<String> emptyList(), Arrays.asList(unequalObject));
    }

    @Test(expected = AssertionError.class)
    public void testAssertEqualsAndHashCodeContractDoesProvideAssertionErrorWhenUnequalObjectsAreEquals() {
        String objectUnderTest = "someString";
        String equalObject = "someString";

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Collections.<String> emptyList(), Arrays.asList(equalObject));
    }

    @Test(expected = AssertionError.class)
    public void testAssertEqualsAndHashCodeContractDoesProvideAssertionErrorWhenUnequalObjectsAreHashcodeCompliant() {
        CustomHashcode objectUnderTest = new CustomHashcode(false, 1);
        CustomHashcode equalObject = new CustomHashcode(false, 1);

        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Collections.<CustomHashcode> emptyList(),
                Arrays.asList(equalObject));
    }

    @Test
    public void testAssertCompareToSucceedsForCompliantComparable() {
        String objectUnderTest = "middle";
        final Collection<String> lessThanObjects = Arrays.asList(
                "lessThan1", "lessThan2");
        // Intentionally creating a new string object for comparison
        final Collection<String> equalObjects = Arrays.asList(new String(
                "middle"));
        final Collection<String> greaterThanObjects = Arrays.asList("zebras",
                "zimbabwe");

        TestUtil.assertCompareToContract(objectUnderTest, lessThanObjects,
                equalObjects, greaterThanObjects);
    }

    @Test(expected = AssertionError.class)
    public void testAssertCompareToFindsUncompliantGreaterThanObject() {
        String objectUnderTest = "middle";
        final Collection<String> lessThanObjects = Arrays.asList("lessThan1");
        // Intentionally creating a new string object for comparison
        final Collection<String> equalObjects = Arrays.asList(new String(
                "middle"));
        final Collection<String> greaterThanObjects = Arrays
                .asList("intentionallyLessThan");

        TestUtil.assertCompareToContract(objectUnderTest, lessThanObjects,
                equalObjects, greaterThanObjects);
    }

    @Test(expected = AssertionError.class)
    public void testAssertCompareToFindsUncompliantLessThanObject() {
        String objectUnderTest = "middle";
        final Collection<String> lessThanObjects = Arrays
                .asList("purposefullyGreaterThan");
        // Intentionally creating a new string object for comparison
        final Collection<String> equalObjects = Arrays.asList(new String(
                "middle"));
        final Collection<String> greaterThanObjects = Arrays.asList("zebras");

        TestUtil.assertCompareToContract(objectUnderTest, lessThanObjects,
                equalObjects, greaterThanObjects);
    }

    @Test(expected = AssertionError.class)
    public void testAssertCompareToFindsUncompliantEqualObject() {
        String objectUnderTest = "middle";
        final Collection<String> lessThanObjects = Arrays
                .asList("lessThan");
        final Collection<String> equalObjects = Arrays.asList("notEqual");
        final Collection<String> greaterThanObjects = Arrays.asList("zebras");

        TestUtil.assertCompareToContract(objectUnderTest, lessThanObjects,
                equalObjects, greaterThanObjects);
    }

    private static class CustomHashcode {
        private final int hashcode;

        private final boolean shouldBeEqual;

        private CustomHashcode(boolean shouldBeEqual, int hashcode) {
            this.shouldBeEqual = shouldBeEqual;
            this.hashcode = hashcode;
        }

        @Override
        public boolean equals(Object obj) {
            return shouldBeEqual;
        }

        @Override
        public int hashCode() {
            return hashcode;
        }
    }
}
