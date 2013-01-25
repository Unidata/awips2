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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.lf5.util.StreamUtils;
import org.junit.Assert;
import org.junit.Ignore;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Utilities for testing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012 955        djohnson     Initial creation
 * Sep 07, 2012 1102       djohnson     Add test for more specification defined properties of hashCode/equals.
 * Oct 23, 2012 1286       djohnson     setupTestClassDir() takes any class.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public final class TestUtil {
    public static final Pattern COMMA_PATTERN = Pattern.compile(",");

    private static final Pattern NEW_LINE_PATTERN = Pattern.compile("\n");

    private static final Pattern INPUT_LINE_PATTERN = Pattern
            .compile("input=(.*)");

    private static final Pattern OUTPUT_LINE_PATTERN = Pattern
            .compile("output=(.*)");

    private TestUtil() {

    }

    /**
     * Assert that two objects are not equal.
     * 
     * @param objectUnderTest
     * 
     * @param shouldNotBeEqual
     */
    public static void assertNotEquals(Object objectUnderTest,
            Object shouldNotBeEqual) {
        assertNotEquals("Expected the two objects to not be equal!",
                objectUnderTest, shouldNotBeEqual);
    }

    /**
     * @param string
     * @param firstTime
     * @param secondTime
     */
    public static void assertNotEquals(String message, Object objectUnderTest,
            Object shouldNotBeEqual) {
        assertThat(message, objectUnderTest, not(equalTo(shouldNotBeEqual)));
    }

    /**
     * Assert the contents of two arrays are the same.
     * 
     * @param <T>
     *            the object type
     * @param expected
     *            the expected values
     * @param actual
     *            the actual values
     */
    public static <T> void assertArrayEquals(T[] expected, T[] actual) {
        Assert.assertArrayEquals("Expected [" + StringUtil.join(expected, ',')
                + "] but got [" + StringUtil.join(actual, ',') + "]", expected,
                actual);
    }

    /**
     * Assert the contents of two collections are the same.
     * 
     * @param <T>
     *            the object type
     * @param expected
     *            the expected collection
     * @param actual
     *            the actual collection
     */
    public static <T> void assertCollectionEquals(Collection<T> expected,
            Collection<T> actual) {
        Object[] expectedArray = expected.toArray(new Object[expected.size()]);
        Object[] actualArray = actual.toArray(new Object[actual.size()]);

        TestUtil.assertArrayEquals(expectedArray, actualArray);
    }

    /**
     * Assert the times represented by two {@link Calendar} objects are the
     * same.
     * 
     * @param message
     * 
     * @param expected
     * @param actual
     */
    public static void assertCalEquals(String message, Calendar expected,
            Calendar actual) {
        if (!(expected.getTimeInMillis() == actual.getTimeInMillis())) {
            // Create a readable String to display when comparing calendars
            String expectedString = TimeUtil.formatCalendar(expected);
            String actualString = TimeUtil.formatCalendar(actual);
            assertEquals(message, expectedString, actualString);
        }
    }

    /**
     * Retrieves the input and output parameter lines from the specified file.
     * 
     * @param callingClass
     *            the class requesting the file, used for relative file pathing
     * @param file
     *            name
     * @return a list of lists, [0] = inputs, [1] = outputs
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    public static List<List<String>> getInputsAndOutputs(Class<?> callingClass,
            String file)
            throws IOException {
        List<String> inputs = new ArrayList<String>();
        List<String> outputs = new ArrayList<String>();

        InputStream is = null;
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            is = callingClass.getResourceAsStream(file);

            StreamUtils.copy(is, bos);

            String contents = bos.toString("UTF-8");

            String[] inputsAndOutputs = NEW_LINE_PATTERN.split(contents);
            for (int i = 0; i < inputsAndOutputs.length; i = i + 2) {
                String inputLine = inputsAndOutputs[i];
                String outputLine = inputsAndOutputs[i + 1];

                Matcher inputMatcher = INPUT_LINE_PATTERN.matcher(inputLine);
                Matcher outputMatcher = OUTPUT_LINE_PATTERN.matcher(outputLine);

                if (inputMatcher.find() && outputMatcher.find()) {
                    inputs.add(inputMatcher.group(1));
                    outputs.add(outputMatcher.group(1));
                } else {
                    throw new IllegalArgumentException(
                            "Did not find properly formatted lines for input ["
                                    + inputLine + "] or output [" + outputLine
                                    + "]!");
                }

            }
        } finally {
            Util.close(is);
        }
        return Arrays.asList(inputs, outputs);
    }

    /**
     * Asserts the equals and hashcode contract for an object is implemented
     * correctly. Details can be found by reading the Javadoc for
     * {@link Object#equals(Object)} and {@link Object#hashCode()}.
     * 
     * @param objectUnderTest
     *            the object under test
     * @param equalObjects
     *            the objects that should be determined to be equal and return
     *            the same hashcode
     * @param unequalObjects
     *            the objects that should be determined to be unequal
     */
    public static <T> void assertEqualsAndHashcodeContract(T objectUnderTest,
            Collection<T> equalObjects, Collection<T> unequalObjects) {

        assertEquals(
                "The specified object should have been reflexively equal!",
                objectUnderTest, objectUnderTest);
        assertEquals("The specified object should have a consistent hashcode!",
                objectUnderTest.hashCode(), objectUnderTest.hashCode());
        
        for (T equalObject : equalObjects) {
            assertEquals(
                    "The specified object should have been equal, but wasn't!",
                    objectUnderTest, equalObject);
            assertEquals(
                    "The specified object should have been symmetrically equal, but wasn't!",
                    equalObject, objectUnderTest);
            assertEquals(
                    "The specified object should have the same hashcode, but it didn't!",
                    objectUnderTest.hashCode(), equalObject.hashCode());
        }

        for (T unequalObject : unequalObjects) {
            assertFalse(
                    "The specified object should not have been equal, but was!",
                    objectUnderTest.equals(unequalObject));
            assertFalse(
                    "The specified object should not have been symmetrically equal, but was!",
                    unequalObject.equals(objectUnderTest));
        }
    }

    /**
     * Asserts the {@link Comparable#compareTo(Object)} contract for an object
     * is implemented correctly. Details can be found by reading the Javadoc for
     * {@link Comparable#compareTo(Object)}.
     * 
     * @param objectUnderTest
     *            the object under test
     * @param lessThanObjects
     *            the objects that should be determined to be less than the
     *            object under test
     * @param equalObjects
     *            the objects that should be determined to be equal to the
     *            object under test
     * @param unequalObjects
     *            the objects that should be determined to be greater than the
     *            object under test
     */
    public static <T extends Comparable<? super T>> void assertCompareToContract(
            T objectUnderTest,
            Collection<T> lessThanObjects, Collection<T> equalObjects,
            Collection<T> greaterThanObjects) {
        assertEquals(
                "The specified object should have been equal with itself!", 0,
                objectUnderTest.compareTo(objectUnderTest));

        for (T lessThanObject : lessThanObjects) {
            assertTrue(
                    "The specified object should have returned a positive integer!",
                    objectUnderTest.compareTo(lessThanObject) > 0);
            assertEquals(
                    "The specified object should have returned a value that differed only in sign!",
                    (-objectUnderTest.compareTo(lessThanObject)),
                    lessThanObject.compareTo(objectUnderTest));
        }

        for (T equalObject : equalObjects) {
            assertEquals(
                    "The specified object should have been equal, but wasn't!",
                    0, objectUnderTest.compareTo(equalObject));
            assertEquals(
                    "The specified object should have been symettrically equal, but wasn't!",
                    0, equalObject.compareTo(objectUnderTest));
        }

        for (T greaterThanObject : greaterThanObjects) {
            assertTrue(
                    "The specified object should have returned a negative integer!",
                    objectUnderTest.compareTo(greaterThanObject) < 0);
            assertEquals(
                    "The specified object should have returned a value that differed only in sign!",
                    (-objectUnderTest.compareTo(greaterThanObject)),
                    greaterThanObject.compareTo(objectUnderTest));
        }
    }

    /**
     * Reads a resource from the relative location of the calling class.
     * 
     * @param callingClass
     * 
     * @param resource
     * 
     * @return the byte array representing the file contents
     */
    public static byte[] readResource(Class<?> callingClass, String resource) {
        InputStream is = null;
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
            is = callingClass.getResourceAsStream(resource);
            try {
                StreamUtils.copy(is, bos);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        } finally {
            PropertiesUtil.close(is);
            PropertiesUtil.close(bos);
        }

        return bos.toByteArray();
    }

    /**
     * Sets up a unique directory specific to the class requesting it. This
     * method will also delete any existing version that exists.
     * 
     * @param clazz
     *            the requesting class
     * @return the directory reference
     */
    public static File setupTestClassDir(Class<?> clazz) {
        File testDir = new File(System.getProperty("java.io.tmpdir"),
                clazz.getName());
        if (testDir.exists()) {
            FileUtil.deleteDir(testDir);
        }

        testDir.mkdirs();

        return testDir;
    }

    /**
     * Attempts to find the specified resource on the classpath. First it
     * searches for the provided string resource path, if that fails, returns
     * the string prepended with "/res" since that is where the "res" resources
     * are placed in the jar files.
     * 
     * @param resourcePath
     * @return
     */
    public static String getResResourcePath(String resourcePath) {
        return JarUtil.getResResourcePath(resourcePath);
    }
}
