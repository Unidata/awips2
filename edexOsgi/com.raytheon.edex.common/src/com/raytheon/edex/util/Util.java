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

package com.raytheon.edex.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.jar.JarFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;
import java.util.zip.ZipEntry;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ConfigurableApplicationContext;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ConvertUtil;

/**
 * Contains utility methods for use in common.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 14Jun2006    TO 3        G Armendariz Initial baseline
 * 02Jul2007    333         MW Fegan    Removed Lucene based methods.
 * 08/10/2007          379  jkorman     Added copyFile method. 
 * 10Apr2008    1068        MW Fegan    Remove redundant memory reporting.
 * 15Jul2008    1014        MW Fegan    Improved logging of JiBX marshaling errors.
 * Aug 20, 2008             dglazesk    Added functions for handling JaXB marshalling
 * Nov 09, 2012 1322        djohnson    Add close for Spring context.
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */
public final class Util {

    private static final NumberFormat milliFormatter = new DecimalFormat("000");

    private static final NumberFormat nanoFormatter = new DecimalFormat(
            "000000");

    // public static final String MEMORY_FORMAT = "Used Memory: %f MB, Free
    // Memory: %f MB, Heap Size: %f MB, Available: %f MB, Max Memory: %f MB";
    public static final String MEMORY_FORMAT = "Used Memory: %f MB, Heap Size: %f MB, Max Memory: %f MB";

    /*
     * constants used in adjusting observation dates
     */
    public static final long MILLI_PER_SECOND = 1000;

    public static final long MILLI_PER_HOUR = 3600000;

    public static final long MILLI_PER_TWENTY = 1200000;

    public static final long MILLI_PER_FOURTY = 2400000;

    public static Log logger = LogFactory.getLog(Util.class);

    /**
     * Easy reference to system-dependent end of line
     */
    public static final String EOL = System.getProperty("line.separator");

    public static final float GRID_FILL_VALUE = -999999;

    private Util() {
        // No Instantiation
    }

    /**
     * Simple check if str is null or empty.
     * 
     * @param str
     *            A string to check
     * @return true if string is null or empty, false otherwise
     */
    public static final boolean isEmptyString(String str) {
        return (str == null) || ("".equals(str));
    }

    /**
     * Wraps input Object.toString (if non-null) in [] for display (helps show
     * empty string in output).<br>
     * Example: printString("test") would display "[test]"<br>
     * printString(null) would display "[null]"
     * 
     * @param obj
     *            An object instance
     * @return The object's {@link Object#toString()} value
     */
    public static final String printString(Object obj) {
        return "[" + (obj == null ? "null" : obj.toString()) + "]";
    }

    /**
     * Invokes the setter for the specified field on an object.
     * <P>
     * The {@code objClass} argument could be eliminated by calling
     * {@link java.lang.Object#getClass()} on the object, but it is assumed this
     * method will only be used by a client using introspection, so the client
     * will already have created a {@link java.lang.Class} object. (If you know
     * what the object is, you can call the appropriate methods directly and do
     * not need this utility)
     * 
     * @param object
     *            the object on which to invoke the setter.
     * @param objClass
     *            the CLass object
     * @param field
     *            the field to be set
     * @param value
     *            the value to be set
     * 
     * @throws NoSuchMethodException
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     */
    public static void setFieldValue(Object object, Class<?> objClass,
            String field, Object value) throws NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {
        StringBuffer setter = new StringBuffer("set").append(
                field.substring(0, 1).toUpperCase()).append(field.substring(1));
        Method worker = objClass.getMethod(setter.toString(),
                new Class<?>[] { value.getClass() });
        worker.invoke(object, new Object[] { value });
    }

    /**
     * Get the filename from a fully qualified path
     * 
     * @param aFilePath
     * @return The filename
     */
    public static String getFileNameFromPath(String aFilePath) {
        String[] tokens;
        String fileName = null;

        // check for a valid file name
        if (aFilePath != null && !aFilePath.isEmpty()) {

            // split on the path separator to get the filename
            tokens = aFilePath.split("[\\\\,/]");
            fileName = tokens[tokens.length - 1];
        }

        return fileName;
    }

    /**
     * Converts a string in YYYYMMDDhhmmss format to a {@link Calendar}.
     * Milli-seconds are set to zero. If the input value is not a valid date
     * time string, the current time is returned.
     * 
     * @param date
     *            the formated date string
     * @return the calendar representing the date
     */
    public static GregorianCalendar convertStr14ToCal(String date) {
        Pattern p = Pattern
                .compile("(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})");
        Matcher m = p.matcher(date);

        // get a calender based on GMT timezone
        // all time stamp strings from the catalog query are in GMT
        GregorianCalendar calendar = new GregorianCalendar(
                TimeZone.getTimeZone("GMT"));

        if (m.matches()) {
            int year = Integer.parseInt(m.group(1));
            int month = Integer.parseInt(m.group(2)) - 1;
            int day = Integer.parseInt(m.group(3));
            int hour = Integer.parseInt(m.group(4));
            int min = Integer.parseInt(m.group(5));
            int sec = Integer.parseInt(m.group(6));
            calendar.set(year, month, day, hour, min, sec);
            calendar.set(Calendar.MILLISECOND, 0);
        }
        return calendar;
    }

    /**
     * Take in long representing nanoseconds, return formatted string.
     * 
     * @param nano
     *            number of nanoseconds to format
     * @return String representing seconds (ss.SSS:nnnnnn)
     */
    public static String nano2Seconds(long nano) {
        long seconds = nano / 1000000000;
        long millis = (nano - (seconds * 1000000000)) / 1000000;
        return seconds
                + "."
                + milliFormatter.format(millis)
                + ":"
                + nanoFormatter.format(nano - (seconds * 1000000000)
                        - (millis * 1000000)) + " s";
    }

    /**
     * Creates an array of URL objects from an array of File objects.
     * 
     * @param files
     *            An array of File objects
     * @return An array of URL objects created from the array of file locations
     */
    public static URL[] fileNameToURL(File[] files) {

        ArrayList<URL> list = new ArrayList<URL>();
        URL[] urls = null;

        if (files != null) {
            for (int counter = 0; counter < files.length; counter++) {
                try {
                    list.add(files[counter].toURI().toURL());
                } catch (MalformedURLException e) {
                    logger.error("Unable to create a URL for : "
                            + files[counter], e);
                } catch (Throwable e) {
                    logger.error("Unable to create a URL for : "
                            + files[counter], e);
                }
            }

            // finally, assign this list back
            urls = list.toArray(new URL[] {});
        }
        return urls;
    }

    /**
     * Creates an array of URL objects from an array of String objects.
     * 
     * @param theLocations
     * @return An array of URL objects created from the String array of
     *         locations
     */
    public static URL[] fileNameToURL(String[] theLocations) {
        ArrayList<URL> list = new ArrayList<URL>();
        URL[] urls = null;

        // if the locations value is null or empty,
        // return a list empty URLs
        if (theLocations == null || theLocations.length == 0) {
            urls = new URL[] {};
        } else {
            for (int counter = 0; counter < theLocations.length; counter++) {
                try {
                    list.add(new URL(theLocations[counter]));
                } catch (MalformedURLException e) {
                    logger.error("Unable to create a URL for : "
                            + theLocations[counter], e);
                } catch (Throwable e) {
                    logger.error("Unable to create a URL for : "
                            + theLocations[counter], e);
                }
            }

            // finally, assign this list back
            urls = list.toArray(new URL[] {});
        }

        return urls;
    }

    /**
     * Extracts a file as a string from a jar
     * 
     * @param jarFile
     *            The jar file to examine
     * @param fileName
     *            The name of the file within the jar to read
     * @return The string representation of the file
     */
    public static String getJarEntry(JarFile jarFile, String fileName) {

        InputStream jarStream = null;
        String fileContents = null;
        byte[] fileBytes = null;
        try {
            jarStream = jarFile.getInputStream(jarFile.getEntry(fileName));

            fileBytes = new byte[jarStream.available()];
            jarStream.read(fileBytes);
            fileContents = new String(fileBytes);

        } catch (IOException e) {
            logger.error("Unable to get input stream for jar entry: ["
                    + fileName + "]");

        } finally {
            if (jarStream != null) {
                try {
                    jarStream.close();
                } catch (IOException e) {
                    logger.error("Unable to close input stream to jar", e);
                }
            }
        }
        return fileContents;
    }

    /**
     * Creates a new class loaded by adding the specified list of files to the
     * existing class loader.
     * 
     * @param files
     *            the list of files to access with the class loader
     * 
     * @return the class loader for the listed files
     * 
     * @throws MalformedURLException
     */
    public static URLClassLoader getLibrariesClassLoader(File[] files)
            throws MalformedURLException {
        URL[] urls = new URL[files.length];

        // iterate through the list of files and get a URL for each
        for (int i = 0; i < files.length; i++) {
            // logger.debug("Utl: adding " + files[i].toString());
            urls[i] = files[i].toURI().toURL();
        }

        // finally, add the urls to the class loader and return
        return URLClassLoader.newInstance(urls, Thread.currentThread()
                .getContextClassLoader());

    }

    /**
     * Retrive an array of directories from a comma-separated string.
     * 
     * @param theLocationsString
     * @return An array of directories
     */
    public static String[] retrieveDirectories(String theLocationsString) {
        String[] directories = null;

        // check for an empty property list,
        // if found, return an empty directory list
        if (theLocationsString == null) {
            return new String[] {};
        }

        // split the list of locations on ,
        directories = theLocationsString.split(",");

        return directories;
    }

    /**
     * Create a temporary file from a byte array message inthe OS's temporary
     * directory This file is removed from the OS when the JVM terminates, but
     * you must specify {@link File#deleteOnExit()}
     * 
     * @param message
     *            The byte array to write
     * @return A file with this data
     */
    public static File createTempFile(byte[] message, String filePrefix) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream(message.length);
        File file = null;
        FileOutputStream fos = null;
        try {
            file = File.createTempFile(filePrefix, null);
            fos = new FileOutputStream(file);
            bos.write(message);
            bos.writeTo(fos);
        } catch (FileNotFoundException e) {
            logger.error("Unable to find file", e);
        } catch (IOException e) {
            logger.error("Unable to open file", e);
        } finally {
            if (bos != null) {
                try {
                    bos.close();
                } catch (IOException e) {
                    logger.error("Unable to close output stream");
                }
            }

            if (fos != null) {
                try {
                    fos.close();
                } catch (IOException e) {
                    logger.error("Unable to close input stream");
                }
            }
        }

        return file;
    }

    /**
     * Creates a temporary file in the default location named "tempFile" with
     * the provided data
     * 
     * @param message
     *            The data to write to the file
     * @return The created file
     */
    public static File createTempFile(byte[] message) {
        return createTempFile(message, "tempFile");
    }

    /**
     * Flip double array along a horizontal line.
     * 
     * @param baseArray
     * @return
     */
    public static double[][] flipHoriz(double[][] baseArray) {

        int y = baseArray.length;
        int x = baseArray[0].length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < y; i++) {
            for (int j = 0; j < x; j++) {
                returnVal[y - i - 1][j] = baseArray[i][j];
            }
        }

        return returnVal;
    }

    /**
     * Flip double array along a vertical line
     * 
     * @param baseArray
     * @return
     */
    public static double[][] flipVert(double[][] baseArray) {

        int y = baseArray.length;
        int x = baseArray[0].length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < y; i++) {
            for (int j = 0; j < x; j++) {
                returnVal[i][x - j - 1] = baseArray[i][j];
            }
        }

        return returnVal;
    }

    /**
     * Flip float array along a horizontal line.
     * 
     * @param baseArray
     * @return
     */
    public static void flipHoriz(float[][] baseArray) {

        float temp;
        int height = baseArray[0].length;
        int width = baseArray.length;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[i][j];
                baseArray[i][j] = baseArray[i][width - j - 1];
                baseArray[i][width - j - 1] = temp;
            }
        }
    }

    /**
     * Rotate 180 degrees.
     * 
     * @param baseArray
     * @return
     */
    public static void rotate180(float[] baseArray, int height, int width) {

        int counter = 0;
        int total = height * width;
        float temp;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[counter];
                baseArray[counter] = baseArray[total - counter - 1];
                baseArray[total - counter - 1] = temp;
                counter++;
            }
        }
    }

    /**
     * Flip along a vertical
     * 
     * @param baseArray
     * @return
     */
    public static void flipVert(float[] baseArray, int height, int width) {

        float temp;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[i * width + j];
                baseArray[i * width + j] = baseArray[i * width + width - j - 1];
                baseArray[i * width + width - j - 1] = temp;
            }
        }
    }

    /**
     * Flip float array along a vertical line.
     * 
     * @param baseArray
     * @return
     */
    public static void flipHoriz(float[] baseArray, int height, int width) {

        float temp = 0;

        for (int i = 0; i < height / 2; i++) {
            for (int j = 0; j < width; j++) {
                temp = baseArray[j + width * i];
                baseArray[j + width * i] = baseArray[j + width
                        * (height - i - 1)];
                baseArray[j + width * (height - i - 1)] = temp;
            }
        }
    }

    /**
     * Flip along a vertical
     * 
     * @param baseArray
     * @return
     */
    public static void flipVert(byte[] baseArray, int height, int width) {

        byte temp;

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width / 2; j++) {
                temp = baseArray[i * width + j];
                baseArray[i * width + j] = baseArray[i * width + width - j - 1];
                baseArray[i * width + width - j - 1] = temp;
            }
        }
    }

    /**
     * Flip float array along a vertical line.
     * 
     * @param baseArray
     * @return
     */
    public static void flipHoriz(byte[] baseArray, int height, int width) {

        byte temp = 0;

        for (int i = 0; i < height / 2; i++) {
            for (int j = 0; j < width; j++) {
                temp = baseArray[j + width * i];
                baseArray[j + width * i] = baseArray[j + width
                        * (height - i - 1)];
                baseArray[j + width * (height - i - 1)] = temp;
            }
        }
    }

    /**
     * Flip float array along a vertical line
     * 
     * @param baseArray
     * @return
     */
    public static float[][] flipVert(float[][] baseArray) {

        int y = baseArray.length;
        int x = baseArray[0].length;
        float[][] returnVal = new float[y][x];

        for (int i = 0; i < y; i++) {
            for (int j = 0; j < x; j++) {
                returnVal[i][x - j - 1] = baseArray[i][j];
            }
        }

        return returnVal;
    }

    /**
     * Rotate double array 90 degrees to the right
     * 
     * @param baseArray
     * @return
     */
    public static double[][] rotateRight(double[][] baseArray) {

        int y = baseArray[0].length;
        int x = baseArray.length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                returnVal[j][x - 1 - i] = baseArray[i][j];
            }
        }
        return returnVal;

    }

    /**
     * Rotate a double array 90 degrees to the left
     * 
     * @param baseArray
     * @return
     */
    public static double[][] rotateLeft(double[][] baseArray) {

        int y = baseArray[0].length;
        int x = baseArray.length;
        double[][] returnVal = new double[y][x];

        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                returnVal[y - j - 1][i] = baseArray[i][j];
            }
        }
        return returnVal;

    }

    /**
     * Converts a ddhhmm time group to a Calendar. Adjusts the calendar as
     * follows: Any time group with a day (dd) in the future is set back one
     * month.
     * 
     * @param baseTime
     *            the time to convert
     * 
     * @return the converted time
     * 
     * @throws DataFormatException
     *             if an error occurs
     */
    public static Calendar findCurrentTime(String baseTime)
            throws DataFormatException {
        Calendar retVal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        try {
            String regexe = "(\\d{2})(\\d{2})(\\d{2})[Zz]?";
            Pattern pattern = Pattern.compile(regexe);
            Matcher matcher = pattern.matcher(baseTime);
            if (matcher.matches()) {
                adjustDayHourMinute(retVal, matcher.group(1), matcher.group(2),
                        matcher.group(3));
            } else {
                throw new ParseException("Invalid format - does not match "
                        + regexe, 0);
            }
        } catch (Exception e) {
            throw new DataFormatException("Unable to find current time for "
                    + baseTime + ", exception was " + e.toString());
        }
        return retVal;
    }

    /**
     * Creates a reference time for the the specified base time. Reference times
     * are hh:00, hh:20, and hh:40. Times between fifteen minutes prior and five
     * minutes after the reference time are rounded to the rererence time.
     * 
     * @param baseTime
     *            the time to convert to reference time
     * 
     * @return the reference time
     * 
     * @throws DataFormatException
     *             if any problem occurs.
     */
    public static Calendar findReferenceTime(String baseTime)
            throws DataFormatException {
        Calendar retVal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        int minute = 0;
        try {
            String regexe = "(\\d{2})(\\d{2})(\\d{2})[Zz]?";
            Pattern pattern = Pattern.compile(regexe);
            Matcher matcher = pattern.matcher(baseTime);
            if (matcher.matches()) {
                minute = Integer.parseInt(matcher.group(3));
                adjustDayHourMinute(retVal, matcher.group(1), matcher.group(2),
                        matcher.group(3));
                // get the time and truncate to the previous hour
                long time = retVal.getTimeInMillis();
                time -= time % MILLI_PER_HOUR;

                if (minute < 5) {
                    // nothing to do, the time should be correct.
                } else if (minute < 25) {
                    // need to add 20 minutes
                    time += MILLI_PER_TWENTY;
                } else if (minute < 45) {
                    // need to add 40 minutes
                    time += MILLI_PER_FOURTY;
                } else {
                    // need to add 60 minutes
                    time += MILLI_PER_HOUR;
                }
                // reset the calendar to the reference time
                retVal.setTimeInMillis(time);
            } else {
                throw new ParseException("Invalid format - does not match "
                        + regexe, 0);
            }
        } catch (Exception e) {
            throw new DataFormatException("Unable to find reference time for "
                    + baseTime + ", exception was " + e.toString());
        }
        return retVal;
    }

    /**
     * Computes the reference hour for the specified time.
     * 
     * @param baseTime
     *            the time to convert to reference hour
     * 
     * @return the reference hour
     * 
     * @throws DataFormatException
     *             if any problem occurs.
     */
    public static Calendar findReferenceHour(String baseTime)
            throws DataFormatException {
        Calendar retVal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        int minute = 0;
        String regexe = "(\\d{2})(\\d{2})(\\d{2})[Zz]?";
        Pattern pattern = Pattern.compile(regexe);
        Matcher matcher = pattern.matcher(baseTime);
        try {
            if (matcher.matches()) {
                minute = Integer.parseInt(matcher.group(3));
                adjustDayHourMinute(retVal, matcher.group(1), matcher.group(2),
                        matcher.group(3));
                // get the time and truncate to the previous hour
                long time = retVal.getTimeInMillis();
                time -= time % MILLI_PER_HOUR;

                /*
                 * if past the cut point, round up to the next hour
                 */
                if (minute >= 45) {
                    time += MILLI_PER_HOUR;
                }
                // reset the calendar to the reference hour
                retVal.setTimeInMillis(time);
            } else {
                throw new ParseException("Invalid format - does not match "
                        + regexe, 0);
            }
        } catch (Exception e) {
            throw new DataFormatException("Unable to find reference hour for "
                    + baseTime + ", exception was " + e.toString());
        }
        return retVal;
    }

    /**
     * Adjusts the calendar from the current date to the specified date. If the
     * specified date is later than the current date, the calendar is "backed
     * up" one month. In addition, the second and millisecond fields are set to
     * zero.
     * 
     * @param cal
     *            the calendar to adjust
     * @param day
     *            the new day of month
     * @param hour
     *            the new hour of day
     * @param minute
     *            the new minute of the hour
     */
    private static void adjustDayHourMinute(Calendar cal, String day,
            String hour, String minute) {
        int iDay = Integer.parseInt(day);
        int iHour = Integer.parseInt(hour);
        int iMinute = Integer.parseInt(minute);
        int iMonth = cal.get(Calendar.MONTH);
        int iYear = cal.get(Calendar.YEAR);
        // adjust the month and year for roll-over situations
        if (iDay > cal.get(Calendar.DAY_OF_MONTH)) {
            iMonth--;
            if (iMonth < 0) {
                iMonth = Calendar.DECEMBER;
                iYear--;
            }
        }
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        cal.set(Calendar.YEAR, iYear);
        cal.set(Calendar.MONTH, iMonth);
        cal.set(Calendar.DAY_OF_MONTH, iDay);
        cal.set(Calendar.HOUR_OF_DAY, iHour);
        cal.set(Calendar.MINUTE, iMinute);

    }

    /**
     * Retrieve the contents of a resource in a jar file as a string
     * 
     * @param jarLocation
     *            The location of the jar (directory + jar name)
     * @param resourceName
     *            The resource to retrieve
     * @param resourceDirectory
     *            The dirctory of the resource in the jar, e.g. "res\"
     * @return The string contents of the resource
     */
    public static String getFileFromJar(String jarLocation,
            String resourceName, String resourceDirectory) {
        StringBuffer buffer = new StringBuffer();
        InputStream input = null;
        BufferedReader reader = null;
        JarFile jarFile = null;
        String line = null;

        try {
            jarFile = new JarFile(jarLocation);
            ZipEntry entry = jarFile.getEntry(resourceDirectory + resourceName);
            input = jarFile.getInputStream(entry);
            reader = new BufferedReader(new InputStreamReader(input));

            while ((line = reader.readLine()) != null) {
                buffer.append(line);
            }
        } catch (IOException e) {
            logger.error("Unable to open file", e);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    logger.error("Unable to close reader", e);
                }
            }

            if (input != null) {
                try {
                    input.close();
                } catch (IOException e) {
                    logger.error("Unable to close input stream", e);
                }
            }

            if (jarFile != null) {
                try {
                    jarFile.close();
                } catch (IOException e) {
                    logger.error("Unable to close jar file", e);
                }
            }

        }

        return buffer.toString();
    }

    /**
     * Converts a String into an (possibly numeric) Object. If the String parses
     * as an Integer, it will return the Integer object. If the String parses as
     * a Float, it will return the Float object. Otherwise, the original String
     * object is returned.
     * 
     * @param string
     *            the string to convert
     * 
     * @return the converted object
     */
    public static Object getObjForStr(String string, Class<?> aClass) {

        Object retValue = null;

        // convert to calendar, if the pattern matches
        if (string.matches("\\d{14}")) {
            retValue = convertStr14ToCal(string);
        } else if (string
                .matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}.0")) {
            retValue = ConvertUtil.convertObject(string, Calendar.class);
        }

        if (retValue != null) {
            return retValue;
        }
        // if there is a class, get the instance of it.
        if (aClass != null && string.indexOf(",") == -1) {

            try {

                if (Float.class.equals(aClass)) {
                    retValue = new Float(string);
                } else if (Integer.class.equals(aClass)) {
                    retValue = new Integer(string);
                }

            } catch (Exception e) {
                logger.error("Unable to instaniate: " + aClass, e);
            }

        } else {

            // try to get the instance based on try/catch
            try {
                retValue = Integer.parseInt(string);
            } catch (NumberFormatException e) {
                try {
                    retValue = Float.parseFloat(string);
                } catch (NumberFormatException e1) {
                    retValue = string;
                }
            }
        }

        return retValue;
    }

    /**
     * Copy a file from its location to another location.
     * 
     * @param copyFrom
     *            The file reference.
     * @param toDir
     *            Location where the file will be copied to.
     * @return String status information.
     */
    public static String copyFile(File copyFrom, String toDir) {
        String status = "";
        if (copyFrom.exists()) {
            if (copyFrom.isFile()) {
                File to = new File(toDir + copyFrom.getName());

                BufferedInputStream bis = null;
                BufferedOutputStream bos = null;
                try {
                    try {
                        bis = new BufferedInputStream(new FileInputStream(
                                copyFrom));
                        bos = new BufferedOutputStream(new FileOutputStream(to));

                        int i = 0;
                        while ((i = bis.read()) > -1) {
                            bos.write(i);
                        }
                    } catch (IOException ioe) {
                        status = ioe.toString();
                    }
                } finally {
                    String s = close(bis);
                    if (s != null) {
                        status += s;
                    }
                    s = close(bos);
                    if (s != null) {
                        status += s;
                    }
                }
            } else {
                status = "Is not a file reference";
            }
        } else {
            status = "File not found";
        }
        return status;
    }

    public static void copy(String fromFile, String toFile) throws IOException {

        FileInputStream fis = new FileInputStream(fromFile);
        FileOutputStream fos = new FileOutputStream(toFile);

        int b = 0;
        while ((b = fis.read()) != -1) {
            fos.write(b);
        }
        fis.close();
        fos.close();
    }

    public static void copy(File fromFile, File toFile) throws IOException {
        copy(fromFile.getAbsolutePath(), toFile.getAbsolutePath());
    }

    /**
     * Closes any closeable object.
     * 
     * @param c
     *            An closeable target.
     * @return Close status.
     */
    public static final String close(Closeable c) {
        String status = null;

        if (c != null) {
            try {
                c.close();
            } catch (IOException ioe) {
                status = ioe.toString();
            }
        }
        return status;
    }

    /**
     * Creates a hash number based on the toString values of all elements in the
     * supplied array
     * 
     * @param elements
     *            Elements that construct the hash number
     * @return A hash number
     */
    public static Integer generateHash(Object[] elements) {

        HashCodeBuilder hashBuilder = new HashCodeBuilder(17, 37);

        for (int i = 0; i < elements.length; i++) {
            hashBuilder.append(elements[i].toString());
        }
        return hashBuilder.toHashCode();
    }

    /**
     * Returns the unique ID associated with a cache key address
     * 
     * @param address
     *            An address of an element in the cache
     * @return The UUID associated with this address
     */
    public static String parseID(String address) {
        String id = address.split("[/\\|]")[1];

        return id;
    }

    /**
     * Retrieve a formatted string displaying current memory information.
     * 
     * String is in the format: Max Size: x Mb, Free Memory: z Mb
     * 
     * @return runtime memory information
     */
    public static String getCurrentMemory() {
        final double oneMeg = 1024.0 * 1024.0;
        double freeMemory = Runtime.getRuntime().freeMemory() / oneMeg;
        double heapSize = Runtime.getRuntime().totalMemory() / oneMeg;
        double maxSize = Runtime.getRuntime().maxMemory() / oneMeg;

        double usedSize = heapSize - freeMemory;
        return String.format(MEMORY_FORMAT, usedSize, heapSize, maxSize);
    }

    public static float getPercentFreeMemory() {
        long maxSize = Runtime.getRuntime().maxMemory();
        float available = (maxSize - (Runtime.getRuntime().totalMemory() - Runtime
                .getRuntime().freeMemory())) / (1024 * 1024f);
        float percentFree = (available / ((maxSize / 1024f) / 1024)) * 100;
        return percentFree;
    }

    /**
     * Determines if the given string is all alpha-numeric characters
     * 
     * @param str
     *            The string to test
     * @return True if the string is alpha-numeric
     */
    public static boolean isAlnum(String str) {
        int count = 0;
        Pattern pat = Pattern.compile("\\p{Alnum}");
        Matcher mat = pat.matcher(str);

        while (mat.find()) {
            count++;
        }

        if (count == str.length()) {
            return true;
        } else {
            return false;
        }

    }

    /**
     * Gets a java type based on a string representation
     * 
     * @param type
     *            The type as a string
     * @return The Class type
     */
    public static Class<?> getJavaType(String type) {
        if ("varchar".equals(type)) {
            return String.class;
        }
        if ("integer".equals(type)) {
            return Integer.class;
        }
        if ("float".equals(type)) {
            return Float.class;
        }
        if ("numeric".equals(type)) {
            return Double.class;
        }
        if ("timestamp with time zone".equals(type) || "timestamp".equals(type)) {
            return Calendar.class;
        }

        throw new IllegalArgumentException("Bad type: " + type);
    }

    /**
     * Return the JAXB global context
     * 
     * @return the jaxb global context
     * @throws JAXBException
     * 
     * @deprecated Please use {@link SerializationUtil} instead
     */
    @Deprecated
    public static JAXBContext getJaxbContext() throws JAXBException {
        return SerializationUtil.getJaxbContext();
    }

    /**
     * Instantiates an object from the XML representation in a string. Uses
     * JAXB.
     * 
     * @param xml
     *            The XML representation
     * @return A new instance from the XML representation
     * @throws JAXBException
     * @deprecated Please use {@link SerializationUtil} instead
     */
    @Deprecated
    public static Object unmarshalFromXml(String xml) throws JAXBException {
        return SerializationUtil.unmarshalFromXml(xml);
    }

    /**
     * Convert an instance of a class to an XML representation in a string. Uses
     * JAXB.
     * 
     * @param obj
     *            Object being marshalled
     * @return XML string representation of the object
     * @throws JAXBException
     * @deprecated Please use {@link SerializationUtil} instead
     */
    @Deprecated
    public static String marshalToXml(Object obj) throws JAXBException {
        return SerializationUtil.marshalToXml(obj);
    }

    /**
     * Convert an instance of a class to an XML representation and write XML to
     * file. Uses JAXB.
     * 
     * @param obj
     *            Object to be marshalled
     * @param filePath
     *            Path to the output file
     * @throws SerializationException
     * @deprecated Please use {@link SerializationUtil} instead
     */
    @Deprecated
    public static void jaxbMarshalToXmlFile(Object obj, String filePath)
            throws SerializationException {
        SerializationUtil.jaxbMarshalToXmlFile(obj, filePath);
    }

    /**
     * Instantiates an object from the XML representation in a File. Uses JAXB.
     * 
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     * @deprecated Please use {@link SerializationUtil} instead
     */
    @Deprecated
    public static Object jaxbUnmarshalFromXmlFile(String filePath)
            throws SerializationException {
        return SerializationUtil.jaxbUnmarshalFromXmlFile(filePath);
    }

    /**
     * Retrieve date as a long in the index standard format: yyyy-MM-dd
     * kk:mm:ss.SSS
     * 
     * @param time
     *            Time in seconds
     * @return A formatted date string from the time parameter
     */
    public static String formatDate(float time) {
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis((long) time * MILLI_PER_SECOND);
        return TimeUtil.formatCalendar(cal);
    }

    public static long getUnixTime(Date date) {
        if (date == null) {
            return 0;
        } else {
            return date.getTime() / 1000l;
        }
    }

    /**
     * Resizes a 1-D data array into a 2-D array based on the provided row and
     * column count
     * 
     * @param data
     *            The 1-D array of data
     * @param columnCount
     *            The number of columns to map the data to
     * @param rowCount
     *            The number of rows to map the data to
     * @return The 2-D array of data
     */
    public static float[][] resizeDataTo2D(float[] data, int columnCount,
            int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row][column] = data[row * columnCount + column];
            }
        }

        return newGrid;
    }

    /**
     * Resizes a 2-D array of data to a 1-D array
     * 
     * @param data
     *            The 2-D array of data
     * @param rowCount
     *            The number of rows in the 2-D data array
     * @param columnCount
     *            The number of columns in the 2-D data array
     * @return The 1-D array of data
     */
    public static float[] resizeDataTo1D(float[][] data, int rowCount,
            int columnCount) {
        float[] newGrid = new float[rowCount * columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row * columnCount + column] = data[row][column];
            }
        }
        return newGrid;
    }

    /**
     * Extracts a sub-grid of data from a 2-D array of data
     * 
     * @param data
     *            The 2-D array of data
     * @param startColumn
     *            The start column of the sub-grid
     * @param startRow
     *            The start row of the sub-grid
     * @param columnCount
     *            The number of columns in the sub-grid
     * @param rowCount
     *            The number of rows in the sub-grid
     * @return The sub-grid of data
     */
    public static float[][] subGrid(float[][] data, int startColumn,
            int startRow, int columnCount, int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        for (int row = startRow; row < rowCount + startRow; row++) {
            for (int column = startColumn; column < columnCount + startColumn; column++) {
                newGrid[row - startRow][column - startColumn] = data[row][column];
            }
        }
        return newGrid;
    }

    public static void insertSubgrid(float[][] data, float[][] subGrid,
            int startColumn, int startRow, int columnCount, int rowCount) {

        for (int row = startRow; row < rowCount + startRow; row++) {
            for (int column = startColumn; column < columnCount + startColumn; column++) {
                // allow data off the end to wrap to the other side this is only
                // makes sense for Lat Lon data that wraps around the world.
                int dataColumn = column % data[row].length;
                data[row][dataColumn] = subGrid[row - startRow][column
                        - startColumn];
            }
        }
    }

    /**
     * Performs a safe-close on a {@link ConfigurableApplicationContext}.
     * 
     * @param ctx
     *            the context
     */
    public static String close(final ConfigurableApplicationContext ctx) {
        // Just adapt to a normal Java closeable
        return close(new Closeable() {
            @Override
            public void close() throws IOException {
                if (ctx != null) {
                    ctx.close();
                }
            }
        });
}

}
