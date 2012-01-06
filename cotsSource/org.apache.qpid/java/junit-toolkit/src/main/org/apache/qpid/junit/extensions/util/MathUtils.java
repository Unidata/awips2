/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.junit.extensions.util;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Mathematical support methods for the toolkit. Caculating averages, variances, min/max for test latencies and
 * generating linear/exponential sequences for test size/concurrency ramping up.
 *
 * <p/>The sequence specifications are of the form [lowest(, ...)(, highest)](,sample=s)(,exp), where round brackets
 * enclose optional values. Using this pattern form it is possible to specify a single value, a range of values divided
 * into s samples, a range of values divided into s samples but distributed exponentially, or a fixed set of samples.
 *
 * <p/>The duration arguments are of the form (dD)(hH)(mM)(sS), where round brackets enclose optional values. At least
 * one of the optional values must be present.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Generate a sequene of integers from a sequence specification.
 * <tr><td> Parse an encoded duration into milliseconds.
 * </table>
 *
 * @author Rupert Smith
 */
public class MathUtils
{
    /** Used for debugging. */
    // private static final Logger log = Logger.getLogger(MathUtils.class);

    /** The sequence defintion matching regular expression. */
    public static final String SEQUENCE_REGEXP = "^(\\[[0-9:]+\\])(:samples=[0-9]+)?(:exp)?$";

    /** The regular expression that matches sequence definitions. */
    private static final Pattern SEQUENCE_PATTERN = Pattern.compile(SEQUENCE_REGEXP);

    /** The duration definition matching regular expression. */
    public static final String DURATION_REGEXP = "^(\\d+D)?(\\d+H)?(\\d+M)?(\\d+S)?$";

    /** The regular expression that matches the duration expression. */
    public static final Pattern DURATION_PATTERN = Pattern.compile(DURATION_REGEXP);

    /** For matching name=value pairs. */
    public static final String NAME_VALUE_REGEXP = "^\\w+=\\w+$";

    /** For matching name=[value1: value2: ...] variations. */
    public static final String NAME_VALUE_VARIATION_REGEXP = "^\\w+=\\[[\\w:]+\\]$";

    /** For matching name=[n: ... :m](:sample=s)(:exp) sequences. */
    public static final String NAME_VALUE_SEQUENCE_REGEXP = "^\\w+=(\\[[0-9:]+\\])(:samples=[0-9]+)?(:exp)?$";

    /** The regular expression that matches name=value pairs and variations. */
    public static final Pattern NAME_VALUE_PATTERN =
        Pattern.compile("(" + NAME_VALUE_REGEXP + ")|(" + NAME_VALUE_VARIATION_REGEXP + ")|(" + NAME_VALUE_SEQUENCE_REGEXP
            + ")");

    /**
     * Runs a quick test of the sequence generation methods to confirm that they work as expected.
     *
     * @param args The command line parameters.
     */
    public static void main(String[] args)
    {
        // Use the command line parser to evaluate the command line.
        CommandLineParser commandLine =
            new CommandLineParser(
                new String[][]
                {
                    { "s", "The sequence definition.", "[m:...:n](:sample=s)(:exp)", "true", MathUtils.SEQUENCE_REGEXP },
                    { "d", "The duration definition.", "dDhHmMsS", "false", MathUtils.DURATION_REGEXP }
                });

        // Capture the command line arguments or display errors and correct usage and then exit.
        ParsedProperties options = null;

        try
        {
            options = new ParsedProperties(commandLine.parseCommandLine(args));
        }
        catch (IllegalArgumentException e)
        {
            System.out.println(commandLine.getErrors());
            System.out.println(commandLine.getUsage());
            System.exit(-1);
        }

        // Extract the command line options.
        String sequence = options.getProperty("s");
        String durationString = options.getProperty("d");

        System.out.println("Sequence is: " + printArray(parseSequence(sequence)));

        if (durationString != null)
        {
            System.out.println("Duration is: " + parseDuration(durationString));
        }
    }

    /**
     * Given a start and end and a number of steps this method generates a sequence of evenly spaced integer
     * values, starting at the start (inclusive) and finishing at the end (inclusive) with the specified number
     * of values in the sequence. The sequence returned may contain less than the specified number where the integer
     * range between start and end is too small to contain that many.
     *
     * <p/>As the results are integers, they will not be perfectly evenly spaced but a best-fit.
     *
     * @param start The sequence start.
     * @param end   The sequence end.
     * @param steps The number of steps.
     *
     * @return The sequence.
     */
    public static int[] generateSequence(int start, int end, int steps)
    {
        // Check that there are at least two steps.
        if (steps < 2)
        {
            throw new IllegalArgumentException("There must be at least 2 steps.");
        }

        ArrayList<Integer> result = new ArrayList<Integer>();

        // Calculate the sequence using floating point, then round into the results.
        double fStart = start;
        double fEnd = end;
        double fCurrent = start;

        for (int i = 0; i < steps; i++)
        {
            fCurrent = (((fEnd - fStart) / (steps - 1)) * i) + fStart;

            roundAndAdd(result, fCurrent);
        }

        // Return the results after converting to a primitive array.
        return intListToPrimitiveArray(result);
    }

    /**
     * Given a start and end and a number of steps this method generates a sequence of expontentially spaced integer
     * values, starting at the start (inclusive) and finishing at the end (inclusive) with the specified number
     * of values in the sequence. An exponentially spaced sequence is one where the ratio between any two consecutive
     * numbers in the sequence remains constant. The sequence returned may contain less than the specified number where
     * the difference between two consecutive values is too small (this is more likely at the start of the sequence,
     * where the values are closer together).
     *
     * <p/>As the results are integers, they will not be perfectly exponentially spaced but a best-fit.
     *
     * @param start The sequence start.
     * @param end   The sequence end.
     * @param steps The number of steps.
     *
     * @return The sequence.
     */
    public static int[] generateExpSequence(int start, int end, int steps)
    {
        // Check that there are at least two steps.
        if (steps < 2)
        {
            throw new IllegalArgumentException("There must be at least 2 steps.");
        }

        ArrayList<Integer> result = new ArrayList<Integer>();

        // Calculate the sequence using floating point, then round into the results.
        double fStart = start;
        double fEnd = end;
        // float fCurrent = start;
        double diff = fEnd - fStart;
        double factor = java.lang.Math.pow(diff, (1.0f / (steps - 1)));

        for (int i = 0; i < steps; i++)
        {
            // This is a cheat to get the end exactly on and lose the accumulated rounding error.
            if (i == (steps - 1))
            {
                result.add(end);
            }
            else
            {
                roundAndAdd(result, fStart - 1.0f + java.lang.Math.pow(factor, i));
            }
        }

        // Return the results after converting to a primitive array.
        return intListToPrimitiveArray(result);
    }

    /**
     * Parses a string defintion of a sequence into an int array containing the sequence. The definition will conform
     * to the regular expression: "^(\[[0-9,]+\])(,samples=[0-9]+)?(,exp)?$". This splits it into three parts,
     * an array of integers, the optional sample count and the optional exponential flag.
     *
     * @param sequenceDef The sequence definition.
     *
     * @return The sequence as a fully expanded int array.
     */
    public static int[] parseSequence(String sequenceDef)
    {
        // Match the sequence definition against the regular expression for sequences.
        Matcher matcher = SEQUENCE_PATTERN.matcher(sequenceDef);

        // Check that the argument is of the right format accepted by this method.
        if (!matcher.matches())
        {
            throw new IllegalArgumentException("The sequence definition is not in the correct format.");
        }

        // Get the total number of matching groups to see if either of the optional samples or exponential flag
        // goups were set.
        int numGroups = matcher.groupCount();

        // Split the array of integers on commas.
        String intArrayString = matcher.group(1);

        String[] intSplits = intArrayString.split("[:\\[\\]]");

        int[] sequence = new int[intSplits.length - 1];

        for (int i = 1; i < intSplits.length; i++)
        {
            sequence[i - 1] = Integer.parseInt(intSplits[i]);
        }

        // Check for the optional samples count.
        int samples = 0;

        if ((numGroups > 1) && (matcher.group(2) != null))
        {
            String samplesGroup = matcher.group(2);

            String samplesString = samplesGroup.substring(",samples=".length());
            samples = Integer.parseInt(samplesString);
        }

        // Check for the optional exponential flag.
        boolean expFlag = false;

        if ((numGroups > 2) && (matcher.group(3) != null))
        {
            expFlag = true;
        }

        // If there is a sample count and 2 or more sequence values defined, then generate the sequence from the first
        // and last sequence values.
        if ((samples != 0) && (sequence.length >= 2))
        {
            int start = sequence[0];
            int end = sequence[sequence.length - 1];

            if (!expFlag)
            {
                sequence = generateSequence(start, end, samples);
            }
            else
            {
                sequence = generateExpSequence(start, end, samples);
            }
        }

        return sequence;
    }

    /**
     * Parses a duration defined as a string, giving a duration in days, hours, minutes and seconds into a number
     * of milliseconds equal to that duration.
     *
     * @param duration The duration definition string.
     *
     * @return The duration in millliseconds.
     */
    public static long parseDuration(String duration)
    {
        // Match the duration against the regular expression.
        Matcher matcher = DURATION_PATTERN.matcher(duration);

        // Check that the argument is of the right format accepted by this method.
        if (!matcher.matches())
        {
            throw new IllegalArgumentException("The duration definition is not in the correct format.");
        }

        // This accumulates the duration.
        long result = 0;

        int numGroups = matcher.groupCount();

        // Extract the days.
        if (numGroups >= 1)
        {
            String daysString = matcher.group(1);
            result +=
                (daysString == null)
                ? 0 : (Long.parseLong(daysString.substring(0, daysString.length() - 1)) * 24 * 60 * 60 * 1000);
        }

        // Extract the hours.
        if (numGroups >= 2)
        {
            String hoursString = matcher.group(2);
            result +=
                (hoursString == null) ? 0
                                      : (Long.parseLong(hoursString.substring(0, hoursString.length() - 1)) * 60 * 60 * 1000);
        }

        // Extract the minutes.
        if (numGroups >= 3)
        {
            String minutesString = matcher.group(3);
            result +=
                (minutesString == null)
                ? 0 : (Long.parseLong(minutesString.substring(0, minutesString.length() - 1)) * 60 * 1000);
        }

        // Extract the seconds.
        if (numGroups >= 4)
        {
            String secondsString = matcher.group(4);
            result +=
                (secondsString == null) ? 0 : (Long.parseLong(secondsString.substring(0, secondsString.length() - 1)) * 1000);
        }

        return result;
    }

    /**
     * Pretty prints an array of ints as a string.
     *
     * @param array The array to pretty print.
     *
     * @return The pretty printed string.
     */
    public static String printArray(int[] array)
    {
        String result = "[";
        for (int i = 0; i < array.length; i++)
        {
            result += array[i];
            result += (i < (array.length - 1)) ? ", " : "";
        }

        result += "]";

        return result;
    }

    /**
     * Returns the maximum value in an array of integers.
     *
     * @param values The array to find the amx in.
     *
     * @return The max value.
     */
    public static int maxInArray(int[] values)
    {
        if ((values == null) || (values.length == 0))
        {
            throw new IllegalArgumentException("Cannot find the max of a null or empty array.");
        }

        int max = values[0];

        for (int value : values)
        {
            max = (max < value) ? value : max;
        }

        return max;
    }

    /**
     * The #toArray methods of collections cannot be used with primitive arrays. This loops over and array list
     * of Integers and outputs and array of int.
     *
     * @param result The array of Integers to convert.
     *
     * @return An array of int.
     */
    private static int[] intListToPrimitiveArray(ArrayList<Integer> result)
    {
        int[] resultArray = new int[result.size()];
        int index = 0;
        for (int r : result)
        {
            resultArray[index] = result.get(index);
            index++;
        }

        return resultArray;
    }

    /**
     * Rounds the specified floating point value to the nearest integer and adds it to the specified list of
     * integers, provided it is not already in the list.
     *
     * @param result The list of integers to add to.
     * @param value  The new candidate to round and add to the list.
     */
    private static void roundAndAdd(ArrayList<Integer> result, double value)
    {
        int roundedValue = (int) Math.round(value);

        if (!result.contains(roundedValue))
        {
            result.add(roundedValue);
        }
    }
}
