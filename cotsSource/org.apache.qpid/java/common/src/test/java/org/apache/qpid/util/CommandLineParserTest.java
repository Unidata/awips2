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

package org.apache.qpid.util;

import junit.framework.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Properties;

/**
 * Unit tests the {@link CommandLineParser} class.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Check that parsing a single flag works ok.
 * <tr><td> Check that parsing multiple flags condensed together works ok.
 * <tr><td> Check that parsing an option with a space between it and its argument works ok.
 * <tr><td> Check that parsing an option with no space between it and its argument works ok.
 * <tr><td> Check that parsing an option with specific argument format works ok.
 * <tr><td> Check that parsing an option with specific argument format fails on bad argument.
 * <tr><td> Check that parsing a flag condensed together with an option fails.
 * <tr><td> Check that parsing a free argument works ok.
 * <tr><td> Check that parsing a free argument with specific format works ok.
 * <tr><td> Check that parsing a free argument with specific format fails on bad argument.
 * <tr><td> Check that parsing a mandatory option works ok.
 * <tr><td> Check that parsing a mandatory free argument works ok.
 * <tr><td> Check that parsing a mandatory option fails when no option is set.
 * <tr><td> Check that parsing a mandatory free argument fails when no argument is specified.
 * <tr><td> Check that parsing an unknown option works when unknowns not errors.
 * <tr><td> Check that parsing an unknown flag fails when unknowns are to be reported as errors.
 * <tr><td> Check that parsing an unknown option fails when unknowns are to be reported as errors.
 * <tr><td> Check that get errors returns a string on errors.
 * <tr><td> Check that get errors returns an empty string on no errors.
 * <tr><td> Check that get usage returns a string.
 * <tr><td> Check that get options in force returns an empty string before parsing.
 * <tr><td> Check that get options in force return a non-empty string after parsing.
 * </table>
 */
public class CommandLineParserTest extends TestCase
{
    private static final Logger log = LoggerFactory.getLogger(CommandLineParserTest.class);

    public CommandLineParserTest(String name)
    {
        super(name);
    }

    /**
     * Compile all the tests for the default test implementation of a traversable state into a test suite.
     */
    public static Test suite()
    {
        // Build a new test suite
        TestSuite suite = new TestSuite("CommandLineParser Tests");

        // Add all the tests defined in this class (using the default constructor)
        suite.addTestSuite(CommandLineParserTest.class);

        return suite;
    }

    /** Check that get errors returns an empty string on no errors. */
    public void testGetErrorsReturnsEmptyStringOnNoErrors() throws Exception
    {
        // Create a command line parser for some flags and options.
        CommandLineParser parser =
            new CommandLineParser(
                new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Option 2.", "test" },
                    { "t3", "Test Option 3.", "test", "true" },
                    { "t4", "Test Option 4.", "test", null, "^test$" }
                });

        // Do some legal parsing.
        parser.parseCommandLine(new String[] { "-t1", "-t2test", "-t3test", "-t4test" });

        // Check that the get errors message returns an empty string.
        assertTrue("The errors method did not return an empty string.", "".equals(parser.getErrors()));
    }

    /** Check that get errors returns a string on errors. */
    public void testGetErrorsReturnsStringOnErrors() throws Exception
    {
        // Create a command line parser for some flags and options.
        CommandLineParser parser =
            new CommandLineParser(
                new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Option 2.", "test" },
                    { "t3", "Test Option 3.", "test", "true" },
                    { "t4", "Test Option 4.", "test", null, "^test$" }
                });

        try
        {
            // Do some illegal parsing.
            parser.parseCommandLine(new String[] { "-t1", "-t1t2test", "-t4fail" });
        }
        catch (IllegalArgumentException e)
        { }

        // Check that the get errors message returns a string.
        assertTrue("The errors method returned an empty string.",
            !((parser.getErrors() == null) || "".equals(parser.getErrors())));

    }

    /** Check that get options in force returns an empty string before parsing. */
    public void testGetOptionsInForceReturnsEmptyStringBeforeParsing() throws Exception
    {
        // Create a command line parser for some flags and options.
        CommandLineParser parser =
            new CommandLineParser(
                new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Option 2.", "test" },
                    { "t3", "Test Option 3.", "test", "true" },
                    { "t4", "Test Option 4.", "test", null, "^test$" }
                });

        // Check that the options in force method returns an empty string.
        assertTrue("The options in force method did not return an empty string.", "".equals(parser.getOptionsInForce()));
    }

    /** Check that get options in force return a non-empty string after parsing. */
    public void testGetOptionsInForceReturnsNonEmptyStringAfterParsing() throws Exception
    {
        // Create a command line parser for some flags and options.
        CommandLineParser parser =
            new CommandLineParser(
                new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Option 2.", "test" },
                    { "t3", "Test Option 3.", "test", "true" },
                    { "t4", "Test Option 4.", "test", null, "^test$" }
                });

        // Do some parsing.
        parser.parseCommandLine(new String[] { "-t1", "-t2test", "-t3test", "-t4test" });

        // Check that the options in force method returns a string.
        assertTrue("The options in force method did not return a non empty string.",
            !((parser.getOptionsInForce() == null) || "".equals(parser.getOptionsInForce())));
    }

    /** Check that get usage returns a string. */
    public void testGetUsageReturnsString() throws Exception
    {
        // Create a command line parser for some flags and options.
        CommandLineParser parser =
            new CommandLineParser(
                new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Option 2.", "test" },
                    { "t3", "Test Option 3.", "test", "true" },
                    { "t4", "Test Option 4.", "test", null, "^test$" }
                });

        // Check that the usage method returns a string.
        assertTrue("The usage method did not return a non empty string.",
            !((parser.getUsage() == null) || "".equals(parser.getUsage())));
    }

    /** Check that parsing multiple flags condensed together works ok. */
    public void testParseCondensedFlagsOk() throws Exception
    {
        // Create a command line parser for multiple flags.
        CommandLineParser parser =
            new CommandLineParser(
                new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Flag 2." },
                    { "t3", "Test Flag 3." }
                });

        // Parse a command line with the flags set and condensed together.
        Properties testProps = parser.parseCommandLine(new String[] { "-t1t2t3" });

        // Check that the flags were set in the parsed properties.
        assertTrue("The t1 flag was not \"true\", it was: " + testProps.get("t1"), "true".equals(testProps.get("t1")));
        assertTrue("The t2 flag was not \"true\", it was: " + testProps.get("t2"), "true".equals(testProps.get("t2")));
        assertTrue("The t3 flag was not \"true\", it was: " + testProps.get("t3"), "true".equals(testProps.get("t3")));
    }

    /** Check that parsing a flag condensed together with an option fails. */
    public void testParseFlagCondensedWithOptionFails() throws Exception
    {
        // Create a command line parser for a flag and an option.
        CommandLineParser parser =
            new CommandLineParser(new String[][]
                {
                    { "t1", "Test Flag 1." },
                    { "t2", "Test Option 2.", "test" }
                });

        // Check that the parser reports an error.
        boolean testPassed = false;

        try
        {
            // Parse a command line with the flag and option condensed together.
            Properties testProps = parser.parseCommandLine(new String[] { "-t1t2" });
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        assertTrue("IllegalArgumentException not thrown when a flag and option are condensed together.", testPassed);
    }

    /** Check that parsing a free argument with specific format fails on bad argument. */
    public void testParseFormattedFreeArgumentFailsBadArgument() throws Exception
    {
        // Create a command line parser for a formatted free argument.
        CommandLineParser parser =
            new CommandLineParser(new String[][]
                {
                    { "1", "Test Free Argument.", "test", null, "^test$" }
                });

        // Check that the parser signals an error for a badly formatted argument.
        boolean testPassed = false;

        try
        {
            // Parse a command line with this option set incorrectly.
            Properties testProps = parser.parseCommandLine(new String[] { "fail" });
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        assertTrue("IllegalArgumentException not thrown when a badly formatted argument was set.", testPassed);
    }

    /** Check that parsing a free argument with specific format works ok. */
    public void testParseFormattedFreeArgumentOk() throws Exception
    {
        // Create a command line parser for a formatted free argument.
        CommandLineParser parser =
            new CommandLineParser(new String[][]
                {
                    { "1", "Test Free Argument.", "test", null, "^test$" }
                });

        // Parse a command line with this argument set correctly.
        Properties testProps = parser.parseCommandLine(new String[] { "test" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The first free argument was not equal to \"test\" but was: " + testProps.get("1"),
            "test".equals(testProps.get("1")));
    }

    /** Check that parsing an option with specific argument format fails on bad argument. */
    public void testParseFormattedOptionArgumentFailsBadArgument() throws Exception
    {
        // Create a command line parser for a formatted option.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Option.", "test", null, "^test$" }
                });

        // Check that the parser signals an error for a badly formatted argument.
        boolean testPassed = false;

        try
        {
            // Parse a command line with this option set incorrectly.
            Properties testProps = parser.parseCommandLine(new String[] { "-t", "fail" });
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        assertTrue("IllegalArgumentException not thrown when a badly formatted argument was set.", testPassed);
    }

    /** Check that parsing an option with specific argument format works ok. */
    public void testParseFormattedOptionArgumentOk() throws Exception
    {
        // Create a command line parser for a formatted option.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Option.", "test", null, "^test$" }
                });

        // Parse a command line with this option set correctly.
        Properties testProps = parser.parseCommandLine(new String[] { "-t", "test" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The test option was not equal to \"test\" but was: " + testProps.get("t"),
            "test".equals(testProps.get("t")));
    }

    /** Check that parsing a free argument works ok. */
    public void testParseFreeArgumentOk() throws Exception
    {
        // Create a command line parser for a free argument.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "1", "Test Free Argument.", "test" }
                });

        // Parse a command line with this argument set.
        Properties testProps = parser.parseCommandLine(new String[] { "test" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The first free argument was not equal to \"test\" but was: " + testProps.get("1"),
            "test".equals(testProps.get("1")));
    }

    /** Check that parsing a mandatory option works ok. */
    public void testParseMandatoryOptionOk() throws Exception
    {
        // Create a command line parser for a mandatory option.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Option.", "test", "true" }
                });

        // Parse a command line with this option set correctly.
        Properties testProps = parser.parseCommandLine(new String[] { "-t", "test" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The test option was not equal to \"test\" but was: " + testProps.get("t"),
            "test".equals(testProps.get("t")));
    }

    /** Check that parsing a mandatory free argument works ok. */
    public void testParseMandatoryFreeArgumentOk() throws Exception
    {
        // Create a command line parser for a mandatory free argument.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "1", "Test Option.", "test", "true" }
                });

        // Parse a command line with this argument set.
        Properties testProps = parser.parseCommandLine(new String[] { "test" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The first free argument was not equal to \"test\" but was: " + testProps.get("1"),
            "test".equals(testProps.get("1")));
    }

    /** Check that parsing a mandatory free argument fails when no argument is specified. */
    public void testParseManadatoryFreeArgumentFailsNoArgument() throws Exception
    {
        // Create a command line parser for a mandatory free argument.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "1", "Test Option.", "test", "true" }
                });

        // Check that parsing fails when this mandatory free argument is missing.
        boolean testPassed = false;

        try
        {
            // Parse a command line with this free argument not set.
            Properties testProps = parser.parseCommandLine(new String[] {});
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("IllegalArgumentException not thrown for a missing mandatory option.", testPassed);
    }

    /** Check that parsing a mandatory option fails when no option is set. */
    public void testParseMandatoryFailsNoOption() throws Exception
    {
        // Create a command line parser for a mandatory option.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Option.", "test", "true" }
                });

        // Check that parsing fails when this mandatory option is missing.
        boolean testPassed = false;

        try
        {
            // Parse a command line with this option not set.
            Properties testProps = parser.parseCommandLine(new String[] {});
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("IllegalArgumentException not thrown for a missing mandatory option.", testPassed);
    }

    /** Check that parsing an option with no space between it and its argument works ok. */
    public void testParseOptionWithNoSpaceOk() throws Exception
    {
        // Create a command line parser for an option.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Option.", "test" }
                });

        // Parse a command line with this option set with no space.
        Properties testProps = parser.parseCommandLine(new String[] { "-ttest" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The test option was not equal to \"test\" but was: " + testProps.get("t"),
            "test".equals(testProps.get("t")));
    }

    /** Check that parsing an option with a space between it and its argument works ok. */
    public void testParseOptionWithSpaceOk() throws Exception
    {
        // Create a command line parser for an option.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Option.", "test" }
                });

        // Parse a command line with this option set with a space.
        Properties testProps = parser.parseCommandLine(new String[] { "-t", "test" });

        // Check that the resultant properties contains the correctly parsed option.
        assertTrue("The test option was not equal to \"test\" but was: " + testProps.get("t"),
            "test".equals(testProps.get("t")));
    }

    /** Check that parsing a single flag works ok. */
    public void testParseSingleFlagOk() throws Exception
    {
        // Create a command line parser for a single flag.
        CommandLineParser parser = new CommandLineParser(new String[][]
                {
                    { "t", "Test Flag." }
                });

        // Parse a command line with the single flag set.
        Properties testProps = parser.parseCommandLine(new String[] { "-t" });

        // Check that the flag is set in the parsed properties.
        assertTrue("The t flag was not \"true\", it was: " + testProps.get("t"), "true".equals(testProps.get("t")));

        // Reset the parser.
        parser.reset();

        // Parse a command line with the single flag not set.
        testProps = parser.parseCommandLine(new String[] {});

        // Check that the flag is cleared in the parsed properties.
        assertTrue("The t flag was not \"false\", it was: " + testProps.get("t"), "false".equals(testProps.get("t")));
    }

    /** Check that parsing an unknown option works when unknowns not errors. */
    public void testParseUnknownOptionOk() throws Exception
    {
        // Create a command line parser for no flags or options
        CommandLineParser parser = new CommandLineParser(new String[][] {});

        // Check that parsing does not fail on an unknown flag.
        try
        {
            parser.parseCommandLine(new String[] { "-t" });
        }
        catch (IllegalArgumentException e)
        {
            fail("The parser threw an IllegalArgumentException on an unknown flag when errors on unkowns is off.");
        }
    }

    /** Check that parsing an unknown flag fails when unknowns are to be reported as errors. */
    public void testParseUnknownFlagFailsWhenUnknownsAreErrors() throws Exception
    {
        // Create a command line parser for no flags or options
        CommandLineParser parser = new CommandLineParser(new String[][] {});

        // Turn on fail on unknowns mode.
        parser.setErrorsOnUnknowns(true);

        // Check that parsing fails on an unknown flag.
        boolean testPassed = false;

        try
        {
            parser.parseCommandLine(new String[] { "-t" });
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        assertTrue("IllegalArgumentException not thrown for an unknown flag when errors on unknowns mode is on.",
            testPassed);
    }

    /** Check that parsing an unknown option fails when unknowns are to be reported as errors. */
    public void testParseUnknownOptionFailsWhenUnknownsAreErrors() throws Exception
    {
        // Create a command line parser for no flags or options
        CommandLineParser parser = new CommandLineParser(new String[][] {});

        // Turn on fail on unknowns mode.
        parser.setErrorsOnUnknowns(true);

        // Check that parsing fails on an unknown flag.
        boolean testPassed = false;

        try
        {
            parser.parseCommandLine(new String[] { "-t", "test" });
        }
        catch (IllegalArgumentException e)
        {
            testPassed = true;
        }

        assertTrue("IllegalArgumentException not thrown for an unknown option when errors on unknowns mode is on.",
            testPassed);
    }
}
