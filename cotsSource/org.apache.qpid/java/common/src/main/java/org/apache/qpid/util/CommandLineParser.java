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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.*;

/**
 * CommandLineParser provides a utility for specifying the format of a command line and parsing command lines to ensure
 * that they fit their specified format. A command line is made up of flags and options, both may be refered to as
 * options. A flag is an option that does not take an argument (specifying it means it has the value 'true' and not
 * specifying it means it has the value 'false'). Options must take arguments but they can be set up with defaults so
 * that they take a default value when not set. Options may be mandatory in wich case it is an error not to specify
 * them on the command line. Flags are never mandatory because they are implicitly set to false when not specified.
 *
 * <p/>Some example command lines are:
 *
 * <ul>
 * <li>This one has two options that expect arguments:
 * <pre>
 * cruisecontrol -configfile cruisecontrol.xml -port 9000
 * </pre>
 * <li>This has one no-arg flag and two 'free' arguments:
 * <pre>
 * zip -r project.zip project/*
 * </pre>
 * <li>This one concatenates multiple flags into a single block with only one '-':
 * <pre>
 * jar -tvf mytar.tar
 * </pre>
 *
 * <p/>The parsing rules are:
 *
 * <ol>
 * <li>Flags may be combined after a single '-' because they never take arguments. Normally such flags are single letter
 * flags but this is only a convention and not enforced. Flags of more than one letter are usually specified on their own.
 * <li>Options expecting arguments must always be on their own.
 * <li>The argument to an option may be seperated from it by whitespace or appended directly onto the option.
 * <li>The argument to an option may never begin with a '-' character.
 * <li>All other arguments not beginning with a '-' character are free arguments that do not belong to any option.
 * <li>The second or later of a set of duplicate or repeated flags are ignored.
 * <li>Options are matched up to the shortest matching option. This is because of the possibility of having no space
 * between an option and its argument. This rules out the possibility of using two options where one is an opening
 * substring of the other. For example, the options "foo" and "foobar" cannot be used on the same command line because
 * it is not possible to distinguish the argument "-foobar" from being the "foobar" option or the "foo" option with
 * the "bar" argument.
 * </ol>
 *
 * <p/>By default, unknown options are simply ignored if specified on the command line. This behaviour may be changed
 * so that the parser reports all unknowns as errors by using the {@link #setErrorsOnUnknowns} method.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Accept a command line specification.
 * <tr><td> Parse a command line into properties, validating it against its specification.
 * <tr><td> Report all errors between a command line and its specification.
 * <tr><td> Provide a formatted usage string for a command line.
 * <tr><td> Provide a formatted options in force string for a command line.
 * <tr><td> Allow errors on unknowns behaviour to be turned on or off.
 * </table>
 */
public class CommandLineParser
{
    /** Holds a mapping from command line option names to detailed information about those options. */
    private Map<String, CommandLineOption> optionMap = new HashMap<String, CommandLineOption>();

    /** Holds a list of parsing errors. */
    private List<String> parsingErrors = new ArrayList<String>();

    /** Holds the regular expression matcher to match command line options with. */
    private Matcher optionMatcher = null;

    /** Holds the parsed command line properties after parsing. */
    private Properties parsedProperties = null;

    /** Flag used to indicate that errors should be created for unknown options. False by default. */
    private boolean errorsOnUnknowns = false;

    /**
     * Creates a command line options parser from a command line specification. This is passed to this constructor
     * as an array of arrays of strings. Each array of strings specifies the command line for a single option. A static
     * array may therefore easily be used to configure the command line parser in a single method call with an easily
     * readable format.
     *
     * <p/>Each array of strings must be 2, 3, 4 or 5 elements long. If any of the last three elements are missing they
     * are assumed to be null. The elements specify the following parameters:
     * <ol>
     * <li>The name of the option without the leading '-'. For example, "file".  To specify the format of the 'free'
     *     arguments use the option names "1", "2", ... and so on.
     * <li>The option comment. A line of text describing the usage of the option. For example, "The file to be processed."
     * <li>The options argument. This is a very short description of the argument to the option, often a single word
     *     or a reminder as to the arguments format. When this element is null the option is a flag and does not
     *     accept any arguments. For example, "filename" or "(unix | windows)" or null. The actual text specified
     *     is only used to print in the usage message to remind the user of the usage of the option.
     * <li>The mandatory flag. When set to "true" an option must always be specified. Any other value, including null,
     *     means that the option is mandatory. Flags are always mandatory (see class javadoc for explanation of why) so
     *     this is ignored for flags.
     * <li>A regular expression describing the format that the argument must take. Ignored if null.
     * </ol>
     * <p/>An example call to this constructor is:
     *
     * <pre>
     * CommandLineParser commandLine = new CommandLineParser(
     *     new String[][] {{"file", "The file to be processed. ", "filename", "true"},
     *                     {"dir", "Directory to store results in. Current dir used if not set.", "out dir"},
     *                     {"os", "Operating system EOL format to use.", "(windows | unix)", null, "windows\|unix"},
     *                     {"v", "Verbose mode. Prints information about the processing as it goes."},
     *                     {"1", "The processing command to run.", "command", "true", "add\|remove\|list"}});
     * </pre>
     *
     * @param config The configuration as an array of arrays of strings.
     */
    public CommandLineParser(String[][] config)
    {
        // Loop through all the command line option specifications creating details for each in the options map.
        for (int i = 0; i < config.length; i++)
        {
            String[] nextOptionSpec = config[i];

            addOption(nextOptionSpec[0], nextOptionSpec[1], (nextOptionSpec.length > 2) ? nextOptionSpec[2] : null,
                (nextOptionSpec.length > 3) ? ("true".equals(nextOptionSpec[3]) ? true : false) : false,
                (nextOptionSpec.length > 4) ? nextOptionSpec[4] : null);
        }
    }

    /**
     * Lists all the parsing errors from the most recent parsing in a string.
     *
     * @return All the parsing errors from the most recent parsing.
     */
    public String getErrors()
    {
        // Return the empty string if there are no errors.
        if (parsingErrors.isEmpty())
        {
            return "";
        }

        // Concatenate all the parsing errors together.
        String result = "";

        for (String s : parsingErrors)
        {
            result += s;
        }

        return result;
    }

    /**
     * Lists the properties set from the most recent parsing or an empty string if no parsing has been done yet.
     *
     * @return The properties set from the most recent parsing or an empty string if no parsing has been done yet.
     */
    public String getOptionsInForce()
    {
        // Check if there are no properties to report and return and empty string if so.
        if (parsedProperties == null)
        {
            return "";
        }

        // List all the properties.
        String result = "Options in force:\n";

        for (Map.Entry<Object, Object> property : parsedProperties.entrySet())
        {
            result += property.getKey() + " = " + property.getValue() + "\n";
        }

        return result;
    }

    /**
     * Generates a usage string consisting of the name of each option and each options argument description and
     * comment.
     *
     * @return A usage string for all the options.
     */
    public String getUsage()
    {
        String result = "Options:\n";

        // Print usage on each of the command line options.
        for (CommandLineOption optionInfo : optionMap.values())
        {
            result +=
                optionInfo.option + " " + ((optionInfo.argument != null) ? (optionInfo.argument + " ") : "")
                + optionInfo.comment + "\n";
        }

        return result;
    }

    /**
     * Control the behaviour of the errors on unkowns reporting. When turned on this reports all unkowns options
     * as errors. When turned off, all unknowns are simply ignored.
     *
     * @param errors The setting of the errors on unkown flag. True to turn it on.
     */
    public void setErrorsOnUnknowns(boolean errors)
    {
        errorsOnUnknowns = errors;
    }

    /**
     * Parses a set of command line arguments into a set of properties, keyed by the argument flag. The free arguments
     * are keyed by integers as strings starting at "1" and then "2", ... and so on.
     *
     * <p/>See the class level comment for a description of the parsing rules.
     *
     * @param args The command line arguments.
     *
     * @return The arguments as a set of properties.
     *
     * @throws IllegalArgumentException If the command line cannot be parsed against its specification. If this exception
     *                                  is thrown a call to {@link #getErrors} will provide a diagnostic of the command
     *                                  line errors.
     */
    public Properties parseCommandLine(String[] args) throws IllegalArgumentException
    {
        Properties options = new Properties();

        // Used to keep count of the current 'free' argument.
        int free = 1;

        // Used to indicate that the most recently parsed option is expecting arguments.
        boolean expectingArgs = false;

        // The option that is expecting arguments from the next element of the command line.
        String optionExpectingArgs = null;

        // Used to indicate that the most recently parsed option is a duplicate and should be ignored.
        boolean ignore = false;

        // Create the regular expression matcher for the command line options.
        String regexp = "^(";
        int optionsAdded = 0;

        for (Iterator<String> i = optionMap.keySet().iterator(); i.hasNext();)
        {
            String nextOption = i.next();

            // Check that the option is not a free argument definition.
            boolean notFree = false;

            try
            {
                Integer.parseInt(nextOption);
            }
            catch (NumberFormatException e)
            {
                notFree = true;
            }

            // Add the option to the regular expression matcher if it is not a free argument definition.
            if (notFree)
            {
                regexp += nextOption + (i.hasNext() ? "|" : "");
                optionsAdded++;
            }
        }

        // There has to be more that one option in the regular expression or else the compiler complains that the close
        // cannot be nullable if the '?' token is used to make the matched option string optional.
        regexp += ")" + ((optionsAdded > 0) ? "?" : "") + "(.*)";
        Pattern pattern = Pattern.compile(regexp);

        // Loop through all the command line arguments.
        for (int i = 0; i < args.length; i++)
        {
            // Check if the next command line argument begins with a '-' character and is therefore the start of
            // an option.
            if (args[i].startsWith("-"))
            {
                // Extract the value of the option without the leading '-'.
                String arg = args[i].substring(1);

                // Match up to the longest matching option.
                optionMatcher = pattern.matcher(arg);
                optionMatcher.matches();

                String matchedOption = optionMatcher.group(1);

                // Match any argument directly appended onto the longest matching option.
                String matchedArg = optionMatcher.group(2);

                // Check that a known option was matched.
                if ((matchedOption != null) && !"".equals(matchedOption))
                {
                    // Get the command line option information for the matched option.
                    CommandLineOption optionInfo = optionMap.get(matchedOption);

                    // Check if this option is expecting arguments.
                    if (optionInfo.expectsArgs)
                    {
                        // The option is expecting arguments so swallow the next command line argument as an
                        // argument to this option.
                        expectingArgs = true;
                        optionExpectingArgs = matchedOption;

                        // In the mean time set this options argument to the empty string in case no argument is ever
                        // supplied.
                        // options.put(matchedOption, "");
                    }

                    // Check if the option was matched on its own and is a flag in which case set that flag.
                    if ("".equals(matchedArg) && !optionInfo.expectsArgs)
                    {
                        options.put(matchedOption, "true");
                    }
                    // The option was matched as a substring with its argument appended to it or is a flag that is
                    // condensed together with other flags.
                    else if (!"".equals(matchedArg))
                    {
                        // Check if the option is a flag and therefore is allowed to be condensed together
                        // with other flags.
                        if (!optionInfo.expectsArgs)
                        {
                            // Set the first matched flag.
                            options.put(matchedOption, "true");

                            // Repeat the longest matching process on the remainder but ensure that the remainder
                            // consists only of flags as only flags may be condensed together in this fashion.
                            do
                            {
                                // Match the remainder against the options.
                                optionMatcher = pattern.matcher(matchedArg);
                                optionMatcher.matches();

                                matchedOption = optionMatcher.group(1);
                                matchedArg = optionMatcher.group(2);

                                // Check that an option was matched.
                                if (matchedOption != null)
                                {
                                    // Get the command line option information for the next matched option.
                                    optionInfo = optionMap.get(matchedOption);

                                    // Ensure that the next option is a flag or raise an error if not.
                                    if (optionInfo.expectsArgs == true)
                                    {
                                        parsingErrors.add("Option " + matchedOption + " cannot be combined with flags.\n");
                                    }

                                    options.put(matchedOption, "true");
                                }
                                // The remainder could not be matched against a flag it is either an unknown flag
                                // or an illegal argument to a flag.
                                else
                                {
                                    parsingErrors.add("Illegal argument to a flag in the option " + arg + "\n");

                                    break;
                                }
                            }
                            // Continue until the remainder of the argument has all been matched with flags.
                            while (!"".equals(matchedArg));
                        }
                        // The option is expecting an argument, so store the unmatched portion against it
                        // as its argument.
                        else
                        {
                            // Check the arguments format is correct against any specified format.
                            checkArgumentFormat(optionInfo, matchedArg);

                            // Store the argument against its option (regardless of its format).
                            options.put(matchedOption, matchedArg);

                            // The argument to this flag has already been supplied to it. Do not swallow the
                            // next command line argument as an argument to this flag.
                            expectingArgs = false;
                        }
                    }
                }
                else // No matching option was found.
                {
                    // Add this to the list of parsing errors if errors on unkowns is being used.
                    if (errorsOnUnknowns)
                    {
                        parsingErrors.add("Option " + matchedOption + " is not a recognized option.\n");
                    }
                }
            }
            // The command line argument did not being with a '-' so it is an argument to the previous flag or it
            // is a free argument.
            else
            {
                // Check if a previous flag is expecting to swallow this next argument as its argument.
                if (expectingArgs)
                {
                    // Get the option info for the option waiting for arguments.
                    CommandLineOption optionInfo = optionMap.get(optionExpectingArgs);

                    // Check the arguments format is correct against any specified format.
                    checkArgumentFormat(optionInfo, args[i]);

                    // Store the argument against its option (regardless of its format).
                    options.put(optionExpectingArgs, args[i]);

                    // Clear the expecting args flag now that the argument has been swallowed.
                    expectingArgs = false;
                    optionExpectingArgs = null;
                }
                // This command line option is not an argument to any option. Add it to the set of 'free' options.
                else
                {
                    // Get the option info for the free option, if there is any.
                    CommandLineOption optionInfo = optionMap.get(Integer.toString(free));

                    if (optionInfo != null)
                    {
                        // Check the arguments format is correct against any specified format.
                        checkArgumentFormat(optionInfo, args[i]);
                    }

                    // Add to the list of free options.
                    options.put(Integer.toString(free), args[i]);

                    // Move on to the next free argument.
                    free++;
                }
            }
        }

        // Scan through all the specified options to check that all mandatory options have been set and that all flags
        // that were not set are set to false in the set of properties.
        for (CommandLineOption optionInfo : optionMap.values())
        {
            // Check if this is a flag.
            if (!optionInfo.expectsArgs)
            {
                // Check if the flag is not set in the properties and set it to false if so.
                if (!options.containsKey(optionInfo.option))
                {
                    options.put(optionInfo.option, "false");
                }
            }
            // Check if this is a mandatory option and was not set.
            else if (optionInfo.mandatory && !options.containsKey(optionInfo.option))
            {
                // Create an error for the missing option.
                parsingErrors.add("Option " + optionInfo.option + " is mandatory but not was not specified.\n");
            }
        }

        // Check if there were any errors.
        if (!parsingErrors.isEmpty())
        {
            // Throw an illegal argument exception to signify that there were parsing errors.
            throw new IllegalArgumentException();
        }

        // Convert any name/value pairs in the free arguments into properties in the parsed options.
        options = takeFreeArgsAsProperties(options, 1);

        parsedProperties = options;

        return options;
    }

    /**
     * If a command line has been parsed, calling this method sets all of its parsed options into the specified properties.
     */
    public void addCommandLineToProperties(Properties properties)
    {
        if (parsedProperties != null)
        {
            for (Object propKey : parsedProperties.keySet())
            {
                String name = (String) propKey;
                String value = parsedProperties.getProperty(name);

                properties.setProperty(name, value);
            }
        }
    }

    /**
     * Resets this command line parser after it has been used to parse a command line. This method will only need
     * to be called to use this parser a second time which is not likely seeing as a command line is usually only
     * specified once. However, it is exposed as a public method for the rare case where this may be done.
     *
     * <p/>Cleans the internal state of this parser, removing all stored errors and information about the options in
     * force.
     */
    public void reset()
    {
        parsingErrors = new ArrayList<String>();
        parsedProperties = null;
    }

    /**
     * Adds the option to list of available command line options.
     *
     * @param option       The option to add as an available command line option.
     * @param comment      A comment for the option.
     * @param argument     The text that appears after the option in the usage string.
     * @param mandatory    When true, indicates that this option is mandatory.
     * @param formatRegexp The format that the argument must take, defined as a regular expression.
     */
    protected void addOption(String option, String comment, String argument, boolean mandatory, String formatRegexp)
    {
        // Check if usage text has been set in which case this option is expecting arguments.
        boolean expectsArgs = ((argument == null) || argument.equals("")) ? false : true;

        // Add the option to the map of command line options.
        CommandLineOption opt = new CommandLineOption(option, expectsArgs, comment, argument, mandatory, formatRegexp);
        optionMap.put(option, opt);
    }

    /**
     * Converts the free arguments into property declarations. After parsing the command line the free arguments
     * are numbered from 1, such that the parsed properties contain values for the keys "1", "2", ... This method
     * converts any free arguments declared using the 'name=value' syntax into properties with key 'name', value
     * 'value'.
     *
     * <p/>For example the comand line:
     * <pre>
     * ... debug=true
     * </pre>
     *
     * <p/>After parsing has properties:
     * <pre>[[1, debug=true]]</pre>
     *
     * <p/>After applying this method the properties are:
     * <pre>[[1, debug=true], [debug, true]]</pre>
     *
     * @param properties The parsed command line properties.
     * @param from       The free argument index to convert to properties from.
     *
     * @return The parsed command line properties, with free argument name value pairs too.
     */
    private Properties takeFreeArgsAsProperties(Properties properties, int from)
    {
        for (int i = from; true; i++)
        {
            String nextFreeArg = properties.getProperty(Integer.toString(i));

            // Terminate the loop once all free arguments have been consumed.
            if (nextFreeArg == null)
            {
                break;
            }

            // Split it on the =, strip any whitespace and set it as a system property.
            String[] nameValuePair = nextFreeArg.split("=");

            if (nameValuePair.length == 2)
            {
                properties.setProperty(nameValuePair[0], nameValuePair[1]);
            }
        }

        return properties;
    }

    /**
     * Checks the format of an argument to an option against its specified regular expression format if one has
     * been set. Any errors are added to the list of parsing errors.
     *
     * @param optionInfo The command line option information for the option which is havings its argument checked.
     * @param matchedArg The string argument to the option.
     */
    private void checkArgumentFormat(CommandLineOption optionInfo, String matchedArg)
    {
        // Check if this option enforces a format for its argument.
        if (optionInfo.argumentFormatRegexp != null)
        {
            Pattern pattern = Pattern.compile(optionInfo.argumentFormatRegexp);
            Matcher argumentMatcher = pattern.matcher(matchedArg);

            // Check if the argument does not meet its required format.
            if (!argumentMatcher.matches())
            {
                // Create an error for this badly formed argument.
                parsingErrors.add("The argument to option " + optionInfo.option + " does not meet its required format.\n");
            }
        }
    }

    /**
     * Extracts all name=value pairs from the command line, sets them all as system properties and also returns
     * a map of properties containing them.
     *
     * @param args        The command line.
     * @param commandLine The command line parser.
     * @param properties  The properties object to inject all parsed properties into (optional may be <tt>null</tt>).
     *
     * @return A set of properties containing all name=value pairs from the command line.
     */
    public static Properties processCommandLine(String[] args, CommandLineParser commandLine, Properties properties)
    {
        // Capture the command line arguments or display errors and correct usage and then exit.
        Properties options = null;

        try
        {
            options = commandLine.parseCommandLine(args);

            // Add all the trailing command line options (name=value pairs) to system properties. They may be picked up
            // from there.
            commandLine.addCommandLineToProperties(properties);
        }
        catch (IllegalArgumentException e)
        {
            System.out.println(commandLine.getErrors());
            System.out.println(commandLine.getUsage());
            System.exit(1);
        }

        return options;
    }

    /**
     * Holds information about a command line options. This includes what its name is, whether or not it is a flag,
     * whether or not it is mandatory, what its user comment is, what its argument reminder text is and what its
     * regular expression format is.
     *
     * <p><table id="crc"><caption>CRC Card</caption>
     * <tr><th> Responsibilities <th> Collaborations
     * <tr><td> Hold details of a command line option.
     * </table>
     */
    protected class CommandLineOption
    {
        /** Holds the text for the flag to match this argument with. */
        public String option = null;

        /** Holds a string describing how to use this command line argument. */
        public String argument = null;

        /** Flag that determines whether or not this command line argument can take arguments. */
        public boolean expectsArgs = false;

        /** Holds a short comment describing what this command line argument is for. */
        public String comment = null;

        /** Flag that determines whether or not this is an mandatory command line argument. */
        public boolean mandatory = false;

        /** A regular expression describing what format the argument to this option muist have. */
        public String argumentFormatRegexp = null;

        /**
         * Create a command line option object that holds specific information about a command line option.
         *
         * @param option       The text that matches the option.
         * @param expectsArgs  Whether or not the option expects arguments. It is a flag if this is false.
         * @param comment      A comment explaining how to use this option.
         * @param argument     A short reminder of the format of the argument to this option/
         * @param mandatory    Set to true if this option is mandatory.
         * @param formatRegexp The regular expression that the argument to this option must meet to be valid.
         */
        public CommandLineOption(String option, boolean expectsArgs, String comment, String argument, boolean mandatory,
            String formatRegexp)
        {
            this.option = option;
            this.expectsArgs = expectsArgs;
            this.comment = comment;
            this.argument = argument;
            this.mandatory = mandatory;
            this.argumentFormatRegexp = formatRegexp;
        }
    }
}
