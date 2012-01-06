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
package org.apache.qpid.junit.extensions;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.framework.TestSuite;

import org.apache.log4j.Logger;

import org.apache.qpid.junit.extensions.listeners.CSVTestListener;
import org.apache.qpid.junit.extensions.listeners.ConsoleTestListener;
import org.apache.qpid.junit.extensions.listeners.XMLTestListener;
import org.apache.qpid.junit.extensions.util.CommandLineParser;
import org.apache.qpid.junit.extensions.util.MathUtils;
import org.apache.qpid.junit.extensions.util.ParsedProperties;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

import java.io.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

/**
 * TKTestRunner extends {@link junit.textui.TestRunner} with the ability to run tests multiple times, to execute a test
 * simultaneously using many threads, to put a delay between test runs and adds support for tests that take integer
 * parameters that can be 'stepped' through on multiple test runs. These features can be accessed by using this class
 * as an entry point and passing command line arguments to specify which features to use:
 *
 * <pre>
 * -w ms       The number of milliseconds between invocations of test cases.
 * -c pattern  The number of tests to run concurrently.
 * -r num      The number of times to repeat each test.
 * -d duration The length of time to run the tests for.
 * -t name     The name of the test case to execute.
 * -s pattern  The size parameter to run tests with.
 * -o dir      The name of the directory to output test timings to.
 * --csv       Output test results in CSV format.
 * --xml       Output test results in XML format.
 * </pre>
 *
 * <p/>This command line may also have trailing 'name=value' parameters added to it. All of these values are added
 * to the test context properties and passed to the test, which can access them by name.
 *
 * <p/>The pattern arguments are of the form [lowest(: ...)(: highest)](:sample=s)(:exp), where round brackets
 * enclose optional values. Using this pattern form it is possible to specify a single value, a range of values divided
 * into s samples, a range of values divided into s samples but distributed exponentially, or a fixed set of samples.
 *
 * <p/>The duration arguments are of the form (dD)(hH)(mM)(sS), where round brackets enclose optional values. At least
 * one of the optional values must be present.
 *
 * <p/>When specifying optional test parameters on the command line, in 'name=value' format, it is also possible to use
 * the format 'name=[value1:value2:value3:...]', to specify multiple values for a parameter. All permutations of all
 * parameters with multiple values will be created and tested. If the values are numerical, it is also possible to use
 * the sequence generation patterns instead of fully specifying all of the values.
 *
 * <p/>Here are some examples:
 *
 * <p/><table>
 * <tr><td><pre> -c [10:20:30:40:50] </pre><td> Runs the test with 10,20,...,50 threads.
 * <tr><td><pre> -s [1:100]:samples=10 </pre>
 *     <td> Runs the test with ten different size parameters evenly spaced between 1 and 100.
 * <tr><td><pre> -s [1:1000000]:samples=10:exp </pre>
 *     <td> Runs the test with ten different size parameters exponentially spaced between 1 and 1000000.
 * <tr><td><pre> -r 10 </pre><td> Runs each test ten times.
 * <tr><td><pre> -d 10H </pre><td> Runs the test repeatedly for 10 hours.
 * <tr><td><pre> -d 1M, -r 10 </pre>
 *     <td> Runs the test repeatedly for 1 minute but only takes a timing sample every 10 test runs.
 * <tr><td><pre> -r 10, -c [1:5:10:50], -s [100:1000:10000] </pre>
 *     <td> Runs 12 test cycles (4 concurrency samples * 3 size sample), with 10 repeats each. In total the test
 *          will be run 199 times (3 + 15 + 30 + 150)
 * <tr><td><pre> cache=true </pre><td> Passes the 'cache' parameter with value 'true' to the test.
 * <tr><td><pre> cache=[true:false] </pre><td> Runs the test with the 'cache' parameter set to 'true' and 'false'.
 * <tr><td><pre> cacheSize=[1000:1000000],samples=4,exp </pre>
 *     <td> Runs the test with the 'cache' parameter set to a series of exponentially increasing sizes.
 * </table>
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create the test configuration specified by the command line parameters.
 * </table>
 *
 * @todo Verify that the output directory exists or can be created.
 *
 * @todo Verify that the specific named test case to execute exists.
 *
 * @todo Drop the delay parameter as it is being replaced by throttling.
 *
 * @todo Completely replace the test ui test runner, instead of having TKTestRunner inherit from it, its just not
 *       good code to extend.
 *
 * @author Rupert Smith
 */
public class TKTestRunner extends TestRunnerImprovedErrorHandling
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(TKTestRunner.class);

    /** Used for displaying information on the console. */
    // private static final Logger console = Logger.getLogger("CONSOLE." + TKTestRunner.class.getName());

    /** Used for generating the timestamp when naming output files. */
    protected static final DateFormat TIME_STAMP_FORMAT = new SimpleDateFormat("yyyy-MM-dd-HH.mm.ss");

    /** Number of times to rerun the test. */
    protected Integer repetitions = 1;

    /** The length of time to run the tests for. */
    protected Long duration;

    /** Number of threads running the tests. */
    protected int[] threads;

    /** Delay in ms to wait between two test cases. */
    protected int delay = 0;

    /** The parameter values to pass to parameterized tests. */
    protected int[] params;

    /** Name of the single test case to execute. */
    protected String testCaseName = null;

    /** Name of the test class. */
    protected String testClassName = null;

    /** Name of the test run. */
    protected String testRunName = null;

    /** Directory to output XML reports into, if specified. */
    protected String reportDir = null;

    /** Flag that indicates the CSV results listener should be used to output results. */
    protected boolean csvResults;

    /** Flag that indiciates the XML results listener should be used to output results. */
    protected boolean xmlResults;

    /**
     * Holds the name of the class of the test currently being run. Ideally passed into the {@link #createTestResult}
     * method, but as the signature is already fixed for this, the current value gets pushed here as a member variable.
     */
    protected String currentTestClassName;

    /** Holds the test results object, which is reponsible for instrumenting tests/threads to record results. */
    protected TKTestResult result;

    /** Holds a list of factories for instantiating optional user specified test decorators. */
    protected List<TestDecoratorFactory> decoratorFactories;

    /**
     * Constructs a TKTestRunner using System.out for all the output.
     *
     * @param repetitions        The number of times to repeat the test, or test batch size.
     * @param duration           The length of time to run the tests for. -1 means no duration has been set.
     * @param threads            The concurrency levels to ramp up to.
     * @param delay              A delay in milliseconds between test runs.
     * @param params             The sets of 'size' parameters to pass to test.
     * @param testCaseName       The name of the test case to run.
     * @param reportDir          The directory to output the test results to.
     * @param runName            The name of the test run; used to name the output file.
     * @param csvResults         <tt>true</tt> if the CSV results listener should be attached.
     * @param xmlResults         <tt>true</tt> if the XML results listener should be attached.
     * @param decoratorFactories List of factories for user specified decorators.
     */
    public TKTestRunner(Integer repetitions, Long duration, int[] threads, int delay, int[] params, String testCaseName,
        String reportDir, String runName, boolean csvResults, boolean xmlResults,
        List<TestDecoratorFactory> decoratorFactories)
    {
        super(new NullResultPrinter(System.out));

        log.debug("public TKTestRunner(): called");

        // Keep all the test parameters.
        this.repetitions = repetitions;
        this.duration = duration;
        this.threads = threads;
        this.delay = delay;
        this.params = params;
        this.testCaseName = testCaseName;
        this.reportDir = reportDir;
        this.testRunName = runName;
        this.csvResults = csvResults;
        this.xmlResults = xmlResults;
        this.decoratorFactories = decoratorFactories;
    }

    /**
     * The entry point for the toolkit test runner.
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args)
    {
        // Use the command line parser to evaluate the command line.
        CommandLineParser commandLine =
            new CommandLineParser(
                new String[][]
                {
                    { "w", "The number of milliseconds between invocations of test cases.", "ms", "false" },
                    { "c", "The number of tests to run concurrently.", "num", "false", MathUtils.SEQUENCE_REGEXP },
                    { "r", "The number of times to repeat each test.", "num", "false" },
                    { "d", "The length of time to run the tests for.", "duration", "false", MathUtils.DURATION_REGEXP },
                    { "f", "The maximum rate to call the tests at.", "frequency", "false", "^([1-9][0-9]*)/([1-9][0-9]*)$" },
                    { "s", "The size parameter to run tests with.", "size", "false", MathUtils.SEQUENCE_REGEXP },
                    { "t", "The name of the test case to execute.", "name", "false" },
                    { "o", "The name of the directory to output test timings to.", "dir", "false" },
                    { "n", "A name for this test run, used to name the output file.", "name", "true" },
                    {
                        "X:decorators", "A list of additional test decorators to wrap the tests in.",
                        "\"class.name[:class.name]*\"", "false"
                    },
                    { "1", "Test class.", "class", "true" },
                    { "-csv", "Output test results in CSV format.", null, "false" },
                    { "-xml", "Output test results in XML format.", null, "false" }
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
            System.exit(FAILURE_EXIT);
        }

        // Extract the command line options.
        Integer delay = options.getPropertyAsInteger("w");
        String threadsString = options.getProperty("c");
        Integer repetitions = options.getPropertyAsInteger("r");
        String durationString = options.getProperty("d");
        String paramsString = options.getProperty("s");
        String testCaseName = options.getProperty("t");
        String reportDir = options.getProperty("o");
        String testRunName = options.getProperty("n");
        String decorators = options.getProperty("X:decorators");
        String testClassName = options.getProperty("1");
        boolean csvResults = options.getPropertyAsBoolean("-csv");
        boolean xmlResults = options.getPropertyAsBoolean("-xml");

        int[] threads = (threadsString == null) ? null : MathUtils.parseSequence(threadsString);
        int[] params = (paramsString == null) ? null : MathUtils.parseSequence(paramsString);
        Long duration = (durationString == null) ? null : MathUtils.parseDuration(durationString);

        // The test run name defaults to the test class name unless a value was specified for it.
        testRunName = (testRunName == null) ? testClassName : testRunName;

        // Add all the command line options and trailing settings to test context properties. Tests may pick up
        // overridden values from there, and these values will be logged in the test results, for analysis and
        // to make tests repeatable.
        commandLine.addTrailingPairsToProperties(TestContextProperties.getInstance());
        commandLine.addOptionsToProperties(TestContextProperties.getInstance());

        // Create and start the test runner.
        try
        {
            // Create a list of test decorator factories for use specified decorators to be applied.
            List<TestDecoratorFactory> decoratorFactories = parseDecorators(decorators);

            TKTestRunner testRunner =
                new TKTestRunner(repetitions, duration, threads, (delay == null) ? 0 : delay, params, testCaseName,
                    reportDir, testRunName, csvResults, xmlResults, decoratorFactories);

            TestResult testResult = testRunner.start(testClassName);

            if (!testResult.wasSuccessful())
            {
                System.exit(FAILURE_EXIT);
            }
        }
        catch (Exception e)
        {
            System.err.println(e.getMessage());
            e.printStackTrace(new PrintStream(System.err));
            System.exit(EXCEPTION_EXIT);
        }
    }

    /**
     * Parses a list of test decorators, in the form "class.name[:class.name]*", and creates factories for those
     * TestDecorator classes , and returns a list of the factories. This list of factories will be in the same
     * order as specified in the string. The factories can be used to succesively wrap tests in layers of
     * decorators, as decorators themselves implement the 'Test' interface.
     *
     * <p/>If the string fails to parse, or if any of the decorators specified in it are cannot be loaded, or are not
     * TestDecorators, a runtime exception with a suitable error message will be thrown. The factories themselves
     * throw runtimes if the constructor method calls on the decorators fail.
     *
     * @param decorators The decorators list to be parsed.
     *
     * @return A list of instantiated decorators.
     */
    protected static List<TestDecoratorFactory> parseDecorators(String decorators)
    {
        List<TestDecoratorFactory> result = new LinkedList<TestDecoratorFactory>();
        String toParse = decorators;

        // Check that the decorators string is not null or empty, returning an empty list of decorator factories it
        // it is.
        if ((decorators == null) || "".equals(decorators))
        {
            return result;
        }

        // Strip any leading and trailing quotes from the string.
        if (toParse.charAt(0) == '\"')
        {
            toParse = toParse.substring(1, toParse.length() - 1);
        }

        if (toParse.charAt(toParse.length() - 1) == '\"')
        {
            toParse = toParse.substring(0, toParse.length() - 2);
        }

        // Instantiate all decorators.
        for (String decoratorClassName : toParse.split(":"))
        {
            try
            {
                Class decoratorClass = Class.forName(decoratorClassName);
                final Constructor decoratorConstructor = decoratorClass.getConstructor(WrappedSuiteTestDecorator.class);

                // Check that the decorator is an instance of WrappedSuiteTestDecorator.
                if (!WrappedSuiteTestDecorator.class.isAssignableFrom(decoratorClass))
                {
                    throw new RuntimeException("The decorator class " + decoratorClassName
                        + " is not a sub-class of WrappedSuiteTestDecorator, which it needs to be.");
                }

                result.add(new TestDecoratorFactory()
                    {
                        public WrappedSuiteTestDecorator decorateTest(Test test)
                        {
                            try
                            {
                                return (WrappedSuiteTestDecorator) decoratorConstructor.newInstance(test);
                            }
                            catch (InstantiationException e)
                            {
                                throw new RuntimeException(
                                    "The decorator class " + decoratorConstructor.getDeclaringClass().getName()
                                    + " cannot be instantiated.", e);
                            }
                            catch (IllegalAccessException e)
                            {
                                throw new RuntimeException(
                                    "The decorator class " + decoratorConstructor.getDeclaringClass().getName()
                                    + " does not have a publicly accessable constructor.", e);
                            }
                            catch (InvocationTargetException e)
                            {
                                throw new RuntimeException(
                                    "The decorator class " + decoratorConstructor.getDeclaringClass().getName()
                                    + " cannot be invoked.", e);
                            }
                        }
                    });
            }
            catch (ClassNotFoundException e)
            {
                throw new RuntimeException("The decorator class " + decoratorClassName + " could not be found.", e);
            }
            catch (NoSuchMethodException e)
            {
                throw new RuntimeException("The decorator class " + decoratorClassName
                    + " does not have a constructor that accepts a single 'WrappedSuiteTestDecorator' argument.", e);
            }
        }

        return result;
    }

    /**
     * TestDecoratorFactory is a factory for creating test decorators from tests.
     */
    protected interface TestDecoratorFactory
    {
        /**
         * Decorates the specified test with a new decorator.
         *
         * @param test The test to decorate.
         *
         * @return The decorated test.
         */
        public WrappedSuiteTestDecorator decorateTest(Test test);
    }

    /**
     * Runs a test or suite of tests, using the super class implemenation. This method wraps the test to be run
     * in any test decorators needed to add in the configured toolkits enhanced junit functionality.
     *
     * @param test The test to run.
     * @param wait Undocumented. Nothing in the JUnit javadocs to say what this is for.
     *
     * @return The results of the test run.
     */
    public TestResult doRun(Test test, boolean wait)
    {
        log.debug("public TestResult doRun(Test \"" + test + "\", boolean " + wait + "): called");

        // Wrap the tests in decorators for duration, scaling, repetition, parameterization etc.
        WrappedSuiteTestDecorator targetTest = decorateTests(test);

        // Delegate to the super method to run the decorated tests.
        log.debug("About to call super.doRun");

        TestResult result = super.doRun(targetTest, wait);
        log.debug("super.doRun returned.");

        /*if (result instanceof TKTestResult)
        {
            TKTestResult tkResult = (TKTestResult) result;

            tkResult.notifyEndBatch();
        }*/

        return result;
    }

    /**
     * Applies test decorators to the tests for parameterization, duration, scaling and repetition.
     *
     * @param test The test to decorat.
     *
     * @return The decorated test.
     */
    protected WrappedSuiteTestDecorator decorateTests(Test test)
    {
        log.debug("params = " + ((params == null) ? null : MathUtils.printArray(params)));
        log.debug("repetitions = " + repetitions);
        log.debug("threads = " + ((threads == null) ? null : MathUtils.printArray(threads)));
        log.debug("duration = " + duration);

        // Wrap all tests in the test suite with WrappedSuiteTestDecorators. This is quite ugly and a bit baffling,
        // but the reason it is done is because the JUnit implementation of TestDecorator has some bugs in it.
        WrappedSuiteTestDecorator targetTest = null;

        if (test instanceof TestSuite)
        {
            log.debug("targetTest is a TestSuite");

            TestSuite suite = (TestSuite) test;

            int numTests = suite.countTestCases();
            log.debug("There are " + numTests + " in the suite.");

            for (int i = 0; i < numTests; i++)
            {
                Test nextTest = suite.testAt(i);
                log.debug("suite.testAt(" + i + ") = " + nextTest);

                if (nextTest instanceof TimingControllerAware)
                {
                    log.debug("nextTest is TimingControllerAware");
                }

                if (nextTest instanceof TestThreadAware)
                {
                    log.debug("nextTest is TestThreadAware");
                }
            }

            targetTest = new WrappedSuiteTestDecorator(suite);
            log.debug("Wrapped with a WrappedSuiteTestDecorator.");
        }
        // If the test has already been wrapped, no need to do it again.
        else if (test instanceof WrappedSuiteTestDecorator)
        {
            targetTest = (WrappedSuiteTestDecorator) test;
        }

        // If size parameter values have been set, then wrap the test in an asymptotic test decorator.
        if (params != null)
        {
            targetTest = new AsymptoticTestDecorator(targetTest, params, (repetitions == null) ? 1 : repetitions);
            log.debug("Wrapped with asymptotic test decorator.");
            log.debug("targetTest = " + targetTest);
        }

        // If no size parameters are set but the repitions parameter is, then wrap the test in an asymptotic test decorator.
        else if ((repetitions != null) && (repetitions > 1))
        {
            targetTest = new AsymptoticTestDecorator(targetTest, new int[] { 1 }, repetitions);
            log.debug("Wrapped with asymptotic test decorator.");
            log.debug("targetTest = " + targetTest);
        }

        // Apply any optional user specified decorators.
        targetTest = applyOptionalUserDecorators(targetTest);

        // If a test run duration has been set then wrap the test in a duration test decorator. This will wrap on
        // top of size, repeat or concurrency wrappings already applied.
        if (duration != null)
        {
            DurationTestDecorator durationTest = new DurationTestDecorator(targetTest, duration);
            targetTest = durationTest;

            log.debug("Wrapped with duration test decorator.");
            log.debug("targetTest = " + targetTest);

            registerShutdownHook(durationTest);
        }

        // ParameterVariationTestDecorator...

        // If a test thread concurrency level is set then wrap the test in a scaled test decorator. This will wrap on
        // top of size scaling or repetition wrappings.
        ScaledTestDecorator scaledDecorator;

        if ((threads != null) && ((threads.length > 1) || (MathUtils.maxInArray(threads) > 1)))
        {
            scaledDecorator = new ScaledTestDecorator(targetTest, threads);
            targetTest = scaledDecorator;
            log.debug("Wrapped with scaled test decorator.");
            log.debug("targetTest = " + targetTest);
        }
        else
        {
            scaledDecorator = new ScaledTestDecorator(targetTest, new int[] { 1 });
            targetTest = scaledDecorator;
            log.debug("Wrapped with scaled test decorator with default of 1 thread.");
            log.debug("targetTest = " + targetTest);
        }

        // Register the scaled test decorators shutdown hook.
        registerShutdownHook(scaledDecorator);

        return targetTest;
    }

    /**
     * If there were any user specified test decorators on the command line, this method instantiates them and wraps
     * the test in them, from inner-most to outer-most in the order in which the decorators were supplied on the
     * command line.
     *
     * @param targetTest The test to wrap.
     *
     * @return A wrapped test.
     */
    protected WrappedSuiteTestDecorator applyOptionalUserDecorators(WrappedSuiteTestDecorator targetTest)
    {
        // If there are user defined test decorators apply them in order now.
        for (TestDecoratorFactory factory : decoratorFactories)
        {
            targetTest = factory.decorateTest(targetTest);
        }

        return targetTest;
    }

    /**
     * Creates the TestResult object to be used for test runs. See {@link TKTestResult} for more information and the
     * enhanced test result class that this uses.
     *
     * @return An instance of the enhanced test result object, {@link TKTestResult}.
     */
    protected TestResult createTestResult()
    {
        log.debug("protected TestResult createTestResult(): called");

        TKTestResult result = new TKTestResult(delay, testCaseName);

        // Check if a directory to output reports to has been specified and attach test listeners if so.
        if (reportDir != null)
        {
            // Create the report directory if it does not already exist.
            File reportDirFile = new File(reportDir);

            if (!reportDirFile.exists())
            {
                reportDirFile.mkdir();
            }

            // Create the results file (make the name of this configurable as a command line parameter).
            Writer timingsWriter;

            // Always set up a console feedback listener.
            ConsoleTestListener feedbackListener = new ConsoleTestListener();
            result.addListener(feedbackListener);
            result.addTKTestListener(feedbackListener);

            // Set up an XML results listener to output the timings to the results file, if requested on the command line.
            if (xmlResults)
            {
                try
                {
                    File timingsFile = new File(reportDirFile, "TEST-" + currentTestClassName + ".xml");
                    timingsWriter = new BufferedWriter(new FileWriter(timingsFile), 20000);
                }
                catch (IOException e)
                {
                    throw new RuntimeException("Unable to create the log file to write test results to: " + e, e);
                }

                XMLTestListener listener = new XMLTestListener(timingsWriter, currentTestClassName);
                result.addListener(listener);
                result.addTKTestListener(listener);

                registerShutdownHook(listener);
            }

            // Set up an CSV results listener to output the timings to the results file, if requested on the command line.
            if (csvResults)
            {
                try
                {
                    File timingsFile =
                        new File(reportDirFile, testRunName + "-" + TIME_STAMP_FORMAT.format(new Date()) + "-timings.csv");
                    timingsWriter = new BufferedWriter(new FileWriter(timingsFile), 20000);
                }
                catch (IOException e)
                {
                    throw new RuntimeException("Unable to create the log file to write test results to: " + e, e);
                }

                CSVTestListener listener = new CSVTestListener(timingsWriter);
                result.addListener(listener);
                result.addTKTestListener(listener);

                // Register the results listeners shutdown hook to flush its data if the test framework is shutdown
                // prematurely.
                registerShutdownHook(listener);
            }

            // Register the results listeners shutdown hook to flush its data if the test framework is shutdown
            // prematurely.
            // registerShutdownHook(listener);

            // Record the start time of the batch.
            // result.notifyStartBatch();

            // At this point in time the test class has been instantiated, giving it an opportunity to read its parameters.
            // Inform any test listers of the test properties.
            result.notifyTestProperties(TestContextProperties.getAccessedProps());
        }

        return result;
    }

    /**
     * Registers the shutdown hook of a {@link ShutdownHookable}.
     *
     * @param hookable The hookable to register.
     */
    protected void registerShutdownHook(ShutdownHookable hookable)
    {
        Runtime.getRuntime().addShutdownHook(hookable.getShutdownHook());
    }

    /**
     * Initializes the test runner with the provided command line arguments and and starts the test run.
     *
     * @param testClassName The fully qualified name of the test class to run.
     *
     * @return The test results.
     *
     * @throws Exception Any exceptions from running the tests are allowed to fall through.
     */
    protected TestResult start(String testClassName) throws Exception
    {
        // Record the current test class, so that the test results can be output to a file incorporating this name.
        this.currentTestClassName = testClassName;

        // Delegate to the super method to run the tests.
        return super.start(new String[] { testClassName });
    }
}
