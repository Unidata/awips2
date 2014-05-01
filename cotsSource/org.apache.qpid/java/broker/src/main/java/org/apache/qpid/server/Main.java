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
package org.apache.qpid.server;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.Properties;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.Collection;
import java.util.Arrays;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.apache.log4j.xml.QpidLog4JConfigurator;
import org.apache.qpid.transport.network.ConnectionBinding;
import org.apache.qpid.transport.*;
import org.apache.qpid.common.QpidProperties;
import org.apache.qpid.framing.ProtocolVersion;
import org.apache.qpid.server.configuration.ServerConfiguration;
import org.apache.qpid.server.configuration.management.ConfigurationManagementMBean;
import org.apache.qpid.server.information.management.ServerInformationMBean;
import org.apache.qpid.server.logging.StartupRootMessageLogger;
import org.apache.qpid.server.logging.actors.BrokerActor;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.management.LoggingManagementMBean;
import org.apache.qpid.server.logging.messages.BrokerMessages;
import org.apache.qpid.server.protocol.AMQProtocolEngineFactory;
import org.apache.qpid.server.protocol.MultiVersionProtocolEngineFactory;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.ConfigurationFileApplicationRegistry;
import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.transport.ServerConnection;
import org.apache.qpid.server.transport.QpidAcceptor;
import org.apache.qpid.ssl.SSLContextFactory;
import org.apache.qpid.transport.NetworkDriver;
import org.apache.qpid.transport.network.mina.MINANetworkDriver;
import org.apache.qpid.server.protocol.MultiVersionProtocolEngineFactory.VERSION;

/**
 * Main entry point for AMQPD.
 *
 */
@SuppressWarnings({"AccessStaticViaInstance"})
public class Main
{
    private static Logger _logger;
    private static Logger _brokerLogger;

    private static final String DEFAULT_CONFIG_FILE = "etc/config.xml";

    public static final String DEFAULT_LOG_CONFIG_FILENAME = "log4j.xml";
    public static final String QPID_HOME = "QPID_HOME";
    private static final int IPV4_ADDRESS_LENGTH = 4;

    private static final char IPV4_LITERAL_SEPARATOR = '.';
    private static final Collection<VERSION> ALL_VERSIONS = Arrays.asList(VERSION.values());

    protected static class InitException extends Exception
    {
        InitException(String msg, Throwable cause)
        {
            super(msg, cause);
        }
    }

    protected final Options options = new Options();
    protected CommandLine commandLine;

    protected Main(String[] args)
    {
        setOptions(options);
        if (parseCommandline(args))
        {
            execute();
        }
    }

    protected boolean parseCommandline(String[] args)
    {
        try
        {
            commandLine = new PosixParser().parse(options, args);

            return true;
        }
        catch (ParseException e)
        {
            System.err.println("Error: " + e.getMessage());
            HelpFormatter formatter = new HelpFormatter();
            formatter.printHelp("Qpid", options, true);

            return false;
        }
    }

    protected void setOptions(Options options)
    {
        Option help = new Option("h", "help", false, "print this message");
        Option version = new Option("v", "version", false, "print the version information and exit");
        Option configFile =
                OptionBuilder.withArgName("file").hasArg().withDescription("use given configuration file").withLongOpt("config")
                        .create("c");
        Option port =
                OptionBuilder.withArgName("port").hasArg()
                        .withDescription("listen on the specified port. Overrides any value in the config file")
                        .withLongOpt("port").create("p");

        Option exclude0_10 =
                OptionBuilder.withArgName("exclude-0-10").hasArg()
                        .withDescription("when listening on the specified port do not accept AMQP0-10 connections. The specified port must be one specified on the command line")
                        .withLongOpt("exclude-0-10").create();

        Option exclude0_9_1 =
                OptionBuilder.withArgName("exclude-0-9-1").hasArg()
                        .withDescription("when listening on the specified port do not accept AMQP0-9-1 connections. The specified port must be one specified on the command line")
                        .withLongOpt("exclude-0-9-1").create();


        Option exclude0_9 =
                OptionBuilder.withArgName("exclude-0-9").hasArg()
                        .withDescription("when listening on the specified port do not accept AMQP0-9 connections. The specified port must be one specified on the command line")
                        .withLongOpt("exclude-0-9").create();


        Option exclude0_8 =
                OptionBuilder.withArgName("exclude-0-8").hasArg()
                        .withDescription("when listening on the specified port do not accept AMQP0-8 connections. The specified port must be one specified on the command line")
                        .withLongOpt("exclude-0-8").create();


        Option mport =
                OptionBuilder.withArgName("mport").hasArg()
                        .withDescription("listen on the specified management port. Overrides any value in the config file")
                        .withLongOpt("mport").create("m");


        Option bind =
                OptionBuilder.withArgName("bind").hasArg()
                        .withDescription("bind to the specified address. Overrides any value in the config file")
                        .withLongOpt("bind").create("b");
        Option logconfig =
                OptionBuilder.withArgName("logconfig").hasArg()
                        .withDescription("use the specified log4j xml configuration file. By "
                                         + "default looks for a file named " + DEFAULT_LOG_CONFIG_FILENAME
                                         + " in the same directory as the configuration file").withLongOpt("logconfig").create("l");
        Option logwatchconfig =
                OptionBuilder.withArgName("logwatch").hasArg()
                        .withDescription("monitor the log file configuration file for changes. Units are seconds. "
                                         + "Zero means do not check for changes.").withLongOpt("logwatch").create("w");

        options.addOption(help);
        options.addOption(version);
        options.addOption(configFile);
        options.addOption(logconfig);
        options.addOption(logwatchconfig);
        options.addOption(port);
        options.addOption(exclude0_10);
        options.addOption(exclude0_9_1);
        options.addOption(exclude0_9);
        options.addOption(exclude0_8);
        options.addOption(mport);
        options.addOption(bind);
    }

    protected void execute()
    {
        // note this understands either --help or -h. If an option only has a long name you can use that but if
        // an option has a short name and a long name you must use the short name here.
        if (commandLine.hasOption("h"))
        {
            HelpFormatter formatter = new HelpFormatter();
            formatter.printHelp("Qpid", options, true);
        }
        else if (commandLine.hasOption("v"))
        {
            String ver = QpidProperties.getVersionString();

            StringBuilder protocol = new StringBuilder("AMQP version(s) [major.minor]: ");

            boolean first = true;
            for (ProtocolVersion pv : ProtocolVersion.getSupportedProtocolVersions())
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    protocol.append(", ");
                }

                protocol.append(pv.getMajorVersion()).append('-').append(pv.getMinorVersion());

            }

            System.out.println(ver + " (" + protocol + ")");
        }
        else
        {
            try
            {
                CurrentActor.set(new BrokerActor(new StartupRootMessageLogger()));
                startup();
                CurrentActor.remove();
            }
            catch (InitException e)
            {
                System.out.println("Initialisation Error : " + e.getMessage());
                _brokerLogger.error("Initialisation Error : " + e.getMessage());
                shutdown(1);
            }
            catch (Throwable e)
            {
                System.out.println("Error initialising message broker: " + e);
                _brokerLogger.error("Error initialising message broker: " + e);
                e.printStackTrace();
                shutdown(1);
            }
        }
    }

    protected void shutdown(int status)
    {
        ApplicationRegistry.removeAll();
        System.exit(status);
    }

    protected void startup() throws Exception
    {
        final String QpidHome = System.getProperty(QPID_HOME);
        final File defaultConfigFile = new File(QpidHome, DEFAULT_CONFIG_FILE);
        final File configFile = new File(commandLine.getOptionValue("c", defaultConfigFile.getPath()));
        if (!configFile.exists())
        {
            String error = "File " + configFile + " could not be found. Check the file exists and is readable.";

            if (QpidHome == null)
            {
                error = error + "\nNote: " + QPID_HOME + " is not set.";
            }

            throw new InitException(error, null);
        }
        else
        {
            CurrentActor.get().message(BrokerMessages.BRK_CONFIG(configFile.getAbsolutePath()));
        }

        String logConfig = commandLine.getOptionValue("l");
        String logWatchConfig = commandLine.getOptionValue("w", "0");

        int logWatchTime = 0;
        try
        {
            logWatchTime = Integer.parseInt(logWatchConfig);
        }
        catch (NumberFormatException e)
        {
            System.err.println("Log watch configuration value of " + logWatchConfig + " is invalid. Must be "
                               + "a non-negative integer. Using default of zero (no watching configured");
        }

        File logConfigFile;
        if (logConfig != null)
        {
            logConfigFile = new File(logConfig);
            configureLogging(logConfigFile, logWatchTime);
        }
        else
        {
            File configFileDirectory = configFile.getParentFile();
            logConfigFile = new File(configFileDirectory, DEFAULT_LOG_CONFIG_FILENAME);
            configureLogging(logConfigFile, logWatchTime);
        }

        ConfigurationFileApplicationRegistry config = new ConfigurationFileApplicationRegistry(configFile);
        ServerConfiguration serverConfig = config.getConfiguration();
        updateManagementPort(serverConfig, commandLine.getOptionValue("m"));

        ApplicationRegistry.initialise(config);

        // We have already loaded the BrokerMessages class by this point so we
        // need to refresh the locale setting incase we had a different value in
        // the configuration.
        BrokerMessages.reload();

        // AR.initialise() sets its own actor so we now need to set the actor
        // for the remainder of the startup
        CurrentActor.set(new BrokerActor(config.getRootMessageLogger()));
        CurrentActor.setDefault(new BrokerActor(config.getRootMessageLogger()));

        try
        {
            configureLoggingManagementMBean(logConfigFile, logWatchTime);

            ConfigurationManagementMBean configMBean = new ConfigurationManagementMBean();
            configMBean.register();

            ServerInformationMBean sysInfoMBean =
                    new ServerInformationMBean(QpidProperties.getBuildVersion(), QpidProperties.getReleaseVersion());
            sysInfoMBean.register();

            //fixme .. use QpidProperties.getVersionString when we have fixed the classpath issues
            // that are causing the broker build to pick up the wrong properties file and hence say
            // Starting Qpid Client
            _brokerLogger.info("Starting Qpid Broker " + QpidProperties.getReleaseVersion()
                               + " build: " + QpidProperties.getBuildVersion());



            String[] portStr = commandLine.getOptionValues("p");

            Set<Integer> ports = new HashSet<Integer>();
            Set<Integer> exclude_0_10 = new HashSet<Integer>();
            Set<Integer> exclude_0_9_1 = new HashSet<Integer>();
            Set<Integer> exclude_0_9 = new HashSet<Integer>();
            Set<Integer> exclude_0_8 = new HashSet<Integer>();

            if(portStr == null || portStr.length == 0)
            {

                parsePortList(ports, serverConfig.getPorts());
                parsePortList(exclude_0_10, serverConfig.getPortExclude010());
                parsePortList(exclude_0_9_1, serverConfig.getPortExclude091());
                parsePortList(exclude_0_9, serverConfig.getPortExclude09());
                parsePortList(exclude_0_8, serverConfig.getPortExclude08());

            }
            else
            {
                parsePortArray(ports, portStr);
                parsePortArray(exclude_0_10, commandLine.getOptionValues("exclude-0-10"));
                parsePortArray(exclude_0_9_1, commandLine.getOptionValues("exclude-0-9-1"));
                parsePortArray(exclude_0_9, commandLine.getOptionValues("exclude-0-9"));
                parsePortArray(exclude_0_8, commandLine.getOptionValues("exclude-0-8"));

            }




            String bindAddr = commandLine.getOptionValue("b");
            if (bindAddr == null)
            {
                bindAddr = serverConfig.getBind();
            }
            InetAddress bindAddress = null;



            if (bindAddr.equals("wildcard"))
            {
                bindAddress = new InetSocketAddress(0).getAddress();
            }
            else
            {
                bindAddress = InetAddress.getByAddress(parseIP(bindAddr));
            }

            String hostName = bindAddress.getCanonicalHostName();


            String keystorePath = serverConfig.getKeystorePath();
            String keystorePassword = serverConfig.getKeystorePassword();
            String certType = serverConfig.getCertType();
            SSLContextFactory sslFactory = null;

            if (!serverConfig.getSSLOnly())
            {

                for(int port : ports)
                {

                    NetworkDriver driver = new MINANetworkDriver();

                    Set<VERSION> supported = new HashSet<VERSION>(ALL_VERSIONS);

                    if(exclude_0_10.contains(port))
                    {
                        supported.remove(VERSION.v0_10);
                    }

                    if(exclude_0_9_1.contains(port))
                    {
                        supported.remove(VERSION.v0_9_1);
                    }
                    if(exclude_0_9.contains(port))
                    {
                        supported.remove(VERSION.v0_9);
                    }
                    if(exclude_0_8.contains(port))
                    {
                        supported.remove(VERSION.v0_8);
                    }

                    MultiVersionProtocolEngineFactory protocolEngineFactory =
                            new MultiVersionProtocolEngineFactory(hostName, supported);



                    driver.bind(port, new InetAddress[]{bindAddress}, protocolEngineFactory,
                                serverConfig.getNetworkConfiguration(), null);
                    ApplicationRegistry.getInstance().addAcceptor(new InetSocketAddress(bindAddress, port),
                                                                  new QpidAcceptor(driver,"TCP"));
                    CurrentActor.get().message(BrokerMessages.BRK_LISTENING("TCP", port));

                }

            }

            if (serverConfig.getEnableSSL())
            {
                sslFactory = new SSLContextFactory(keystorePath, keystorePassword, certType);
                NetworkDriver driver = new MINANetworkDriver();
                driver.bind(serverConfig.getSSLPort(), new InetAddress[]{bindAddress},
                            new AMQProtocolEngineFactory(), serverConfig.getNetworkConfiguration(), sslFactory);
                ApplicationRegistry.getInstance().addAcceptor(new InetSocketAddress(bindAddress, serverConfig.getSSLPort()),
                        new QpidAcceptor(driver,"TCP"));
                CurrentActor.get().message(BrokerMessages.BRK_LISTENING("TCP/SSL", serverConfig.getSSLPort()));
            }

            //fixme  qpid.AMQP should be using qpidproperties to get value
            _brokerLogger.info("Qpid Broker Ready :" + QpidProperties.getReleaseVersion()
                    + " build: " + QpidProperties.getBuildVersion());

            CurrentActor.get().message(BrokerMessages.BRK_READY());

        }
        finally
        {
            // Startup is complete so remove the AR initialised Startup actor
            CurrentActor.remove();
        }



    }

    private void parsePortArray(Set<Integer> ports, String[] portStr)
            throws InitException
    {
        if(portStr != null)
        {
            for(int i = 0; i < portStr.length; i++)
            {
                try
                {
                    ports.add(Integer.parseInt(portStr[i]));
                }
                catch (NumberFormatException e)
                {
                    throw new InitException("Invalid port: " + portStr[i], e);
                }
            }
        }
    }

    private void parsePortList(Set<Integer> output, List input)
            throws InitException
    {
        if(input != null)
        {
            for(Object port : input)
            {
                try
                {
                    output.add(Integer.parseInt(String.valueOf(port)));
                }
                catch (NumberFormatException e)
                {
                    throw new InitException("Invalid port: " + port, e);
                }
            }
        }
    }

    /**
     * Update the configuration data with the management port.
     * @param configuration
     * @param managementPort The string from the command line
     */
    private void updateManagementPort(ServerConfiguration configuration, String managementPort)
    {
        if (managementPort != null)
        {
            try
            {
                configuration.setJMXManagementPort(Integer.parseInt(managementPort));
            }
            catch (NumberFormatException e)
            {
                _logger.warn("Invalid management port: " + managementPort + " will use:" + configuration.getJMXManagementPort(), e);
            }
        }
    }

    public static void main(String[] args)
    {
        //if the -Dlog4j.configuration property has not been set, enable the init override
        //to stop Log4J wondering off and picking up the first log4j.xml/properties file it
        //finds from the classpath when we get the first Loggers
        if(System.getProperty("log4j.configuration") == null)
        {
            System.setProperty("log4j.defaultInitOverride", "true");
        }

        //now that the override status is know, we can instantiate the Loggers
        _logger = Logger.getLogger(Main.class);
        _brokerLogger = Logger.getLogger("Qpid.Broker");

        new Main(args);
    }

    private byte[] parseIP(String address) throws Exception
    {
        char[] literalBuffer = address.toCharArray();
        int byteCount = 0;
        int currByte = 0;
        byte[] ip = new byte[IPV4_ADDRESS_LENGTH];
        for (int i = 0; i < literalBuffer.length; i++)
        {
            char currChar = literalBuffer[i];
            if ((currChar >= '0') && (currChar <= '9'))
            {
                currByte = (currByte * 10) + (Character.digit(currChar, 10) & 0xFF);
            }

            if (currChar == IPV4_LITERAL_SEPARATOR || (i + 1 == literalBuffer.length))
            {
                ip[byteCount++] = (byte) currByte;
                currByte = 0;
            }
        }

        if (byteCount != 4)
        {
            throw new Exception("Invalid IP address: " + address);
        }
        return ip;
    }

    private void configureLogging(File logConfigFile, int logWatchTime) throws InitException, IOException
    {
        if (logConfigFile.exists() && logConfigFile.canRead())
        {
            CurrentActor.get().message(BrokerMessages.BRK_LOG_CONFIG(logConfigFile.getAbsolutePath()));
            System.out.println("Configuring logger using configuration file " + logConfigFile.getAbsolutePath());
            if (logWatchTime > 0)
            {
                System.out.println("log file " + logConfigFile.getAbsolutePath() + " will be checked for changes every "
                                   + logWatchTime + " seconds");
                // log4j expects the watch interval in milliseconds
                try
                {
                    QpidLog4JConfigurator.configureAndWatch(logConfigFile.getPath(), logWatchTime * 1000);
                }
                catch (Exception e)
                {
                    throw new InitException(e.getMessage(),e);
                }
            }
            else
            {
                try
                {
                    QpidLog4JConfigurator.configure(logConfigFile.getPath());
                }
                catch (Exception e)
                {
                    throw new InitException(e.getMessage(),e);
                }
            }
        }
        else
        {
            System.err.println("Logging configuration error: unable to read file " + logConfigFile.getAbsolutePath());
            System.err.println("Using the fallback internal log4j.properties configuration");

            InputStream propsFile = this.getClass().getResourceAsStream("/log4j.properties");
            if(propsFile == null)
            {
                throw new IOException("Unable to load the fallback internal log4j.properties configuration file");
            }
            else
            {
                Properties fallbackProps = new Properties();
                fallbackProps.load(propsFile);
                PropertyConfigurator.configure(fallbackProps);
            }
        }
    }

    private void configureLoggingManagementMBean(File logConfigFile, int logWatchTime) throws Exception
    {
        LoggingManagementMBean blm = new LoggingManagementMBean(logConfigFile.getPath(),logWatchTime);

        blm.register();
    }
}
