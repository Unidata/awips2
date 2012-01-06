/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.tools.messagestore;

import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.configuration.Configuration;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.ConfigurationFileApplicationRegistry;
import org.apache.qpid.server.store.MemoryMessageStore;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.tools.messagestore.commands.Clear;
import org.apache.qpid.tools.messagestore.commands.Command;
import org.apache.qpid.tools.messagestore.commands.Copy;
import org.apache.qpid.tools.messagestore.commands.Dump;
import org.apache.qpid.tools.messagestore.commands.Help;
import org.apache.qpid.tools.messagestore.commands.List;
import org.apache.qpid.tools.messagestore.commands.Load;
import org.apache.qpid.tools.messagestore.commands.Quit;
import org.apache.qpid.tools.messagestore.commands.Select;
import org.apache.qpid.tools.messagestore.commands.Show;
import org.apache.qpid.tools.messagestore.commands.Move;
import org.apache.qpid.tools.messagestore.commands.Purge;
import org.apache.qpid.tools.utils.CommandParser;
import org.apache.qpid.tools.utils.Console;
import org.apache.qpid.tools.utils.SimpleCommandParser;
import org.apache.qpid.tools.utils.SimpleConsole;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * MessageStoreTool.
 */
public class MessageStoreTool
{
    /** Text outputted at the start of each console.*/
    private static final String BOILER_PLATE = "MessageStoreTool - for examining Persistent Qpid Broker MessageStore instances";

    /** I/O Wrapper. */
    protected Console _console;

    /** Batch mode flag. */
    protected boolean _batchMode;

    /** Internal State object. */
    private State _state = new State();

    private HashMap<String, Command> _commands = new HashMap<String, Command>();

    /** SLF4J Logger. */
    private static Logger _devlog = LoggerFactory.getLogger(MessageStoreTool.class);

    /** Loaded configuration file. */
    private Configuration _config;

    /** Control used for main run loop. */
    private boolean _running = true;
    private boolean _initialised = false;

    //---------------------------------------------------------------------------------------------------/

    public static void main(String[] args) throws Configuration.InitException
    {

        MessageStoreTool tool = new MessageStoreTool(args);

        tool.start();
    }


    public MessageStoreTool(String[] args) throws Configuration.InitException
    {
        this(args, System.in, System.out);
    }

    public MessageStoreTool(String[] args, InputStream in, OutputStream out) throws Configuration.InitException
    {
        BufferedReader consoleReader = new BufferedReader(new InputStreamReader(in));
        BufferedWriter consoleWriter = new BufferedWriter(new OutputStreamWriter(out));

        Runtime.getRuntime().addShutdownHook(new Thread(new ShutdownHook(this)));
        _batchMode = false;

        _console = new SimpleConsole(consoleWriter, consoleReader);

        _config = new Configuration();

        setOptions();
        _config.processCommandline(args);
    }


    private void setOptions()
    {
        Option help = new Option("h", "help", false, "print this message");
        Option version = new Option("v", "version", false, "print the version information and exit");
        Option configFile =
                OptionBuilder.withArgName("file").hasArg()
                        .withDescription("use given configuration file By "
                                         + "default looks for a file named "
                                         + Configuration.DEFAULT_CONFIG_FILE + " in " + Configuration.QPID_HOME)
                        .withLongOpt("config")
                        .create("c");

        _config.setOption(help);
        _config.setOption(version);
        _config.setOption(configFile);
    }

    public State getState()
    {
        return _state;
    }

    public Map<String, Command> getCommands()
    {
        return _commands;
    }

    public void setConfigurationFile(String configfile) throws Configuration.InitException
    {
        _config.loadConfig(new File(configfile));
        setup();
    }

    public Console getConsole()
    {
        return _console;
    }

    public void setConsole(Console console)
    {
        _console = console;
    }

    /**
     * Simple ShutdownHook to cleanly shutdown the databases
     */
    class ShutdownHook implements Runnable
    {
        MessageStoreTool _tool;

        ShutdownHook(MessageStoreTool messageStoreTool)
        {
            _tool = messageStoreTool;
        }

        public void run()
        {
            _tool.quit();
        }
    }

    public void quit()
    {
        _running = false;

        if (_initialised)
        {
            ApplicationRegistry.remove(1);
        }

        _console.println("...exiting");

        _console.close();
    }

    public void setBatchMode(boolean batchmode)
    {
        _batchMode = batchmode;
    }

    /**
     * Main loop
     */
    protected void start()
    {
        setup();

        if (!_initialised)
        {
            System.exit(1);
        }

        _console.println("");

        _console.println(BOILER_PLATE);        

        runCLI();
    }

    private void setup()
    {
        loadDefaultVirtualHosts();

        loadCommands();

        _state.clearAll();
    }

    private void loadCommands()
    {
        _commands.clear();
        //todo Dynamically load the classes that exis in com.redhat.etp.qpid.commands
        _commands.put("close", new Clear(this));
        _commands.put("copy", new Copy(this));
        _commands.put("dump", new Dump(this));
        _commands.put("help", new Help(this));
        _commands.put("list", new List(this));
        _commands.put("load", new Load(this));
        _commands.put("move", new Move(this));
        _commands.put("purge", new Purge(this));
        _commands.put("quit", new Quit(this));
        _commands.put("select", new Select(this));
        _commands.put("show", new Show(this));
    }

    private void loadDefaultVirtualHosts()
    {
        final File configFile = _config.getConfigFile();

        loadVirtualHosts(configFile);
    }

    private void loadVirtualHosts(File configFile)
    {

        if (!configFile.exists())
        {
            _devlog.error("Config file not found:" + configFile.getAbsolutePath());
            return;
        }
        else
        {
            _devlog.debug("using config file :" + configFile.getAbsolutePath());
        }

        try
        {
            ConfigurationFileApplicationRegistry registry = new ConfigurationFileApplicationRegistry(configFile);

            ApplicationRegistry.remove(1);

            ApplicationRegistry.initialise(registry);

            checkMessageStores();
            _initialised = true;
        }
        catch (ConfigurationException e)
        {
            _console.println("Unable to load configuration due to configuration error: " + e.getMessage());
            e.printStackTrace();
        }
        catch (Exception e)
        {
            _console.println("Unable to load configuration due to: " + e.getMessage());
            e.printStackTrace();
        }


    }

    private void checkMessageStores()
    {
        Collection<VirtualHost> vhosts = ApplicationRegistry.getInstance().getVirtualHostRegistry().getVirtualHosts();

        boolean warning = false;
        for (VirtualHost vhost : vhosts)
        {
            if (vhost.getMessageStore() instanceof MemoryMessageStore)
            {
                _console.println("WARNING: Virtualhost '" + vhost.getName() + "' is using a MemoryMessageStore. "
                                 + "Changes will not persist.");
                warning = true;
            }
        }

        if (warning)
        {
            _console.println("");
            _console.println("Please ensure you are using the correct config file currently using '"
                             + _config.getConfigFile().getAbsolutePath() + "'");
            _console.println("New config file can be specifed by 'load <config file>' or -c on the commandline.");
            _console.println("");
        }
    }

    private void runCLI()
    {
        while (_running)
        {
            if (!_batchMode)
            {
                printPrompt();
            }

            String[] args = _console.readCommand();

            while (args != null)
            {
                exec(args);

                if (_running)
                {
                    if (!_batchMode)
                    {
                        printPrompt();
                    }

                    args = _console.readCommand();
                }
            }
        }
    }

    private void printPrompt()
    {
        _console.print(prompt());
    }


    /**
     * Execute a script (batch mode).
     *
     * @param script The file script
     */
    protected void runScripts(String script)
    {
        //Store Current State
        boolean oldBatch = _batchMode;
        CommandParser oldParser = _console.getCommandParser();
        setBatchMode(true);

        try
        {
            _devlog.debug("Running script '" + script + "'");

            _console.setCommandParser(new SimpleCommandParser(new BufferedReader(new FileReader(script))));

            start();
        }
        catch (java.io.FileNotFoundException e)
        {
            _devlog.error("Script not found: '" + script + "' due to:" + e.getMessage());
        }

        //Restore previous state
        _console.setCommandParser(oldParser);
        setBatchMode(oldBatch);
    }

    public String prompt()
    {
        String state = _state.toString();
        if (state != null && state.length() != 0)
        {
            return state + ":bdb$ ";
        }
        else
        {
            return "bdb$ ";
        }
    }

    /**
     * Execute the command.
     *
     * @param args [command, arg0, arg1...].
     */
    protected void exec(String[] args)
    {
        // Comment lines start with a #
        if (args.length == 0 || args[0].startsWith("#"))
        {
            return;
        }

        final String command = args[0];

        Command cmd = _commands.get(command);

        if (cmd == null)
        {
            _console.println("Command not understood: " + command);
        }
        else
        {
            cmd.execute(args);
        }
    }


    /**
     * Displays usage info.
     */
    protected static void help()
    {
        System.out.println(BOILER_PLATE);
        System.out.println("Usage: java " + MessageStoreTool.class + " [Options]");
        System.out.println("       [-c <broker config file>] : Defaults to \"$QPID_HOME/etc/config.xml\"");
    }


    /**
     * This class is used to store the current state of the tool.
     *
     * This is then interrogated by the various commands to augment their behaviour.
     *
     *
     */
    public class State
    {
        private VirtualHost _vhost = null;
        private AMQQueue _queue = null;
        private Exchange _exchange = null;
        private java.util.List<Long> _msgids = null;

        public State()
        {
        }

        public void setQueue(AMQQueue queue)
        {
            _queue = queue;
        }

        public AMQQueue getQueue()
        {
            return _queue;
        }

        public void setVhost(VirtualHost vhost)
        {
            _vhost = vhost;
        }

        public VirtualHost getVhost()
        {
            return _vhost;
        }

        public Exchange getExchange()
        {
            return _exchange;
        }

        public void setExchange(Exchange exchange)
        {
            _exchange = exchange;
        }

        public String toString()
        {
            StringBuilder status = new StringBuilder();

            if (_vhost != null)
            {
                status.append(_vhost.getName());

                if (_exchange != null)
                {
                    status.append("[");
                    status.append(_exchange.getName());
                    status.append("]");

                    if (_queue != null)
                    {
                        status.append("->'");
                        status.append(_queue.getName());
                        status.append("'");

                        if (_msgids != null)
                        {
                            status.append(printMessages());
                        }
                    }
                }
            }

            return status.toString();
        }


        public String printMessages()
        {
            StringBuilder sb = new StringBuilder();

            Long previous = null;

            Long start = null;
            for (Long id : _msgids)
            {
                if (previous != null)
                {
                    if (id == previous + 1)
                    {
                        if (start == null)
                        {
                            start = previous;
                        }
                    }
                    else
                    {
                        if (start != null)
                        {
                            sb.append(",");
                            sb.append(start);
                            sb.append("-");
                            sb.append(id);
                            start = null;
                        }
                        else
                        {
                            sb.append(",");
                            sb.append(previous);
                        }
                    }
                }

                previous = id;
            }

            if (start != null)
            {
                sb.append(",");
                sb.append(start);
                sb.append("-");
                sb.append(_msgids.get(_msgids.size() - 1));
            }
            else
            {
                sb.append(",");
                sb.append(previous);
            }

            // surround list in ()
            sb.replace(0, 1, "(");
            sb.append(")");
            return sb.toString();
        }

        public void clearAll()
        {
            _vhost = null;
            clearExchange();
        }

        public void clearExchange()
        {
            _exchange = null;
            clearQueue();
        }

        public void clearQueue()
        {
            _queue = null;
            clearMessages();
        }

        public void clearMessages()
        {
            _msgids = null;
        }

        /**
         * A common location to provide parsing of the message id string
         * utilised by a number of the commands.
         * The String is comma separated list of ids that can be individual ids
         * or a range (4-10)
         *
         * @param msgString string of msg ids to parse 1,2,4-10
         */
        public void setMessages(String msgString)
        {
            StringTokenizer tok = new StringTokenizer(msgString, ",");

            if (tok.hasMoreTokens())
            {
                _msgids = new LinkedList<Long>();
            }

            while (tok.hasMoreTokens())
            {
                String next = tok.nextToken();
                if (next.contains("-"))
                {
                    Long start = Long.parseLong(next.substring(0, next.indexOf("-")));
                    Long end = Long.parseLong(next.substring(next.indexOf("-") + 1));

                    if (end >= start)
                    {
                        for (long l = start; l <= end; l++)
                        {
                            _msgids.add(l);
                        }
                    }
                }
                else
                {
                    _msgids.add(Long.parseLong(next));
                }
            }

        }

        public void setMessages(java.util.List<Long> msgids)
        {
            _msgids = msgids;
        }

        public java.util.List<Long> getMessages()
        {
            return _msgids;
        }
    }//Class State

}//Class MessageStoreTool
