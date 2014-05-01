/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.cli2.Argument;
import org.apache.commons.cli2.CommandLine;
import org.apache.commons.cli2.Group;
import org.apache.commons.cli2.Option;
import org.apache.commons.cli2.builder.ArgumentBuilder;
import org.apache.commons.cli2.builder.DefaultOptionBuilder;
import org.apache.commons.cli2.builder.GroupBuilder;
import org.apache.commons.cli2.commandline.Parser;
import org.apache.commons.cli2.option.DefaultOption;
import org.apache.commons.cli2.util.HelpFormatter;
import org.apache.commons.cli2.validation.FileValidator;
import org.apache.commons.cli2.validation.NumberValidator;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.MapConfiguration;
import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfiguration;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess;
import org.rzo.yajsw.tools.ConfigGenerator;
import org.rzo.yajsw.tray.TrayIconMain;
import org.rzo.yajsw.util.VFSFileValidator;
import org.rzo.yajsw.wrapper.WrappedProcess;
import org.rzo.yajsw.wrapper.WrappedProcessFactory;
import org.rzo.yajsw.wrapper.WrappedProcessList;
import org.rzo.yajsw.wrapper.WrappedService;

import com.sun.jna.PlatformEx;

// TODO: Auto-generated Javadoc
/**
 * The Class WrapperExe.
 */
public class WrapperExe
{

	/** The group. */
	static Group				group;

	/** The cl. */
	static CommandLine			cl;

	/** The conf file. */
	static String				confFile;

	static List					confFileList;

	/** The properties. */
	static List					properties;

	/** The cmds. */
	static List					cmds;

	/** The pid. */
	static int					pid;

	/** The pid. */
	static String				defaultFile;

	/** The Constant OPTION_C. */
	static final int			OPTION_C			= 0;

	/** The Constant OPTION_T. */
	static final int			OPTION_T			= 1;

	/** The Constant OPTION_P. */
	static final int			OPTION_P			= 2;

	/** The Constant OPTION_T. */
	static final int			OPTION_TX			= 91;

	/** The Constant OPTION_P. */
	static final int			OPTION_PX			= 92;

	/** The Constant OPTION_I. */
	static final int			OPTION_I			= 3;

	/** The Constant OPTION_R. */
	static final int			OPTION_R			= 4;

	/** The Constant OPTION_N. */
	static final int			OPTION_N			= 5;

	/** The Constant OPTION_G. */
	static final int			OPTION_G			= 6;

	/** The Constant OPTION_D. */
	static final int			OPTION_D			= 7;

	/** The Constant OPTION_Q. */
	static final int			OPTION_Q			= 8;

	/** The Constant OPTION_QS. */
	static final int			OPTION_QS			= 9;

	/** The Constant OPTION_Y. */
	static final int			OPTION_Y			= 10;

	/** The Constant OPTION_QX. */
	static final int			OPTION_QX			= 11;
	static final int			OPTION_RW			= 12;

	/** The Constant CONF_FILE. */
	//static final String			CONF_FILE			= "confFile";

	/** The Constant PROPERTIES. */
	//static final String			PROPERTIES			= "properties";
	static final String			ARGS			= "arguments";

	/** The Constant PID. */
	static final String			PID					= "pid";

	/** The Constant DEFAULT_FILE. */
	static final String			DEFAULT_FILE		= "default configuration file";

	static WrappedService		_service			= null;

	static boolean				_exitOnTerminate	= true;

	static int					_exitCode			= 0;

	static Map<String, Object>	_properties			= new HashMap<String, Object>();

	private static WrappedService getService()
	{
		if (_service != null)
			return _service;
		prepareProperties();
		_service = new WrappedService();
		if (confFileList != null && confFileList.size() > 1)
			_service.setConfFilesList(confFileList);
		_service.setLocalConfiguration(new MapConfiguration(_properties));
		_service.init();
		return _service;
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		/*
		 * bkowal
		 * Suppress extraneous output.
		 */
		/*
		System.out.println("YAJSW: "+YajswVersion.YAJSW_VERSION);
		System.out.println("OS   : "+YajswVersion.OS_VERSION);
		System.out.println("JVM  : "+YajswVersion.JAVA_VERSION);
		*/
		String wrapperJar = WrapperLoader.getWrapperJar();
		String homeDir = new File(wrapperJar).getParent();
		if (!OperatingSystem.instance().setWorkingDir(homeDir))
			System.out.println("could not set working dir, pls check configuration or user rights: "+homeDir);
		
		// System.out.println(System.getProperty("java.class.path"));
		buildOptions();
		parseCommand(args);
		if (cmds != null && cmds.size() > 0)
			for (Iterator it = cmds.iterator(); it.hasNext();)
			{
				Object cmd = it.next();
				if (cmd instanceof DefaultOption)
					executeCommand((Option) cmd);
			}
		else
			executeCommand(group.findOption("c"));
		if (_exitOnTerminate)
			Runtime.getRuntime().halt(_exitCode);
	}

	private static File File(String property)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Execute command.
	 * 
	 * @param cmd
	 *            the cmd
	 */
	private static void executeCommand(Option cmd)
	{
		switch (cmd.getId())
		{
		case OPTION_C:
			doConsole();
			break;
		case OPTION_T:
			doStart();
			break;
		case OPTION_P:
			doStop();
			break;
		case OPTION_TX:
			doStartPosix();
			break;
		case OPTION_PX:
			doStopPosix();
			break;
		case OPTION_I:
			doInstall();
			break;
		case OPTION_R:
			doRemove();
			break;
		case OPTION_RW:
			doRemoveWait();
			break;
		case OPTION_N:
			pid = ((Long) cl.getValue(cmd)).intValue();
			doReconnect();
			break;
		case OPTION_G:
			pid = ((Long) cl.getValue(cmd)).intValue();
			doGenerate();
			break;
		case OPTION_D:
			break;
		case OPTION_Q:
			doState();
			break;
		case OPTION_QS:
			doStateSilent();
		case OPTION_QX:
			doStatePosix();
			break;
		case OPTION_Y:
			doStartTrayIcon();
			break;
		default:
			System.out.println("unimplemented option " + cmd.getPreferredName());
		}
	}

	/**
	 * Do reconnect.
	 */
	private static void doReconnect()
	{
		prepareProperties();
		Configuration localConf = new MapConfiguration(_properties);
		YajswConfiguration conf = new YajswConfigurationImpl(localConf, true);
		WrappedProcess w = WrappedProcessFactory.createProcess(conf);

		System.out.println("************* RECONNECTING WRAPPER TO PID  " + pid + " ***********************");
		System.out.println();

		if (w.reconnect(pid))
			System.out.println("Connected to PID " + pid);
		else
			System.out.println("NOT connected to PID " + pid);
		_exitOnTerminate = false;

	}

	/**
	 * Do remove.
	 */
	private static void doRemove()
	{
		prepareProperties();
		WrappedService w = getService();
		System.out.println("************* REMOVING " + w.getServiceName() + " ***********************");
		System.out.println();
		boolean result = w.uninstall();
		
			if (PlatformEx.isWinVista() && w.requiresElevate())
			{
				System.out.println("try uac elevate");
				WindowsXPProcess.elevateMe();
				return;
			}
		if (result)
			System.out.println("Service " + w.getServiceName() + " removed");
		else
			System.out.println("Service " + w.getServiceName() + " NOT removed");

	}

	private static void doRemoveWait()
	{
		prepareProperties();
		WrappedService w = getService();
		System.out.println("************* REMOVING " + w.getServiceName() + " ***********************");
		System.out.println();
		boolean result = w.uninstall();
		
			if (PlatformEx.isWinVista() && w.requiresElevate())
			{
				System.out.println("try uac elevate");
				WindowsXPProcess.elevateMe();
				return;
			}
		if (result)
		{
			while (w.isInstalled())
				try
				{
					Thread.sleep(1000);
				}
				catch (InterruptedException e)
				{
					Thread.currentThread().interrupt();
					return;
				}
			System.out.println("Service " + w.getServiceName() + " removed");
		}
		else
			System.out.println("Service " + w.getServiceName() + " NOT removed");

	}

	/**
	 * Do install.
	 */
	private static void doInstall()
	{
		WrappedService w = getService();
		System.out.println("************* INSTALLING " + w.getServiceName() + " ***********************");
		System.out.println();
		int i = 0;
		while (w.isInstalled() && i < 10)
		{
			if (PlatformEx.isWinVista() && w.requiresElevate())
			{
				System.out.println("try uac elevate");
				WindowsXPProcess.elevateMe();
				return;
			}

			i++;
			w.uninstall();
			try
			{
				Thread.sleep(2000);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		}

		
		boolean result = w.install();
		if (PlatformEx.isWinVista() && w.requiresElevate())
		{
			System.out.println("try uac elevate");
			WindowsXPProcess.elevateMe();
			return;
		}

		if (result)
			System.out.println("Service " + w.getServiceName() + " installed");
		else
			System.out.println("Service " + w.getServiceName() + " NOT installed");

	}

	/**
	 * Do stop.
	 */
	private static void doStop()
	{
		WrappedService w = getService();

		System.out.println("************* STOPPING " + w.getServiceName() + " ***********************");
		System.out.println();

		try
		{
			w.stop();
			if (PlatformEx.isWinVista() && w.requiresElevate())
			{
				System.out.println("try uac elevate");
				WindowsXPProcess.elevateMe();
				return;
			}

			if (w.isRunning())
				System.out.println("Service " + w.getServiceName() + " NOT stopped");
			else
				System.out.println("Service " + w.getServiceName() + " stopped");
		}
		catch (Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static void doStopPosix()
	{
		WrappedService w = getService();

		System.out.println("************* STOPPING " + w.getServiceName() + " ***********************");
		System.out.println();

		try
		{
			w.stopProcess();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		if (!w.isRunning())
		{
			System.out.println("Service " + w.getServiceName() + " stopped");
			_exitCode = 0;
			_exitOnTerminate = true;
		}
		else
		{
			System.out.println("Service" + w.getServiceName() + " NOT stopped");
			_exitCode = 1;
			_exitOnTerminate = true;
		}

	}

	/**
	 * Do start.
	 */
	private static void doStart()
	{
		WrappedService w = getService();
		// w.setDebug(true);
		w.init();

		System.out.println("************* STARTING " + w.getServiceName() + " ***********************");
		System.out.println();

		w.start();
		try
		{
			Thread.sleep(1000);
		}
		catch (InterruptedException e1)
		{
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		if (PlatformEx.isWinVista() && w.requiresElevate())
		{
			System.out.println("try uac elevate");
			WindowsXPProcess.elevateMe();
			return;
		}
		int i = 0;
		while (!w.isRunning() && i++ < 30)
		{
			try
			{
				Thread.sleep(1000);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				return;
			}
			if (!w.isStarting())
				break;
		}

		if (w.isRunning())
		{
			System.out.println("Service " + w.getServiceName() + " started");
			_exitCode = 0;
			_exitOnTerminate = true;
		}
		else
		{
			System.out.println("Service " + w.getServiceName() + " NOT started");
			_exitCode = 1;
			_exitOnTerminate = true;
		}

	}

	private static void doStartPosix()
	{
		WrappedService w = getService();
		System.out.println("************* STARTING " + w.getServiceName() + " ***********************");
		System.out.println();

		w.startProcess();
		int i = 0;
		while (!w.isRunning() && i < 10)
		{
			i++;
			try
			{
				Thread.sleep(2000);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
			}
		}
		if (w.isRunning())
			System.out.println("Service " + w.getServiceName() + " started");
		else
			System.out.println("Service " + w.getServiceName() + " NOT started");
		_exitOnTerminate = true;

	}

	/**
	 * Do start.
	 */
	private static void doStartTrayIcon()
	{
		prepareProperties();
		String[] args;
		if (_service != null)
			args = new String[]
			{ _service.getConfigLocalPath() };
		else
			args = new String[]
			{ confFile };
		try
		{
			TrayIconMain.main(args);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		_exitOnTerminate = true;
	}

	private static void doState()
	{
		prepareProperties();
		WrappedService w = getService();
		int state = w.state();
		System.out.print("Name        : ");
		System.out.println(w.getServiceName());
		System.out.print("Installed   : ");
		System.out.println(w.isInstalled(state));
		System.out.print("Running     : ");
		System.out.println(w.isRunning(state));
		System.out.print("Interactive : ");
		System.out.println(w.isInteractive(state));
		System.out.print("Automatic   : ");
		System.out.println(w.isAutomatic(state));
		System.out.print("Manual      : ");
		System.out.println(w.isManual(state));
		System.out.print("Disabled    : ");
		System.out.println(w.isDisabled(state));
		System.out.print("Paused      : ");
		System.out.println(w.isPaused(state));
		System.out.print("Unknown      : ");
		System.out.println(w.isStateUnknown(state));
	}

	private static void doStateSilent()
	{
		prepareProperties();
		WrappedService w = getService();
		w.init();
		int state = w.state();
	}

	private static void doStatePosix()
	{
		prepareProperties();
		WrappedService w = getService();
		int state = w.state();
		if (w.isRunning(state))
			_exitCode = 0;
		else
			_exitCode = 3;
		_exitOnTerminate = true;
	}

	/**
	 * Do console.
	 */
	private static void doConsole()
	{
		prepareProperties();
		final WrappedProcessList list = WrappedProcessFactory.createProcessList(_properties, confFileList, true);
		list.startAll();
//		Runtime.getRuntime().addShutdownHook(new Thread(new Runnable()
//		{
//
//			public void run()
//			{
//				list.onStopWrapper();
//			}
//			
//		}));
		_exitOnTerminate = false;
	}

	private static void doGenerate()
	{
		System.out.println("************* GENERATING YAJSW CONFIGURATION FOR PID " + pid + " ***********************");
		System.out.println();
		if (defaultFile != null)
			ConfigGenerator.generate(pid, new File(defaultFile), new File(confFile));
		else
			ConfigGenerator.generate(pid, null, new File(confFile));

	}

	/**
	 * Prepare properties.
	 */
	private static void prepareProperties()
	{
		if (confFile != null)
			_properties.put("wrapper.config", confFile);
		if (defaultFile != null)
			_properties.put("wrapperx.default.config", defaultFile);
		if (properties != null)
			for (Iterator it = properties.iterator(); it.hasNext();)
			{
				String prop = (String) it.next();
				String key = prop.substring(0, prop.indexOf('='));
				String value = prop.substring(prop.indexOf('=') + 1);
				_properties.put(key, value);
			}
	}

	/**
	 * Parses the command.
	 * 
	 * @param args
	 *            the args
	 */
	private static void parseCommand(String[] args)
	{
		Parser parser = new Parser();

		// configure a HelpFormatter
		HelpFormatter hf = new HelpFormatter();
		DefaultOptionBuilder oBuilder = new DefaultOptionBuilder();
		;

		// configure a parser
		Parser p = new Parser();
		p.setGroup(group);
		p.setHelpFormatter(hf);
		p.setHelpOption(oBuilder.withLongName("help").withShortName("?").create());
		cl = p.parseAndHelp(args);

		// abort application if no CommandLine was parsed
		if (cl == null)
		{
			System.exit(-1);
		}
		cmds = cl.getOptions();
		try
		{
			List arguments = cl.getValues(ARGS);
			properties = new ArrayList();
			confFileList = new ArrayList();
			for (Object obj : arguments)
			{
				String arg = (String) obj;
				if (Pattern.matches("wrapper\\..*=.*", arg))
					properties.add(arg);
				else
					confFileList.add(arg);					
			}
			if (confFileList.isEmpty())
				System.out.println("no wrapper config file found ");
			else
				confFile = (String) confFileList.get(0);
			/*
			confFileList = cl.getValues(CONF_FILE);
			if (confFileList == null || confFileList.isEmpty())
				System.out.println("no wrapper config file found ");
			else
				confFile = (String) confFileList.get(0);
				*/
		}
		catch (Exception ex)
		{
			System.out.println("no wrapper config file found ");
		}
		try
		{
			defaultFile = (String) cl.getValue(cl.getOption("-d"));
			if (defaultFile != null)
				defaultFile = new File(defaultFile).getCanonicalPath();
		}
		catch (Exception ex)
		{
			// no defaults -> maybe ok
		}
		//properties = cl.getValues(PROPERTIES);

	}

	/**
	 * Builds the options.
	 */
	private static void buildOptions()
	{
		DefaultOptionBuilder oBuilder = new DefaultOptionBuilder("-", "--", true);
		ArgumentBuilder aBuilder = new ArgumentBuilder();
		GroupBuilder gBuilder = new GroupBuilder();

		gBuilder.withOption(oBuilder.reset().withId(OPTION_C).withShortName("c").withLongName("console").withDescription(
				"run as a Console application").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_T).withShortName("t").withLongName("start").withDescription(
				"starT an NT service or Unix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_P).withShortName("p").withLongName("stop").withDescription(
				"stoP a running NT service or Unix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_TX).withShortName("tx").withLongName("startx").withDescription(
				"starT -internal a Posix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_PX).withShortName("px").withLongName("stopx").withDescription(
				"stoP -internal- a running Posix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_I).withShortName("i").withLongName("install").withDescription(
				"Install an NT service or Unix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_R).withShortName("r").withLongName("remove").withDescription(
		"Remove an NT service or Unix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_RW).withShortName("rw").withLongName("removeWait").withDescription(
		"Remove an NT service or Unix daemon and wait until it is removed").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_Q).withShortName("q").withLongName("query").withDescription(
				"Query the status of an NT service or Unix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_Y).withShortName("y").withLongName("tray").withDescription("Start System Tray Icon")
				.create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_QS).withShortName("qs").withLongName("querysilent").withDescription(
				"Silent Query the status of an NT service or Unix daemon").create());
		gBuilder.withOption(oBuilder.reset().withId(OPTION_QX).withShortName("qx").withLongName("queryposix").withDescription(
				"Query the status of a posix daemon. Return status as exit code").create());

		Argument pid = aBuilder.reset().withName(PID).withDescription("PID of process to reconnect to").withMinimum(1).withMaximum(1).withValidator(
				NumberValidator.getIntegerInstance()).create();

		gBuilder.withOption(oBuilder.reset().withId(OPTION_N).withShortName("n").withLongName("reconnect").withDescription(
				"recoNnect to existing application").withArgument(pid).create());

		Argument pid2 = aBuilder.reset().withName(PID).withDescription("PID of process to reconnect to").withMinimum(1).withMaximum(1).withValidator(
				NumberValidator.getIntegerInstance()).create();

		Argument defaultFile = aBuilder.reset().withName(DEFAULT_FILE).withDescription("Default Configuration File").withMinimum(0).withMaximum(1)
				.withValidator(VFSFileValidator.getExistingFileInstance().setBase(".")).create();
		/*
		 * GroupBuilder childGbuilder = new GroupBuilder(); DefaultOptionBuilder
		 * childoObuilder = new DefaultOptionBuilder("-", "--", true);
		 * 
		 * childGbuilder.withName(DEFAULT_FILE).withMinimum(0).withMaximum(1).
		 * withOption(
		 * childoObuilder.withId(OPTION_D).withArgument(defaultFile).
		 * withShortName("d").withLongName("defaultConf").withDescription(
		 * "Default Configuration File").create());
		 * 
		 * 
		 * 
		 * gBuilder.withOption(oBuilder.reset().withId(OPTION_G).withShortName("g"
		 * ).withLongName("genconf").withDescription(
		 * "Generate configuration file from pid"
		 * ).withArgument(pid2).withChildren(childGbuilder.create()).create());
		 */

		gBuilder.withOption(oBuilder.reset().withId(OPTION_D).withShortName("d").withLongName("defaultConf").withDescription(
				"Default Configuration File").withArgument(defaultFile).create());

		gBuilder.withOption(oBuilder.reset().withId(OPTION_G).withShortName("g").withLongName("genconf").withDescription(
				"Generate configuration file from pid").withArgument(pid2).create());

		FileValidator fValidator = VFSFileValidator.getExistingFileInstance().setBase(".");
		fValidator.setFile(false);
		// fValidator.setReadable(true);
		gBuilder.withOption(aBuilder.reset().withName(ARGS).withDescription("Arguments: a list of configuration files, for example conf/wrapper.conf followed by an optional list of configuration name-value pairs, for example wrapper.debug=true")
				.withMinimum(1).create());

		/*
		Validator pValidator = new Validator()
		{

			public void validate(List values) throws InvalidArgumentException
			{
				for (Iterator it = values.iterator(); it.hasNext();)
				{
					String p = (String) it.next();
					if (!Pattern.matches("wrapper\\..*=.*", p))
					{
						throw new InvalidArgumentException(p);
					}
				}

			}

		};
		gBuilder.withOption(aBuilder.reset().withName(PROPERTIES).withDescription(
				"are configuration name-value pairs which override values. For example: wrapper.debug=true").withMinimum(0).withValidator(pValidator)
				.create());
				*/

		gBuilder.withMaximum(3);
		group = gBuilder.create();

	}

}
