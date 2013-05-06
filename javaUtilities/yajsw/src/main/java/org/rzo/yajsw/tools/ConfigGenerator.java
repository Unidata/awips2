package org.rzo.yajsw.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.configuration.PropertiesConfiguration;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;

public class ConfigGenerator
{
	private static void usage()
	{
		System.out.println("Usage: java -cp wrapper.jar org.rzo.yajsw.ConfigurationGenerator <pid> <output file>");
		System.exit(-1);
	}

	public static void generate(int pid, File input, File output)
	{
		Process p = OperatingSystem.instance().processManagerInstance().getProcess(pid);
		if (p == null)
		{
			System.out.println("cannot find process " + pid);
			return;
		}
		String cmd = p.getCommand().trim();
		if (cmd == null)
		{
			System.out.println("cannot get command line of process");
			return;
		}

		System.out.println();
		System.out.println("process command line:");
		System.out.println(p.getCommand());
		System.out.println();
		createConfigFile(p, input, output, cmd);

	}

	public static void main(String[] args)
	{
		if (args.length < 2)
		{
			usage();
		}
		int pid = -1;
		try
		{
			pid = Integer.parseInt(args[0]);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			usage();
		}
		File output = null;
		try
		{
			output = new File(args[1]);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			usage();
		}
		File input = null;
		try
		{
			input = new File(args[2]);
		}
		catch (Exception ex)
		{
		}

		generate(pid, input, output);

	}

	private static void createConfigFile(Process p, File input, File output, String cmd)
	{
		if (isJavaCmd(p))
			createJavaConfigFile(p, input, output);
		else
			createImageConfigFile(p, input, output);

		System.out.println("-----------------");
		System.out.println("Output file: " + output.getAbsolutePath());
		System.out.println("-----------------");
		System.out.println("NOTE: check/edit the following properties in the config file!");
		System.out.println();
		System.out.println("wrapper.app.account, wrapper.app.password: either set the password or remove the account");
		System.out.println("wrapper.java.command");
		System.out.println("wrapper.working.dir");
		System.out.println("wrapper.ntservice.name, wrapper.ntservice.displayname, wrapper.ntservice.description");
		System.out.println("-----------------");

	}

	// TODO
	private static boolean isJavaCmd(Process p)
	{
		/*
		 * try { MonitoredHost monitoredhost =
		 * MonitoredHost.getMonitoredHost("//localhost"); VmIdentifier
		 * vmidentifier = new VmIdentifier("" + pid); MonitoredVm monitoredvm =
		 * monitoredhost.getMonitoredVm(vmidentifier, 0); return monitoredvm !=
		 * null; } catch (Exception e) { // e.printStackTrace(); } return false;
		 */
		return p.getCommand().contains("java");
	}

	private static void createImageConfigFile(Process p, File input, File output)
	{
		System.out.println("creating image configuration file");
		System.out.println("NOT YET IMPLEMENTED");
	}

	private static void createJavaConfigFile(Process p, File input, File output)
	{
		System.out.println("creating java configuration file");

		try
		{
			String workingDir = p.getWorkingDir();
			String cmd = p.getCommand();

			/*
			 * MonitoredHost monitoredhost =
			 * MonitoredHost.getMonitoredHost("//localhost"); VmIdentifier
			 * vmidentifier = new VmIdentifier("" + p.getPid()); MonitoredVm vm
			 * = monitoredhost.getMonitoredVm(vmidentifier, 0);
			 */

			// System.out.println("cmd " +MonitoredVmUtil.commandLine(vm));
			PropertiesConfiguration conf;
			if (input == null)
				conf = new PropertiesConfiguration();
			else
				conf = new PropertiesConfiguration(input);

			JCLParser parsedCmd = JCLParser.parse(cmd);
			/*
			 * String mainClass = MonitoredVmUtil.mainClass(vm, true); if
			 * (!isNotNullEmpty(mainClass)) {System.out.println(
			 * "could not retrieve main class of java application -> abort");
			 * return; } mainClass = confString(mainClass); if
			 * (mainClass.endsWith(".jar"))
			 * conf.setProperty("wrapper.java.app.jar",
			 * relativeString(mainClass, workingDir)); else
			 * conf.setProperty("wrapper.java.app.mainclass", mainClass);
			 */
			if (parsedCmd.getMainClass() != null)
				conf.setProperty("wrapper.java.app.mainclass", parsedCmd.getMainClass());
			else
				conf.setProperty("wrapper.java.app.jar", relativeString(parsedCmd.getJar(), workingDir));

			/*
			 * // this does not seem to work correctly -> get jvm the hard way
			 * // System.out.println("vmVersion " + vmVersion); String jvm =
			 * null; if (cmd.startsWith("\"")) jvm = cmd.substring(0,
			 * cmd.indexOf("\" ") + 1); else jvm = cmd.substring(0,
			 * cmd.indexOf(" ")); if (isNotNullEmpty(jvm)) { jvm =
			 * confString(jvm); conf.setProperty("wrapper.java.command", jvm); }
			 */
			conf.setProperty("wrapper.java.command", parsedCmd.getJava());
			/*
			 * String classpath = ((StringMonitor)
			 * vm.findByName("java.property.java.class.path")).stringValue(); if
			 * (isNotNullEmpty(classpath)) { classpath =
			 * relativeString(classpath, workingDir); classpath =
			 * confString(classpath); String[] classpaths =
			 * classpath.split(System.getProperty("path.separator")); int i = 1;
			 * for (String file : classpaths) {
			 * conf.setProperty("wrapper.java.classpath." + i, file); i++; } }
			 */
			int i = 1;
			List<String> classpathList = parsedCmd.getClasspath();
			// no longer required - wrapper will automatically add the jar to the classpath
			//if (conf.getString("wrapper.java.app.jar", null) != null)
			//	classpathList.add(conf.getString("wrapper.java.app.jar"));
			if (classpathList == null || classpathList.isEmpty())
				classpathList = getClasspathFromEnvironment(p);
			if (classpathList.isEmpty() && parsedCmd.getJar() == null)
				classpathList.add(".");
			for (String classpath : classpathList)
			{
				classpath = relativeString(classpath, workingDir);
				classpath = confString(classpath);
				// yajsw handles wildcards differently.
				if (classpath.endsWith("*"))
					classpath = classpath + ".jar";
				conf.setProperty("wrapper.java.classpath." + i++, classpath);
			}

			/*
			 * // bug in MonitoredVMUtil 'c:/x.txt "d d"' returns 'c:/x.txt d d'
			 * //String mainArgs = MonitoredVmUtil.mainArgs(vm); // TODO really
			 * parse the cmd String mainArgs =
			 * cmd.substring(cmd.indexOf(" "+mainClass
			 * +" ")+mainClass.length()+2); if (isNotNullEmpty(mainArgs)) { List
			 * args = splitArgs(mainArgs); int i = 1; for (Iterator
			 * it=args.iterator(); it.hasNext(); ) { String arg = (String)
			 * it.next(); arg = relativeString(arg, workingDir); arg =
			 * confString(arg); conf.setProperty("wrapper.app.parameter."+i++,
			 * arg); } }
			 */

			i = 1;
			for (String arg : parsedCmd.getArgs())
			{
				arg = relativeString(arg, workingDir);
				arg = confString(arg);
				conf.setProperty("wrapper.app.parameter." + i++, arg);
			}
			/*
			 * // bug in MonitoredVMUtil '"-Xd=a a"' returns '-Xd=a a' //String
			 * jvmArgs = MonitoredVmUtil.jvmArgs(vm); // TODO really parse the
			 * cmd String jvmArgs = cmd.substring(jvm.length(),
			 * cmd.indexOf(" "+mainClass+" ")); if (cmd.startsWith("\""))
			 * jvmArgs = jvmArgs.substring(1); jvmArgs =
			 * jvmArgs.replace(classpath, ""); jvmArgs =
			 * jvmArgs.replace(" -classpath ", ""); jvmArgs =
			 * jvmArgs.replace(" -cp ", "");
			 * 
			 * if (isNotNullEmpty(jvmArgs)) { List args = splitArgs(jvmArgs);
			 * int i = 1; for (Iterator it=args.iterator(); it.hasNext(); ) {
			 * String arg = (String) it.next(); arg = relativeString(arg,
			 * workingDir); arg = confString(arg);
			 * conf.setProperty("wrapper.java.additional."+i++, arg); } }
			 * 
			 * String jvmFlags = MonitoredVmUtil.jvmFlags(vm);
			 */
			i = 1;
			for (String opt : parsedCmd.getVmOptions())
			{
				opt = relativeString(opt, workingDir);
				opt = confString(opt);
				conf.setProperty("wrapper.java.additional." + i++, opt);
			}

			if (isNotNullEmpty(workingDir))
			{
				workingDir = confString(workingDir);
				conf.setProperty("wrapper.working.dir", workingDir);
			}
			String title = p.getTitle();
			if (cmd.equals(title))
				title = parsedCmd.getMainClass();

			if (isNotNullEmpty(title))
			{
				title = confString(title);
				conf.setProperty("wrapper.console.title", title);
				conf.setProperty("wrapper.ntservice.name", title);
				conf.setProperty("wrapper.ntservice.displayname", title);
				conf.setProperty("wrapper.ntservice.description", title);
			}
			/*
			 * String account = p.getUser(); if (account != null &&
			 * !"".equals(account)) conf.setProperty("wrapper.app.account",
			 * account);
			 */

			/*
			 * List l = vm.findByPattern(".*"); for (Iterator it = l.iterator();
			 * it.hasNext(); ) { Monitor m = (Monitor) it.next();
			 * System.out.println(m.getName()); System.out.println("> "+
			 * m.getValue()); }
			 */

			conf.save(output);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}

	}

	private static List<String> getClasspathFromEnvironment(Process p)
	{
		List<String> result = new ArrayList<String>();
		try
		{
		String cp = (String) p.getEnvironmentAsMap().get("CLASSPATH");
		if (cp == null)
			return result;
		String[] cpArr = cp.split(File.pathSeparator);
		for (String cc : cpArr)
		{
			cc = cc.replaceAll("\"", "");
			result.add(cc.trim());
		}
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		return result;

	}

	public static List splitArgs(String jvmArgs)
	{
		// split either by " or by space
		List result = new ArrayList();
		while (jvmArgs.length() > 0)
		{
			String arg;
			jvmArgs = jvmArgs.trim();
			if (jvmArgs.startsWith("\""))
			{
				jvmArgs = jvmArgs.substring(1);
				int index = jvmArgs.indexOf("\"");
				arg = jvmArgs.substring(0, index);
				jvmArgs = jvmArgs.substring(index + 1);
			}
			else
			{
				int index = jvmArgs.indexOf(" ");
				int index2 = jvmArgs.indexOf("\"");
				if (index2 < index && index2 != -1)
				{
					index = jvmArgs.indexOf("\"", index);
				}
				if (index > -1)
				{
					arg = jvmArgs.substring(0, index);
					jvmArgs = jvmArgs.substring(index + 1);
				}
				else
				{
					arg = jvmArgs;
					jvmArgs = "";
				}
			}
			arg = arg.trim();
			arg = arg.replaceAll("\"", "");
			if (arg.length() > 0)
				result.add(arg);
		}
		return result;
	}

	private static String relativeString(String str, String base)
	{
		if (isNotNullEmpty(base))
		{
			String baseRegEx = base.replaceAll("\\\\", "\\\\\\\\");
			baseRegEx = baseRegEx.replaceAll("\\.", "\\.");
			String sep = "\\".equals(File.separator) ? "\\\\" : File.separator;
			String result = str.replaceAll(baseRegEx, "." + sep);
			return result;
		}
		return str;

	}

	public static String confString(String str)
	{
		String result = str.replaceAll("\\\\", "\\\\");
		result = result.replaceAll(",", "\\,");
		return result;
	}

	public static boolean isNotNullEmpty(String str)
	{
		return str != null && !"".equals(str) && !"Unknown".equals(str);
	}

}
