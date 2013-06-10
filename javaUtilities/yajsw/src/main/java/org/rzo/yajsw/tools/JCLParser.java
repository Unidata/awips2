package org.rzo.yajsw.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JCLParser
{
	List<String>	_classpath	= new ArrayList<String>();
	List<String>	_vmOptions	= new ArrayList<String>();
	List<String>	_args		= new ArrayList<String>();
	String			_java		= null;
	String			_mainClass	= null;
	String			_jar		= null;
	List<Integer> bs = new ArrayList<Integer>();


	private JCLParser(String commandLine)
	{
		parseInternal(commandLine);
	}

	public static JCLParser parse(String commandLine)
	{
		JCLParser result = null;
		result = new JCLParser(commandLine);
		return result;
	}
	
	private boolean inBrackets(int s)
	{
		boolean result = false;
		for (int k = 0; k < bs.size(); k += 2)
		{
			if (s >= bs.get(k) && s < bs.get(k + 1))
			{
				result = true;
				break;
			}
		}
		return result;
	}

	private int bracketEnd(int s)
	{
		int result = -1;
		for (int k = 0; k < bs.size(); k += 2)
		{
			if (s >= bs.get(k) && s < bs.get(k + 1))
			{
				result = bs.get(k + 1);
				break;
			}
		}
		return result;
	}

	// TODO this should cover most cases but is not complete
	private void parseInternal(String commandLine)
	{
		Matcher mr;
		Pattern p;
		// last position of _java in commandLine
		int posJ = 0;
		// last position of _classpath in commandLine
		int posCp = 0;
		// last position of __vmOptions in commandLine
		int posOpts = 0;
		// last position of _mainClass
		int posclp = 0;
		// last position of _jar
		int posJar = 0;

		// parse java
		p = Pattern.compile("\\A(\"[^\"]+\")|(\\S+) ");
		mr = p.matcher(commandLine);
		if (mr.find())
		{
			_java = mr.group();
			_java = _java.replaceAll("\"", "");
			_java = _java.trim();
			posJ = mr.end() - 1;
		}
		else
			throw new RuntimeException("could not parse command line " + commandLine);

		// parse jar
		p = Pattern.compile(" -jar +((\"[^\"]+\")|(\\S+))");
		mr = p.matcher(commandLine);
		if (mr.find(posJ))
		{
			_jar = mr.group(1);
			_jar = _jar.replaceAll("\"", "");
			_jar = _jar.trim();
			posJar = mr.end() - 1;
		}

		// parse classpath
		p = Pattern.compile("(( -cp)|( -classpath)|( \"-classpath\")) +((\"[^\"]+\")|(\\S+)) ");
		mr = p.matcher(commandLine);
		if (mr.find(posJ))
		{
			String cp = mr.group().trim();
			posCp = mr.end() - 1;
			cp = cp.substring(cp.indexOf(' '));
			String[] cpArr = cp.split(File.pathSeparator);
			for (String cc : cpArr)
			{
				cc = cc.replaceAll("\"", "").trim();
				if (cc.length() != 0)
					_classpath.add(cc);
			}
		}

		// find brackets
		p = Pattern.compile("\"([^\"])+\"");
		mr = p.matcher(commandLine);
		int i = 0;
		while (mr.find(i))
		{
			bs.add(mr.start());
			bs.add(mr.end()-1);
			int k = 0;
			while (mr.end()+k < commandLine.length() && commandLine.charAt(mr.end()+k) == '"')
				k++;
			i = mr.end()+k;
		}

		// parse main class
		if (_jar == null)
		{
			//p = Pattern.compile(" ([^- ])+( |$)");
			// "-" may be in class name
			p = Pattern.compile(" ([^ ])+( |$)");
			mr = p.matcher(commandLine);
			int max = Math.max(posJ, posCp);
			while (mr.find(max))
			{
				int s = mr.start();
				String mc = mr.group();
				if (!inBrackets(s) && !mc.trim().startsWith("-"))
				{
					_mainClass = mc;
					_mainClass = _mainClass.replaceAll("\"", "");
					_mainClass = _mainClass.trim();
					posclp = mr.end() - 1;
					break;
				}
				else
					max = mr.end() - 1;
			}
		}

		// parse JVM options
		p = Pattern.compile("(( -\\S+)|( -\"[^\"]+\")|( \"-[^\"]+\")) ");
		mr = p.matcher(commandLine);
		int max = Math.max(posJar, posclp);
		int pos = 0;
		while (mr.find(pos))
		{
			String opt = mr.group().trim();
			pos = mr.end() - 1;
			opt = opt.replaceAll("\"", "");
			if (!opt.startsWith("-jar") && !opt.startsWith("-cp") && !opt.startsWith("-classpath") && mr.end() < max)
			{
				if (inBrackets(pos))
				{
					int end = bracketEnd(pos);
					opt = commandLine.substring(mr.start(), end);
					opt = opt.replaceAll("\"", "");
					_vmOptions.add(opt);
					posOpts = end+1;
					pos = end;
				}
				else
				{
				_vmOptions.add(opt);
				posOpts = mr.end();
				}
			}
		}

		// parse args
		p = Pattern.compile(" ((\"[^\"]+\")|(\\S+))( |$)");
		mr = p.matcher(commandLine);
		max = Math.max(posclp, posJar);
		max = Math.max(max, posOpts);
		if (mr.find(max))
		{
			String arg = mr.group();
			arg = arg.replaceAll("\"", "");
			_args.add(arg.trim());
			max = mr.end() - 1;
			while (mr.find(max))
			{
				arg = mr.group();
				arg = arg.replaceAll("\"", "");
				arg = arg.trim();
				if (arg.length() > 0)
					_args.add(arg.trim());
				max = mr.end() - 1;
			}
		}

		if (_java == null || "".equals(_java) || ((_mainClass == null || "".equals(_mainClass)) && ((_jar == null || "".equals(_jar)))))
			throw new RuntimeException("error parsing java command line ");

	}

	public List<String> getClasspath()
	{
		return _classpath;
	}

	public List<String> getVmOptions()
	{
		return _vmOptions;
	}

	public List<String> getArgs()
	{
		return _args;
	}

	public String getJava()
	{
		return _java;
	}

	public String getMainClass()
	{
		return _mainClass;
	}

	public String getJar()
	{
		return _jar;
	}

	public static void main(String[] args)
	{
		String[] cmds = new String[]
		{
				"/opt/jdk/bin/java -Dcom.sun.management.jmxremote.port=9875 -Dcom.sun.management.jmxremote.authenticate=true -Dcom.sun.management.jmxremote.login.config=virgo-kernel-Dcom.sun.management.jmxremote.access.file=/opt/virgo-tomcat-server-3.0.3.RELEASE/config/org.eclipse.virgo.kernel.jmxremote.access.properties -Djavax.net.ssl.keyStore=/opt/virgo-tomcat-server-3.0.3.RELEASE/config/keystore -Djavax.net.ssl.keyStorePassword=changeit -Dcom.sun.management.jmxremote.ssl=true -Dcom.sun.management.jmxremote.ssl.need.client.auth=false -XX:+HeapDumpOnOutOfMemoryError -XX:ErrorFile=/opt/virgo-tomcat-server-3.0.3.RELEASE/serviceability/error.log -XX:HeapDumpPath=/opt/virgo-tomcat-server-3.0.3.RELEASE/serviceability/heap_dump.hprof -Djava.security.auth.login.config=/opt/virgo-tomcat-server-3.0.3.RELEASE/config/org.eclipse.virgo.kernel.authentication.config -Dorg.eclipse.virgo.kernel.authentication.file=/opt/virgo-tomcat-server-3.0.3.RELEASE/config/org.eclipse.virgo.kernel.users.properties -Djava.io.tmpdir=/opt/virgo-tomcat-server-3.0.3.RELEASE/work/tmp -Dorg.eclipse.virgo.kernel.home=/opt/virgo-tomcat-server-3.0.3.RELEASE -Dorg.eclipse.equinox.console.jaas.file=/opt/virgo-tomcat-server-3.0.3.RELEASE/config/store -Dssh.server.keystore=/opt/virgo-tomcat-server-3.0.3.RELEASE/config/hostkey.ser -Dgosh.args=--nointeractive -classpath :/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/com.springsource.javax.transaction-1.1.0.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/com.springsource.org.apache.mina.core-2.0.2.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/com.springsource.org.apache.sshd.core-0.5.0.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/com.springsource.slf4j.api-1.6.1.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.apache.felix.gogo.runtime-0.8.0.v201105062003.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.equinox.cm-1.0.300.v20101204.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.equinox.console.supportability-1.0.0.201108021516.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.osgi-3.7.0.v20110613.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.osgi.services-3.3.0.v20110110.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.virgo.kernel.authentication-3.0.3.RELEASE.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.virgo.kernel.shutdown-3.0.3.RELEASE.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.virgo.osgi.console-3.0.3.RELEASE.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.virgo.osgi.extensions.equinox-3.0.3.RELEASE.jar:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.virgo.osgi.launcher-3.0.3.RELEASE.jar org.eclipse.virgo.osgi.launcher.Launcher -config /opt/virgo-tomcat-server-3.0.3.RELEASE/lib/org.eclipse.virgo.kernel.launch.properties -Forg.eclipse.virgo.kernel.home=/opt/virgo-tomcat-server-3.0.3.RELEASE -Forg.eclipse.virgo.kernel.config=/opt/virgo-tomcat-server-3.0.3.RELEASE/config -Fosgi.configuration.area=/opt/virgo-tomcat-server-3.0.3.RELEASE/work/osgi/configuration -Fosgi.java.profile=file:/opt/virgo-tomcat-server-3.0.3.RELEASE/lib/java6-server.profile",
				"java -cp wrapper.jar -Xrs x.Test -c conf/wrapper.conf       ",
				"java -cp test.jar test.Main",
				"/opt/jdk/bin/java -Dcom.sun.management.jmxremote.port=9875 -Dcom.sun.management.jmxremote.authenticate=true -Dcom.sun.management.jmxremote.login.config=virgo-kernel -Dcom.sun.management.jmxremote.access.file=/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration/org.eclipse.virgo.kernel.jmxremote.access.properties -Djavax.net.ssl.keyStore=/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration/keystore -Djavax.net.ssl.keyStorePassword=changeit -Dcom.sun.management.jmxremote.ssl=true -Dcom.sun.management.jmxremote.ssl.need.client.auth=false -XX:+HeapDumpOnOutOfMemoryError -XX:ErrorFile=/opt/virgo-tomcat-server-3.5.0.RELEASE/serviceability/error.log -XX:HeapDumpPath=/opt/virgo-tomcat-server-3.5.0.RELEASE/serviceability/heap_dump.hprof -Djava.security.auth.login.config=/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration/org.eclipse.virgo.kernel.authentication.config -Dorg.eclipse.virgo.kernel.authentication.file=/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration/org.eclipse.virgo.kernel.users.properties -Djava.io.tmpdir=/opt/virgo-tomcat-server-3.5.0.RELEASE/work/tmp -Dorg.eclipse.virgo.kernel.home=/opt/virgo-tomcat-server-3.5.0.RELEASE -Dorg.eclipse.virgo.kernel.config=/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration -Dosgi.java.profile=file:/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration/java6-server.profile -Declipse.ignoreApp=true -Dosgi.install.area=/opt/virgo-tomcat-server-3.5.0.RELEASE -Dosgi.configuration.area=/opt/virgo-tomcat-server-3.5.0.RELEASE/work -Dssh.server.keystore=/opt/virgo-tomcat-server-3.5.0.RELEASE/configuration/hostkey.ser -Dosgi.frameworkClassPath=,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/javax.annotation_1.1.0.v201108011116.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/javax.transaction_1.1.1.v201105210645.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.equinox.launcher_1.3.0.v20120308-1358.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.osgi_3.8.0.v20120508-2119.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.kernel.authentication_3.5.0.RELEASE.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.kernel.shutdown_3.5.0.RELEASE.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.osgi.console_3.5.0.RELEASE.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.osgi.extensions.equinox_3.5.0.RELEASE.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.osgi.launcher_3.5.0.RELEASE.jar,file:/opt/virgo-tomcat-server-3.5.0.RELEASE/plugins/org.eclipse.osgi_3.8.0.v20120508-2119.jar -classpath :/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/javax.annotation_1.1.0.v201108011116.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/javax.transaction_1.1.1.v201105210645.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.equinox.launcher_1.3.0.v20120308-1358.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.osgi_3.8.0.v20120508-2119.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.kernel.authentication_3.5.0.RELEASE.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.kernel.shutdown_3.5.0.RELEASE.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.osgi.console_3.5.0.RELEASE.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.osgi.extensions.equinox_3.5.0.RELEASE.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/lib/org.eclipse.virgo.osgi.launcher_3.5.0.RELEASE.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/plugins/org.eclipse.osgi_3.8.0.v20120508-2119.jar:/opt/virgo-tomcat-server-3.5.0.RELEASE/plugins/org.eclipse.equinox.console.ssh_1.0.0.v20120430-1356.jar org.eclipse.equinox.launcher.Main-noExit",
				"\"C:\\Program Files\\Java\\jdk1.6.0_20\\bin\\java\" -Dcom.sun.management.jmxremote.port=9875 -Dcom.sun.management.jmxremote.authenticate=true -Dcom.sun.management.jmxremote.login.config=virgo-kernel -Dcom.sun.management.jmxremote.access.file=\"C:\\ABC-~1.0-S\\config\\org.eclipse.virgo.kernel.jmxremote.access.properties\" -Djavax.net.ssl.keyStore=\"C:\\ABC-~1.0-S\\config\\keystore\" -Djavax.net.ssl.keyStorePassword=abc123 -Dcom.sun.management.jmxremote.ssl=true -Dcom.sun.management.jmxremote.ssl.need.client.auth=false -XX:+HeapDumpOnOutOfMemoryError -XX:ErrorFile=\"C:\\ABC-~1.0-S\\serviceability\\error.log\" -XX:HeapDumpPath=\"C:\\ABC-~1.0-S\\serviceability\\heap_dump.hprof\" -Djava.security.auth.login.config=\"C:\\ABC-~1.0-S\\config\\org.eclipse.virgo.kernel.authentication.config\" -Dorg.eclipse.virgo.kernel.authentication.file=\"C:\\ABC-~1.0-S\\config\\org.eclipse.virgo.kernel.users.properties\" -Djava.io.tmpdir=\"\"C:\\ABC-~1.0-S\\work\tmp\\\"\" -Dorg.eclipse.virgo.kernel.home=\"C:\\ABC-~1.0-S\" -Dorg.eclipse.equinox.console.jaas.file=\"C:\\ABC-~1.0-S\\config/store\" -Dssh.server.keystore=\"C:\\ABC-~1.0-S\\config/hostkey.ser\" -Dgosh.args=\"--nointeractive\" -classpath \"C:\\ABC-~1.0-S\\lib\\com.springsource.javax.transaction-1.1.0.jar;C:\\ABC-~1.0-S\\lib\\com.springsource.org.apache.mina.core-2.0.2.jar;C:\\ABC-~1.0-S\\lib\\com.springsource.org.apache.sshd.core-0.5.0.jar;C:\\ABC-~1.0-S\\lib\\com.springsource.slf4j.api-1.6.1.jar;C:\\ABC-~1.0-S\\lib\\org.apache.felix.gogo.runtime-0.8.0.v201107131313.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.equinox.cm-1.0.300.v20101204.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.equinox.console.supportability-1.0.0.201108021516.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.osgi-3.7.0.v20110613.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.osgi.services-3.3.0.v20110110.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.kernel.authentication-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.kernel.shutdown-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.osgi.console-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.osgi.extensions.equinox-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.osgi.launcher-3.0.2.RELEASE.jar\" org.eclipse.virgo.osgi.launcher.Launcher -config \"C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.kernel.launch.properties\" -Forg.eclipse.virgo.kernel.home=\"C:\\ABC-~1.0-S\" -Forg.eclipse.virgo.kernel.config=\"C:\\ABC-~1.0-S\\config\" -Fosgi.configuration.area=\"C:\\ABC-~1.0-S\\work\\osgi\\configuration\" -Fosgi.java.profile=\"file:C:\\ABC-~1.0-S\\lib\\java6-server.profile\"",
				"\"C:\\Program Files\\Java\\jdk1.6.0_20\\bin\\java\" -Dcom.sun.management.jmxremote.port=9875 -Dcom.sun.management.jmxremote.authenticate=true -Dcom.sun.management.jmxremote.login.config=virgo-kernel -Dcom.sun.management.jmxremote.access.file=\"C:\\ABC-~1.0-S\\config\\org.eclipse.virgo.kernel.jmxremote.access.properties\" -Djavax.net.ssl.keyStore=\"C:\\ABC-~1.0-S\\config\\keystore\" -Djavax.net.ssl.keyStorePassword=abc123 -Dcom.sun.management.jmxremote.ssl=true -Dcom.sun.management.jmxremote.ssl.need.client.auth=false -XX:+HeapDumpOnOutOfMemoryError -XX:ErrorFile=\"C:\\ABC-~1.0-S\\serviceability\\error.log\" -XX:HeapDumpPath=\"C:\\ABC-~1.0-S\\serviceability\\heap_dump.hprof\" -Djava.security.auth.login.config=\"C:\\ABC-~1.0-S\\config\\org.eclipse.virgo.kernel.authentication.config\" -Dorg.eclipse.virgo.kernel.authentication.file=\"C:\\ABC-~1.0-S\\config\\org.eclipse.virgo.kernel.users.properties\" -Djava.io.tmpdir=\"C:\\ABC-~1.0-S\\work\tmp\\\" -Dorg.eclipse.virgo.kernel.home=\"C:\\ABC-~1.0-S\" -Dorg.eclipse.equinox.console.jaas.file=\"C:\\ABC-~1.0-S\\config/store\" -Dssh.server.keystore=\"C:\\ABC-~1.0-S\\config/hostkey.ser\" -Dgosh.args=\"--nointeractive\" -classpath \"C:\\ABC-~1.0-S\\lib\\com.springsource.javax.transaction-1.1.0.jar;C:\\ABC-~1.0-S\\lib\\com.springsource.org.apache.mina.core-2.0.2.jar;C:\\ABC-~1.0-S\\lib\\com.springsource.org.apache.sshd.core-0.5.0.jar;C:\\ABC-~1.0-S\\lib\\com.springsource.slf4j.api-1.6.1.jar;C:\\ABC-~1.0-S\\lib\\org.apache.felix.gogo.runtime-0.8.0.v201107131313.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.equinox.cm-1.0.300.v20101204.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.equinox.console.supportability-1.0.0.201108021516.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.osgi-3.7.0.v20110613.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.osgi.services-3.3.0.v20110110.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.kernel.authentication-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.kernel.shutdown-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.osgi.console-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.osgi.extensions.equinox-3.0.2.RELEASE.jar;C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.osgi.launcher-3.0.2.RELEASE.jar\" org.eclipse.virgo.osgi.launcher.Launcher -config \"C:\\ABC-~1.0-S\\lib\\org.eclipse.virgo.kernel.launch.properties\" -Forg.eclipse.virgo.kernel.home=\"C:\\ABC-~1.0-S\" -Forg.eclipse.virgo.kernel.config=\"C:\\ABC-~1.0-S\\config\" -Fosgi.configuration.area=\"C:\\ABC-~1.0-S\\work\\osgi\\configuration\" -Fosgi.java.profile=\"file:C:\\ABC-~1.0-S\\lib\\java6-server.profile\"",
				"\"java\" -cp \"C:\\Program Files\\yajsw-alpha-9.5\\bat\\/../wrapper.jar\" test.HelloWorld",
				"java -Xrs -jar \"Z:\\dev\\yajsw\\bat\\/..\\wrapper.jar\" -c conf/wrapper.conf       ",
				"java -cp wrapper.jar -Xrs x.Test -c conf/wrapper.conf       ",
				"\"java\" -cp \"C:\\Program Files\\yajsw-alpha-9.5\\bat\\/../wrapper.jar\" test.HelloWorld start \n ",
				"\"java\"  test.HelloWorld",
				"\"C:\\Program Files\\Java\\jre7\\bin\\javaw.exe\" -Xmx512m -jar \"C:\\automa tisation\\bin\\sendfile-server.jar\" abc ",
				"java -jar testJar.jar",
				"java -jar LogConsolidation-1.0.one-jar.jar",
				"java -Dlog4j.debug -Dlog4j.configuration=file:../conf/log4j.xml -jar myApp.jar start",
				"/usr/DAVIDweb/jdk1.6.0_18/bin/java -Dwrapper.teeName=6849389861148562201$1312438311981 -Dwrapper.config=/usr/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3/bin/conf/wrapper.conf -Dwrapper.key=6849389861148562201 -Dwrapper.visible=false -Dwrapper.pidfile=/var/run/wrapper.ApacheTomcatAAADHK3.pid -Dwrapper.port=15003 -Dwrapper.key=6849389861148562201 -Dwrapper.teeName=6849389861148562201$1312438311981 -Dwrapper.tmpPath=/tmp -classpath /usr/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3/bin/wrapper.jar:/usr/DAVIDweb/Tomcat557_AAA-DHK_3/bin/bootstrap.jar -server -Djava.endorsed.dirs=/DAVIDweb/Tomcat557_AAA-DHK_3/common/endorsed -Dcatalina.home=/DAVIDweb/Tomcat557_AAA-DHK_3 -Dcatalina.base=/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3 -Dcatalina.properties=/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3/conf/catalina.properties -Djava.io.tmpdir=/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3/temp -Dibr.debug=false -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3/webapps/AAA-DHK_3/logs -XX:+DisableExplicitGC -Xss1024k -Dibr.dhk.geoinfodok=gid600 -Dibr.dhk.lib=/DAVIDweb/Tomcat557_AAA-DHK_3/AAA-DHK_3/webapps/AAA-DHK_3/WEB-INF/lib/gid600 -Xrs -Dwrapper.service=true -Dwrapper.console.visible=false -Xms512m -Xmx512m org.rzo.yajsw.app.WrapperJVMMain",
				"java -Dsimple.sleepFor=1200 -classpath \";..\\bin\\run.jar;/app/my/dist/runtime.jar;bin\" -DHTTP_PROXY_IP=192.1.21.1  -DHTTP_PROXY_PORT=1211 -Djboss.partition.name:DefaultPartition=app1Cluster -DACCOSA_APP_ROOT=d:/opt/ -DJ2EE_SERVER=JBOSS -DDB_SERVER=db2 -Dcom.APP1.aa.EnableCache=YES -Dcom.APP1.ff.aa.DisableLogging=YES  -DCACHE_TO_USE=\"Memcached\" -DCACHE_SERVER_LIST=\"127.0.0.1:11413 127.0.0.1:11415\" -Dcom.APP1.ff.forceIPAndPort=192.168.1.23_192.168.1.22:3331 -Danother.asdasd=343434_asdasdasd -Danother.asdasd.1=1-343434_asdasdasd -Danother.asdasd.2=2-343434_asdasdasd -Danother.asdasd.3=3-343434_asdasdasd -Danother.asdasd.4=4-343434_asdasdasd -Danother.asdasd.5=5-343434_asdasdasd -Danother.asdasd.6=6-343434_asdasdasd   com.simple.SimpleConsole args_param1 args_param2 args_param3",
				"java -Dsimple.sleepFor=1200 -classpath \";..\\bin\\run.jar;/app/my/dist/runtime.jar;bin\" -DHTTP_PROXY_IP=192.1.21.1  -DHTTP_PROXY_PORT=1211 -Djboss.partition.name:DefaultPartition=app1Cluster -DACCOSA_APP_ROOT=d:/opt/ -DJ2EE_SERVER=JBOSS -DDB_SERVER=db2 -Dcom.APP1.aa.EnableCache=YES -Dcom.APP1.ff.aa.DisableLogging=YES  -DCACHE_TO_USE=\"Memcached\" -DCACHE_SERVER_LIST=\"127.0.0.1:11413 127.0.0.1:11415 \" -Dcom.APP1.ff.forceIPAndPort=192.168.1.23_192.168.1.22:3331 -Danother.asdasd=343434_asdasdasd -Danother.asdasd.1=1-343434_asdasdasd -Danother.asdasd.2=2-343434_asdasdasd -Danother.asdasd.3=3-343434_asdasdasd -Danother.asdasd.4=4-343434_asdasdasd -Danother.asdasd.5=5-343434_asdasdasd -Danother.asdasd.6=6-343434_asdasdasd   com.simple.SimpleConsole args_param1 args_param2 args_param3" };
		for (String cmd : cmds)
		{
			System.out.println("---------------------");
			System.out.println(cmd);
			System.out.println("---------------------");
			JCLParser p = JCLParser.parse(cmd);
			System.out.println(" java:");
			System.out.println(p.getJava());
			System.out.println(" jar:");
			System.out.println(p.getJar());
			System.out.println(" main class:");
			System.out.println(p.getMainClass());
			System.out.println(" args:");
			System.out.println(p.getArgs());
			System.out.println(" classpath:");
			System.out.println(p.getClasspath());
			System.out.println(" options:");
			System.out.println(p.getVmOptions());
		}
	}

}
