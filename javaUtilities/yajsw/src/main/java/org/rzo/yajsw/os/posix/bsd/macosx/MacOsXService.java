package org.rzo.yajsw.os.posix.bsd.macosx;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.os.AbstractService;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.posix.Utils;
import org.rzo.yajsw.os.posix.VelocityLog;

public class MacOsXService extends AbstractService implements Constants
{
	String	_launchdDir;
	String	_plistTemplate;
	String	_plistFile;
	int		_stopTimeout;
	String	_plistName;

	String	_execCmd;

	String	_confFile;
	Utils	_utils	= new Utils();

	public void init()
	{
		if (_name == null)
		{
			System.out.println("no name for daemon -> abort");
			return;
		}
		_launchdDir = _config.getString("wrapper.launchd.dir", System.getProperty("user.home") + "/Library/LaunchAgents");
		File daemonDir = new File(_launchdDir);
		if (!daemonDir.exists() || !daemonDir.isDirectory())
		{
			System.out.println("Error " + _launchdDir + " : is not a directory");
			return;
		}
		String wrapperJar = WrapperLoader.getWrapperJar().trim();
		String wrapperHome = ".";
		try
		{
			wrapperHome = new File(wrapperJar).getParentFile().getCanonicalPath();
		}
		catch (IOException e1)
		{
			e1.printStackTrace();
		}
		String confFile = _config.getString("wrapper.config");
		String confDir = null;
		if (confFile != null)
		{
			File f = new File(confFile);
			if (f.exists())
				try
				{
					confDir = f.getParentFile().getCanonicalPath();
				}
				catch (IOException e)
				{
				}
		}
		if (confDir == null)
			confDir = wrapperHome + "/conf";
		if (confFile == null)
		{
			System.out.println("no conf file found -> abort");
			return;
		}
		try
		{
			_confFile = new File(confFile).getCanonicalPath();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		_plistTemplate = _config.getString("wrapper.launchd.template", wrapperHome + "/templates/launchd.plist.vm");
		File daemonTemplate = new File(_plistTemplate);
		if (!daemonTemplate.exists() || !daemonTemplate.isFile())
		{
			System.out.println("Error " + _plistTemplate + " : template file not found");
			return;
		}
		File daemonScript = new File(daemonDir, "wrapper." + getName());
		if (daemonScript.exists())
			System.out.println(daemonScript.getAbsolutePath() + " already exists -> overwrite");

		_plistName = "wrapper." + _name;
		File plistFile = new File(_launchdDir, _plistName + ".plist");
		try
		{
			_plistFile = plistFile.getCanonicalPath();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		JavaHome javaHome = OperatingSystem.instance().getJavaHome(_config);
		String java = System.clearProperty("java.home") + "/bin/java";
		try
		{
			java = new File(java).getCanonicalPath();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}

		_execCmd = String.format("%1$s -Dwrapper.service=true -Dwrapper.visible=false -jar %2$s -c %3$s", java, wrapperJar, _confFile);

	}

	public boolean install()
	{
		if (_plistFile == null)
		{
			System.out.println("Error : not initialized -> abort");
			return false;
		}
		try
		{
			File daemonTemplate = new File(_plistTemplate);
			VelocityEngine ve = new VelocityEngine();
			ve.setProperty(VelocityEngine.RESOURCE_LOADER, "file");
			ve.setProperty("file.resource.loader.path", daemonTemplate.getParent());
			ve.setProperty("runtime.log.logsystem.class", VelocityLog.class.getCanonicalName());
			ve.init();
			Template t = ve.getTemplate(daemonTemplate.getName());
			VelocityContext context = new VelocityContext();
			context.put("name", _plistName);
			context.put("command", Arrays.asList(_execCmd.split(" ")));
			context.put("autoStart", "AUTOMATIC".equals(_config.getString("wrapper.ntservice.starttype", DEFAULT_SERVICE_START_TYPE)));
			FileWriter writer = new FileWriter(_plistFile);

			t.merge(context, writer);
			writer.flush();
			writer.close();
			_utils.osCommand("launchctl load " + _plistFile, 5000);

		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			return false;
		}
		return isInstalled();
	}

	public boolean isInstalled()
	{
		String sp = String.format(".*\\d+.*%1$s.*", _plistName);
		Pattern p = Pattern.compile(sp, Pattern.DOTALL);
		String list = _utils.osCommand("launchctl list", 5000);
		Matcher m = p.matcher(list);
		return m.matches();
	}

	public boolean isRunning()
	{
		int pid = getPid();
		return pid > 0;
	}

	public boolean start()
	{
		if (isRunning())
		{
			System.out.println("already running");
			return true;
		}
		_utils.osCommand("launchctl start " + _plistName, 5000);
		return isRunning();
	}

	public boolean stop()
	{
		if (isRunning())
		{
			_utils.osCommand("launchctl stop " + _plistName, 5000);
			return !isRunning();
		}
		return true;
	}

	public boolean uninstall()
	{
		if (isRunning())
			stop();
		_utils.osCommand("launchctl unload " + _plistFile, 5000);
		new File(_plistFile).delete();
		return true;
	}

	public int state()
	{
		int result = 0;
		if (isInstalled())
			result |= STATE_INSTALLED;
		if (isRunning())
			result |= STATE_RUNNING;
		return result;
	}

	public int getPid()
	{
		try
		{
			String sp = String.format("(\\d+)\\s*\\-\\s*%1$s", _plistName);
			Pattern p = Pattern.compile(sp, Pattern.DOTALL);
			String list = _utils.osCommand("launchctl list", 5000);
			Matcher m = p.matcher(list);
			m.find();
			int pid = Integer.parseInt(m.group(1));
			return pid;
		}
		catch (Exception ex)
		{
			// ex.printStackTrace();
		}

		return -1;

	}

	public void setLogger(Logger logger)
	{
		super.setLogger(logger);
		_utils.setLog(logger);
	}

}
