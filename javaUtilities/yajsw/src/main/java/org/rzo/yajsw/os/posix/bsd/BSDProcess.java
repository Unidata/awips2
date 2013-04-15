package org.rzo.yajsw.os.posix.bsd;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.io.CyclicBufferFileInputStream;
import org.rzo.yajsw.io.CyclicBufferFilePrintStream;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.posix.PosixProcess;

import com.sun.jna.FromNativeConverter;
import com.sun.jna.ptr.IntByReference;

public class BSDProcess extends PosixProcess
{
	java.lang.Process	_process;
	
	@Override
	public String getStdInName()
	{
		return "__stdinp";
	}

	@Override
	public String getStdOutName()
	{
		return "__stdoutp";
	}

	@Override
	public String getStdErrName()
	{
		return "__stderrp";
	}


	
	
	private String getDOption(String key, String value)
	{
		// posix: setting quotes does not work (cmd is str array). windows: quotes are set in Process class.
		//if (value != null && !value.contains(" "))
			return "-D"+key+"="+value;
		//else
		//	return "-D"+key+"=\""+value+"\"";
	}


	public boolean start()
	{
		_terminated = false;
		_exitCode = -1;
		ArrayList<String> cmdList = new ArrayList();
		cmdList.add(getCurrentJava());
		String tmpDir = _tmpPath;
		if (tmpDir == null)
			tmpDir = System.getProperty("jna_tmpdir", null);
		if (tmpDir != null)
		{
			String opt = getDOption("jna_tmpdir", tmpDir);
			if (!cmdList.contains(opt))
				cmdList.add(opt);
		}
		cmdList.add("-classpath");
		cmdList.add(getStartClasspath());
		if (_pipeStreams)
			cmdList.add("-Dwrapperx.pipeStreams=true");
		if (_user != null)
			cmdList.add("-Dwrapperx.user=" + _user);
		//if (_password != null)
		//	cmdList.add("-Dwrapperx.password=" + _password);
		String[] xenv = getXEnv();
		cmdList.add(AppStarter.class.getName());
		for (int i = 0; i < _arrCmd.length; i++)
			cmdList.add(_arrCmd[i]);
		String[] cmd = new String[cmdList.size()];
		for (int i = 0; i < cmd.length; i++)
		{
			cmd[i] = cmdList.get(i);
		}
		System.out.flush();

		final java.lang.Process p;

		try
		{
			p = Runtime.getRuntime().exec(cmd, xenv, new File(_workingDir)); 
		}
		catch (IOException e)
		{
			e.printStackTrace();
			_terminated = true;
			return false;
		}
		BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String line;
		try
		{
			do
			{
				line = in.readLine();
				//System.out.println("line: " +line);
				if (line != null && line.contains("PID:"))
				{
					setPid(Integer.parseInt(line.substring(4)));
					if (this._teeName == null)
					   line = null;
					// otherwise the stream is closed by the wrapped app
					// we will continue reading the input stream in the gobbler
				}
				else if (line != null && _debug)
					System.out.println(line);
			}
			while (line != null);
		}
		catch (IOException e)
		{
			e.printStackTrace();
			_terminated = true;
			return false;
		}
		_process = p;
		executor.execute(new Runnable()
		{

			public void run()
			{
				try
				{
					p.waitFor();
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
					Thread.currentThread().interrupt();
				}
				_terminated = true;
				_exitCode = p.exitValue();
				/*
				 * bkowal
				 * Suppress extraneous output unless debug is enabled.
				 */
				if (_debug)
				{
					System.out.println("exit code bsd process " + _exitCode);
				}
				BSDProcess.this.setTerminated(true);
			}

		});

		if (_teeName != null && _tmpPath != null)
		{
			File f = new File(_tmpPath);
			try
			{
				if (!f.exists())
					f.mkdir();
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
			try
			{
				// System.out.println("opening tee streams out");
				_inputStream = new CyclicBufferFileInputStream(createRWfile(_tmpPath, "out_" + _teeName));
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
			try
			{
				// System.out.println("opening tee streams err");
				_errorStream = new CyclicBufferFileInputStream(createRWfile(_tmpPath, "err_" + _teeName));
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
			try
			{
				// System.out.println("opening tee streams in");
				_outputStream = new CyclicBufferFilePrintStream(createRWfile(_tmpPath, "in_" + _teeName));
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}
		if (_pipeStreams && _teeName == null)
		{

			_outputStream = _process.getOutputStream();
			_inputStream = _process.getInputStream();
			_errorStream = _process.getErrorStream();

		}
		if (_cpuAffinity != AFFINITY_UNDEFINED)
		{
			IntByReference affinity = new IntByReference();
			affinity.setValue(_cpuAffinity);
			if (CLibrary.INSTANCE.sched_setaffinity(_pid, 4, affinity) == -1)
				System.out.println("error setting affinity");
		}

		/*
		 * bkowal
		 * Suppress extraneous output unless debug is enabled.
		 */
		if (_debug)
		{
			System.out.println("started process " + _pid);
		}

		return true;
	}

	private String[] getXEnv()
	{
		List<String[]> env = getEnvironment();
		if (env != null && !env.isEmpty())
		{
			String [] result = new String[env.size()];
			int i = 0;
			for (String[] x : env)
			{
				result[i] = x[0]+"="+x[1];
				System.out.println("bsd env "+result[i]);
				i++;
			}
			return result;
		}
		return null;
	}

	private String getStartClasspath()
	{
		String wrapperJar = WrapperLoader.getWrapperJar();
		File wrapperHome = new File(wrapperJar).getParentFile();
		File jnaFile = new File(getJNAJar());
		try
		{
			return wrapperJar + ":" + jnaFile.getCanonicalPath();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return null;
	}

	private String getCurrentJava()
	{
		int myPid = OperatingSystem.instance().processManagerInstance().currentProcessId();
		Process myProcess = OperatingSystem.instance().processManagerInstance().getProcess(myPid);
		String cmd = myProcess.getCommand();
		String jvm = null;
		if (cmd.startsWith("\""))
			jvm = cmd.substring(0, cmd.indexOf("\" ") + 1);
		else
			jvm = cmd.substring(0, cmd.indexOf(" "));

		/*
		 * bkowal
		 * Always return the AWIPS II Java.
		 */
		return "/awips2/java/bin/java";
	}

	public String getCommandInternal()
	{
		if (_pid < 0)
			return null;
		String cmd = String.format("ps -p %1$s -o command", _pid);
		String res = _utils.osCommand(cmd, 5000);
		if (res == null)
			return null;
		String[] resx = res.split(System.getProperty("line.separator"));
		if (resx.length < 2)
			return null;
		return resx[1];
	}

	public String getUserInternal()
	{
		if (_pid < 0)
			return null;
		String cmd = String.format("ps -p %1$s -o user", _pid);
		String res = _utils.osCommand(cmd, 5000);
		if (res == null)
			return null;
		String[] resx = res.split(System.getProperty("line.separator"));
		if (resx.length < 2)
			return null;
		return resx[1];
	}

	public String getWorkingDirInternal()
	{
		if (_pid < 0)
			return null;
		return null;
	}

	public static Process getProcess(int pid)
	{
		BSDProcess result = null;
		result = new BSDProcess();
		result.setPid(pid);
		result.setUser(result.getUserInternal());
		result.setCommand(result.getCommandInternal());
		result.setWorkingDir(result.getWorkingDirInternal());
		if (result.getCommand() == null)
			return null;
		return result;
	}

	public static void main(String[] args)
	{
		BSDProcess p = new BSDProcess();
		System.out.println(p.getCurrentJava());
		p.setCommand(new String[]
		{ "ping", "localhost" });
		p.setPipeStreams(true, false);
		p.start();
		BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String line;
		try
		{
			do
			{
				line = in.readLine();
				System.out.println(line);
			}
			while (line != null);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}

	}
	
	private boolean checkPath(String path)
	{
		int ix = path.indexOf("!");
		if (ix == -1)
		{
			System.out.println("<yajsw>/lib/core/jna/jna-xxx.jar not found, please check classpath. aborting wrapper !");
			//Runtime.getRuntime().halt(999);// -> groovy eclipse plugin crashes
			return false;
		}
		return true;
		
	}

	private String getJNAJar()
	{
		String cn = FromNativeConverter.class.getCanonicalName();
		String rn = cn.replace('.', '/') + ".class";
		String path = ".";
		try
		{
			path = FromNativeConverter.class.getClassLoader().getResource(rn).getPath();
			if (!checkPath(path))
				return null;
			path = path.substring(0, path.indexOf("!"));
			path = new URI(path).getPath();
			path.replaceAll("%20", " ");
			return path;
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		return null;
	}
	


}
