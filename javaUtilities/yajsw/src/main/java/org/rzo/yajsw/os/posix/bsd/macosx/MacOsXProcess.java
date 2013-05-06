package org.rzo.yajsw.os.posix.bsd.macosx;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import org.rzo.yajsw.io.CyclicBufferFileInputStream;
import org.rzo.yajsw.io.CyclicBufferFilePrintStream;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.posix.PosixProcess;

import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;

public class MacOsXProcess extends PosixProcess
{

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

	/**
	 * Gets the current cpu.
	 * 
	 * @return the current cpu
	 */
	public int getCurrentCpu()
	{
		return -1;
	}

	public int getCurrentThreads()
	{
		return -1;
	}

	/**
	 * Gets the current physical memory.
	 * 
	 * @return the current physical memory
	 */
	public int getCurrentPhysicalMemory()
	{
		return -1;
	}

	/**
	 * Gets the current virtual memory.
	 * 
	 * @return the current virtual memory
	 */
	public int getCurrentVirtualMemory()
	{
		return -1;
	}

	/**
	 * Gets the current page faults.
	 * 
	 * @return the current page faults
	 */
	public int getCurrentPageFaults()
	{
		return -1;
	}

	public boolean start()
	{
		if (_arrCmd == null && _cmd == null)
			return false;
		if (_arrCmd == null)
		{
			_arrCmd = _cmd.split(" ");
			if (_debug)
			log("exec: " + _cmd);
		}
		else
		{
			String cmd = "";
			for (String c : _arrCmd)
				cmd += c + " ";
			if (_debug)
			log("exec:" + cmd);
		}

		log("starting ");

		int pid = 0;
		_exitCode = -2;
		String title = _title == null ? "yajsw" : _title;
		_terminated = false;
		if (_visible)
			setCommand(String.format("xterm -hold -sb -T %1$s -e %2$s", title, getCommand()));

		// System.out.println("exec \n"+getCommand());
		// System.out.println("working dir\n"+getWorkingDir());

		if (_visible)
			_pipeStreams = false;
		/*
		 * // if (_pipeStreams) { CLibrary.INSTANCE.pipe(_inPipe);
		 * CLibrary.INSTANCE.pipe(_outPipe); CLibrary.INSTANCE.pipe(_errPipe);
		 * // System.out.println(_outPipe[0]+" "+_outPipe[1]); }
		 */

		// fork a child process
		if ((pid = CLibrary.INSTANCE.fork()) == 0)
		{
			// System.out.println("afer fork");

			int stdout = getStdOutNo();
			int stderr = getStdErrNo();// CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdErrName()).getPointer(0));
			int stdin = getStdInNo();// CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdInName()).getPointer(0));

			// pipe streams to OS pipes
			// if (_pipeStreams)
			{
				CLibrary.INSTANCE.close(_inPipe[1]);
				moveDescriptor(_inPipe[0], stdin);
				CLibrary.INSTANCE.close(_outPipe[0]);
				moveDescriptor(_outPipe[1], stdout);
				CLibrary.INSTANCE.close(_errPipe[0]);
				moveDescriptor(_errPipe[1], stderr);
			}

			// closeDescriptors();

			// disconect from parent
			CLibrary.INSTANCE.umask(0);
			if (CLibrary.INSTANCE.setsid() < 0)
				CLibrary.INSTANCE.exit(-1);

			// set working dir
			if (getWorkingDir() != null)
				if (CLibrary.INSTANCE.chdir(getWorkingDir()) != 0)
					log("could not set working dir");

			// set priority
			if (_priority == PRIORITY_BELOW_NORMAL)
			{
				if (CLibrary.INSTANCE.nice(1) == -1)
					log("could not set priority ");
			}
			else if (_priority == PRIORITY_LOW)
			{
				if (CLibrary.INSTANCE.nice(2) == -1)
					log("could not set priority ");
			}
			else if (_priority == PRIORITY_ABOVE_NORMAL)
			{
				if (CLibrary.INSTANCE.nice(-1) == -1)
					log("could not set priority ");
			}
			else if (_priority == PRIORITY_HIGH)
			{
				if (CLibrary.INSTANCE.nice(-2) == -1)
					log("could not set priority ");
			}
			if (getUser() != null)
				switchUser(getUser(), getPassword());

			try
			{
				int res = CLibrary.INSTANCE.execvp(_arrCmd[0], _arrCmd);
				int err = Native.getLastError();
				log("errno " + err + " " + CLibrary.INSTANCE.strerror(err));
				log("exec res " + res);
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
			lock = false;
			// CLibrary.INSTANCE.exit(-1);
		} // child code
		else if (pid > 0)
		{
			_pid = pid;
			try
			{
				Thread.sleep(1000);
			}
			catch (InterruptedException e1)
			{
			}

			// or pipe streams to cyclic buffer files
			if (_teeName != null && _tmpPath != null)
			{
				// System.out.println("opening tee streams");
				File f = new File(_tmpPath);
				try
				{
					if (!f.exists())
						f.mkdir();
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
					Thread.currentThread().interrupt();
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
				// System.out.println("- opening tee streams");
			}
			/*
			 * if (!_pipeStreams) {
			 * System.out.println("setting out streams to /dev/null/");
			 * CLibrary.INSTANCE.freopen("/dev/null", "w", _outPipe[0]);
			 * System.out.println("setting err streams to /dev/null/");
			 * CLibrary.INSTANCE.freopen("/dev/null", "w", _errPipe[0]);
			 * //System.out.println("setting streams to /dev/null/");
			 * //CLibrary.INSTANCE.freopen("/dev/null", "r", _inPipe[1]);
			 * System.out.println("- setting streams to /dev/null/"); }
			 */

			// System.out.println("parent");
			if (_pipeStreams && _teeName == null && _tmpPath == null)
			{
				writefd(in_fd, _inPipe[1]);
				writefd(out_fd, _outPipe[0]);
				writefd(err_fd, _errPipe[0]);

				_outputStream = new BufferedOutputStream(new FileOutputStream(in_fd));
				_inputStream = new BufferedInputStream(new FileInputStream(out_fd));
				_errorStream = new BufferedInputStream(new FileInputStream(err_fd));

				CLibrary.INSTANCE.close(_inPipe[0]);
				CLibrary.INSTANCE.close(_outPipe[1]);
				CLibrary.INSTANCE.close(_errPipe[1]);

			}

			if (_cpuAffinity != AFFINITY_UNDEFINED)
			{
				IntByReference affinity = new IntByReference();
				affinity.setValue(_cpuAffinity);
				if (CLibrary.INSTANCE.sched_setaffinity(_pid, 4, affinity) == -1)
					log("error setting affinity");
			}

			executor.execute(new Runnable()
			{

				public void run()
				{
					int r = CLibrary.INSTANCE.waitpid(_pid, status, 0);
					// System.out.println("wait for "+r);
					if (r == _pid)
						_exitCode = status.getValue();
					log("exit code linux process " + _exitCode);
					_terminated = true;
				}

			});

			log("started process " + _pid);
			return true;
		} // parent process
		else if (pid < 0)
		{
			log("failed to fork: " + pid);
			return false;
		}
		return false;

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

		return jvm;
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
		MacOsXProcess result = null;
		result = new MacOsXProcess();
		result.setPid(pid);
		result.setUser(result.getUserInternal());
		result.setCommand(result.getCommandInternal());
		result.setWorkingDir(result.getWorkingDirInternal());
		if (result.getCommand() == null)
			return null;
		return result;
	}

	public void waitFor(long timeout)
	{
		long start = System.currentTimeMillis();
		while (System.currentTimeMillis() - start < timeout)
		{
			if (!isRunning())
				return;
			try
			{
				Thread.sleep(100);
			}
			catch (InterruptedException e)
			{
				if (_logger != null)
					_logger.throwing(PosixProcess.class.getName(), "waitFor", e);
				Thread.currentThread().interrupt();
			}
		}

	}

}
