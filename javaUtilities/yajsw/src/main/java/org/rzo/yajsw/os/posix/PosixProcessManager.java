package org.rzo.yajsw.os.posix;

import java.util.List;

import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.ProcessManager;
import org.rzo.yajsw.os.TaskList;

public class PosixProcessManager implements ProcessManager
{
	private static ProcessManager	_instance;

	public static synchronized ProcessManager instance()
	{
		if (_instance == null)
			_instance = new PosixProcessManager();
		return _instance;
	}

	public Process createProcess()
	{
		return new PosixProcess();
	}

	public int currentProcessId()
	{
		return PosixProcess.currentProcessId();
	}

	public Process getProcess(int pid)
	{
		return PosixProcess.getProcess(pid);
	}

	public List getProcessTree(int pid)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public int processIdOfActiveWindow()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	public TaskList taskListInstance()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public List getProcessIds()
	{
		// TODO Auto-generated method stub
		return null;
	}

}
