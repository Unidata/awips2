package org.rzo.yajsw.os.posix.solaris;

import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.posix.PosixProcessManager;

public class SolarisProcessManager extends PosixProcessManager
{
	public Process createProcess()
	{
		return (Process) new SolarisProcess();
	}

	public Process getProcess(int pid)
	{
		return SolarisProcess.getProcess(pid);
	}

}
