package org.rzo.yajsw.os.posix.bsd;

import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.posix.PosixProcessManager;

public class BSDProcessManager extends PosixProcessManager
{
	public Process createProcess()
	{
		return (Process) new BSDProcess();
	}

	public Process getProcess(int pid)
	{
		return BSDProcess.getProcess(pid);
	}

}
