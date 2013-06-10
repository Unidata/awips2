package org.rzo.yajsw.os.posix.bsd.macosx;

import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.posix.PosixProcessManager;

public class MacOsXProcessManager extends PosixProcessManager
{
	public Process createProcess()
	{
		return (Process) new MacOsXProcess();
	}

	public Process getProcess(int pid)
	{
		return MacOsXProcess.getProcess(pid);
	}

}
