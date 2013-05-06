package org.rzo.yajsw.os.posix.bsd;

import java.io.File;

import org.rzo.yajsw.os.posix.PosixService;

public class BSDService extends PosixService
{
	protected File getDaemonScript()
	{
		return new File(new File(_daemonDir), "wrapper." + getName() + ".sh");
	}

	protected String getDefaultDaemonDir()
	{
		return "/usr/local/etc/rc.d";
	}

}
