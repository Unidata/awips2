package org.rzo.yajsw.util;

import java.util.concurrent.ThreadFactory;

public class DaemonThreadFactory implements ThreadFactory
{
	final String				_prefix;
	volatile int					_count		= 0;
	static ThreadGroup	group		= new ThreadGroup("yajsw");
	int					_priority	= Thread.NORM_PRIORITY;

	public DaemonThreadFactory(String prefix)
	{
		_prefix = "yajsw." + prefix + "-";
	}

	public DaemonThreadFactory(String prefix, int priority)
	{
		_prefix = "yajsw." + prefix + "-";
		_priority = priority;
	}

	synchronized public Thread newThread(Runnable r)
	{
		Thread t = new Thread(group, r, _prefix + _count++);
		t.setDaemon(true);
		t.setPriority(_priority);
		return t;
	}
}
