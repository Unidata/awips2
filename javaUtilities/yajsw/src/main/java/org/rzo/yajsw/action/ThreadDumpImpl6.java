package org.rzo.yajsw.action;

import java.io.IOException;
import java.io.PrintStream;
import java.lang.management.ManagementFactory;
import java.lang.management.MonitorInfo;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.jboss.netty.channel.Channel;
import org.rzo.yajsw.controller.Message;

public class ThreadDumpImpl6 implements Action
{
	public void execute(Message msg, Channel session, PrintStream out, Object data) throws IOException
	{
		final ThreadMXBean thbean = ManagementFactory.getThreadMXBean();
		final long[] ids = (long[]) (data == null ? thbean.getAllThreadIds() : data);
		Map threads = Thread.getAllStackTraces();
		Map<Long, Thread> threadIds = new HashMap();
		for (Iterator it = threads.keySet().iterator(); it.hasNext();)
		{
			Thread t = (Thread) it.next();
			threadIds.put(t.getId(), t);
		}

		synchronized (ids)
		{

			ThreadInfo[] infos;
			if (!thbean.isObjectMonitorUsageSupported() || !thbean.isSynchronizerUsageSupported())
				infos = thbean.getThreadInfo(ids);
			else
				infos = thbean.getThreadInfo(ids, true, true);

			for (ThreadInfo info : infos)
			{
				String locked = info.getLockOwnerName();
				String daemon = threadIds.get(info.getThreadId()).isDaemon() ? "DAEMON" : "";
				locked = locked == null ? "" : "locked by " + locked;
				out.println(String.format("%1$s %2$s %5$s - %3$s %4$s", info.getThreadId(), info.getThreadName(), info.getThreadState(), locked,
						daemon));
				MonitorInfo[] monitorInfos = info.getLockedMonitors();
				StackTraceElement[] stackTraceElements = info.getStackTrace();
				int k = 0;
				int i = 0;
				for (StackTraceElement trace : stackTraceElements)
				{
					out.println(String.format("    %1$s", trace));
					if (monitorInfos.length > k && monitorInfos[k].getLockedStackDepth() == i)
					{
						out.println(String.format("        - lock  %2$s@%1$s", monitorInfos[k].getClassName(), Integer.toHexString(monitorInfos[k]
								.getIdentityHashCode())));
						k++;
					}
					i++;
				}

			}
			out.flush();
		}

	}

	public static void main(String[] args) throws IOException
	{
		Action a = (Action) new ThreadDumpImpl6();
		a.execute(null, null, System.out, null);
	}

}
