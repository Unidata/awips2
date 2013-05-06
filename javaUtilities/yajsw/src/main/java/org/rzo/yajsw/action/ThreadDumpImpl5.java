package org.rzo.yajsw.action;

import java.io.IOException;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.jboss.netty.channel.Channel;
import org.rzo.yajsw.controller.Message;

public class ThreadDumpImpl5 implements Action
{
	public void execute(Message msg, Channel session, PrintStream out, Object data) throws IOException
	{
		Map allThreads = Thread.getAllStackTraces();
		Iterator iterator = allThreads.keySet().iterator();
		StringBuffer stringBuffer = new StringBuffer();
		Set<Long> ids = new HashSet<Long>();
		if (data != null)
			for (long id : (long[])data)
		{
			ids.add(id);
		}
		while (iterator.hasNext())
		{
			Thread key = (Thread) iterator.next();
			if (data != null && !ids.contains(key.getId()))
					continue;
			StackTraceElement[] trace = (StackTraceElement[]) allThreads.get(key);
			stringBuffer.append(key + "\r\n");
			for (int i = 0; i < trace.length; i++)
			{
				stringBuffer.append("  " + trace[i] + "\r\n");
			}
			stringBuffer.append("\r\n");
		}
		out.println(stringBuffer.toString());
		out.flush();
	}

	public static void main(String[] args) throws IOException
	{
		Action a = (Action) new ThreadDumpImpl5();
		a.execute(null, null, System.out, null);
	}

}
