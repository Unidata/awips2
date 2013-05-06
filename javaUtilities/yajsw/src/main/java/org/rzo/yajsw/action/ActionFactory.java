package org.rzo.yajsw.action;

import java.io.PrintStream;

import org.jboss.netty.channel.Channel;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.controller.Message;

public class ActionFactory
{
	public static Action getAction(Message msg)
	{
		String cls = null;
		if (msg.getCode() == Constants.WRAPPER_MSG_THREAD_DUMP)
		{
			String version = System.getProperty("java.specification.version");
			if (version.startsWith("1.6"))
				cls = "org.rzo.yajsw.action.ThreadDumpImpl6";
			else
				cls = "org.rzo.yajsw.action.ThreadDumpImpl5";
		}
		if (cls != null)
			try
			{
				Class cl = ActionFactory.class.getClassLoader().loadClass(cls);
				return (Action) cl.newInstance();
			}
			catch (Throwable ex)
			{
				ex.printStackTrace();
			}
		return new Action()
		{

			public void execute(Message msg, Channel session, PrintStream out, Object data)
			{
				System.out.println("Error No Action for " + msg);
			}

		};

	}
}
