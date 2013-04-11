package org.rzo.yajsw.nettyutils;

import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.handler.logging.LoggingHandler;

@ChannelPipelineCoverage("one")
public class SystemOutLoggingFilter extends LoggingHandler
{
	String	_name;

	public SystemOutLoggingFilter(String name)
	{
		_name = name;
	}

	@Override
	public void log(ChannelEvent e)
	{
		if (e != null)
			log(e.toString());
		else
			log("null event !!");
	}

	private void log(String txt)
	{
			System.out.println("[" + _name + "]" + txt);
	}

}
