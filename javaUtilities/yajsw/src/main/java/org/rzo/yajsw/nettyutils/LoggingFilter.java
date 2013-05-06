package org.rzo.yajsw.nettyutils;

import java.util.logging.Logger;

import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.handler.logging.LoggingHandler;

@ChannelPipelineCoverage("one")
public class LoggingFilter extends LoggingHandler
{
	Logger	_logger;
	String	_name;

	public LoggingFilter(Logger logger, String name)
	{
		_logger = logger;
		_name = name;
	}

	@Override
	public void log(ChannelEvent e)
	{
		if (e instanceof MessageEvent)
		{
			MessageEvent msg = (MessageEvent) e;
			log(msg.toString());
		}
		if (e != null)
			log(e.toString());
		else
			log("null event !!");
	}

	private void log(String txt)
	{
		if (_logger == null)
			System.out.println(txt);
		else
			_logger.fine("[" + _name + "]" + txt);
	}

}
