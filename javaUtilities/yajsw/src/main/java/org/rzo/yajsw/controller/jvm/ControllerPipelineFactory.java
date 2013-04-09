package org.rzo.yajsw.controller.jvm;

import static org.jboss.netty.channel.Channels.pipeline;

import java.net.InetAddress;
import java.net.UnknownHostException;

import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.handler.codec.frame.DelimiterBasedFrameDecoder;
import org.jboss.netty.handler.codec.frame.Delimiters;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.controller.Message;
import org.rzo.yajsw.nettyutils.ChannelGroupFilter;
import org.rzo.yajsw.nettyutils.Condition;
import org.rzo.yajsw.nettyutils.ConditionFilter;
import org.rzo.yajsw.nettyutils.LoggingFilter;
import org.rzo.yajsw.nettyutils.WhitelistFilter;

class ControllerPipelineFactory implements ChannelPipelineFactory
{

	JVMController	_controller;
	boolean			_debug	= false;

	ControllerPipelineFactory(JVMController controller, boolean debug)
	{
		_controller = controller;
		_debug = debug;
	}

	public ChannelPipeline getPipeline() throws Exception
	{

		ChannelPipeline pipeline = pipeline(); // Note the static import.
		if (_debug)
			pipeline.addLast("logging1", new LoggingFilter(_controller.getLog(), "controller"));

		// allow new connections only if state != LOGGED_ON 
		// and only if state != PROCESS_KILLED
		pipeline.addLast("checkWaiting", new ConditionFilter(new Condition()
		{
			public boolean isOk(ChannelHandlerContext ctx, ChannelEvent e)
			{
				boolean result = true;
				int currentState = _controller.getState();
				if (currentState == JVMController.STATE_LOGGED_ON)
				{
					_controller.getLog().info("app already logged on -> rejecting new connection");
					result = false;
				}
				else if (currentState == JVMController.STATE_PROCESS_KILLED)
				{
					_controller.getLog().info("app not running -> rejecting new connection");
					result = false;
				}
				return result;
			}
		}));

		// create a firewall allowing only localhosts to connect
		WhitelistFilter firewall = new WhitelistFilter();
		try
		{
			firewall.allowAll(InetAddress.getAllByName("127.0.0.1"));
			firewall.allow(InetAddress.getLocalHost());
			pipeline.addLast("firewall", firewall);
		}
		catch (UnknownHostException e)
		{
			_controller.getLog().throwing(JVMController.class.getName(), "start", e);
		}

		// add a framer to split incoming bytes to message chunks
		pipeline.addLast("framer", new DelimiterBasedFrameDecoder(8192, true, Delimiters.nulDelimiter()));

		// add messge codec
		pipeline.addLast("messageEncoder", new MessageEncoder());
		pipeline.addLast("messageDecoder", new MessageDecoder());

		if (_controller.isDebug())
		{
			pipeline.addLast("logging", new LoggingFilter(_controller.getLog(), "controller"));
			_controller.getLog().info("jvm controller set set netty logger");
		}

		// if we found our partner close all other open connections
		pipeline.addLast("removeConnected", new ChannelGroupFilter(new Condition()
		{
			public boolean isOk(ChannelHandlerContext ctx, ChannelEvent e)
			{
				boolean result = false;
				if (e instanceof MessageEvent)
				{
					Message m = (Message) ((MessageEvent) e).getMessage();
					result = m.getCode() == Constants.WRAPPER_MSG_OKKEY;
				}
				return result;
			}
		}));

		// at last add the message handler
		pipeline.addLast("handler", new ControllerHandler(_controller));

		return pipeline;
	}

}
