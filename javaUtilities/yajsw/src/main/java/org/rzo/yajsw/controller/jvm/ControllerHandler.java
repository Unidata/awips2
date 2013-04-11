package org.rzo.yajsw.controller.jvm;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.controller.Message;

@ChannelPipelineCoverage("one")
public class ControllerHandler extends SimpleChannelUpstreamHandler implements Constants
{

	JVMController	_controller;

	ControllerHandler(JVMController controller)
	{
		_controller = controller;
	}

	@Override
	public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
	{
		Message message = (Message) e.getMessage();
		switch (message.getCode())
		{
		case WRAPPER_MSG_KEY:
			// check if JVM sent us correct key
			if (_controller._key.equals(message.getMessage()))
			{
				// we set the channel not in channelConnected, 
				_controller._channel = ctx.getChannel();
				_controller.setState(JVMController.STATE_LOGGED_ON);
				_controller.startupOK();
				ctx.getChannel().write(new Message(Constants.WRAPPER_MSG_OKKEY, "" + _controller._wrappedProcess.getAppPid()));
				if (_controller.isDebug())
					_controller.getLog().info("Correct key");
			}
			// if not: announce it and close session
			else
			{
				if (_controller.isDebug())
					_controller.getLog().info("Wrong key -> closing session");
				ctx.getChannel().write(new Message(Constants.WRAPPER_MSG_BADKEY, null));
				ctx.getChannel().close();
			}
			break;
		case Constants.WRAPPER_MSG_STOP:
			if (_controller._wrappedProcess != null)
				_controller._wrappedProcess.stop("APPLICATION");
			break;

		case Constants.WRAPPER_MSG_STOP_TIMER:
			if (_controller._wrappedProcess != null)
				_controller._wrappedProcess.stopTimer();
			break;

		case Constants.WRAPPER_MSG_RESTART:
			if (_controller._wrappedProcess != null)
				_controller._wrappedProcess.restartInternal();

			break;

		case Constants.WRAPPER_MSG_PING:
			_controller.pingReceived();
			String msg = message.getMessage();
			if (msg != null)
			{
				String[] values = msg.split(";");
				if (values.length == 4)
				try {
					float heap = Float.parseFloat(values[0]);
					long minGC = Long.parseLong(values[1]);
					long fullGC = Long.parseLong(values[2]);
					long heapInBytes = Long.parseLong(values[3]);
					_controller.setHeap(heap, minGC, fullGC, heapInBytes);
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
			}
			break;

		case Constants.WRAPPER_MSG_SERVICE_STARTUP:
			_controller.serviceStartup();
			break;
			
		case Constants.WRAPPER_MSG_STOP_PENDING:
			if (_controller._wrappedProcess != null) {
				_controller._wrappedProcess.signalStopping(Long.valueOf(message.getMessage()));
			}
			break;

		}
	}

	@Override
	public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{

		synchronized (_controller)
		{
		// we accept only one session. if we already have one -> close the
		// new session
		if (_controller._channel != null && _controller._channel != ctx.getChannel())
		{
			if (_controller.isDebug())
				_controller.getLog().info("session already established -> ignore further sessions");
			ctx.getChannel().close();
		}
		else if (_controller._channel == null)
		{
			_controller.setState(JVMController.STATE_ESTABLISHED);
			// a hacker may establish a connection but does not send the key, thus locking the controller for the process.
			// we leave him connected, so he does not keep polling us, but we allow further connections, until we get the key from our app.
			//TODO if we have a real bandit: timeout of connections which do not send the key.
			//_controller._channel = ctx.getChannel();

		}
		}

	}

	@Override
	public void channelDisconnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		synchronized (_controller)
		{
		if (_controller._channel == ctx.getChannel())
		{
			// stop processing outgoing messages
			_controller.workerExecutor.shutdownNow();

			// stop the controller
			_controller._channel = null;
			_controller.setState(JVMController.STATE_WAITING_CLOSED);
			if (_controller.isDebug())
				_controller.getLog().info("session closed -> waiting");
		}
		
		}
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e) throws Exception
	{
		if (_controller.isDebug())
			_controller.getLog().info(e.getCause().getMessage());

	}

}
