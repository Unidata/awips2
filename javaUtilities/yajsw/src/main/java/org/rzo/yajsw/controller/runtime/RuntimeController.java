package org.rzo.yajsw.controller.runtime;

import java.util.Collections;

import org.apache.commons.collections.MultiHashMap;
import org.rzo.yajsw.controller.AbstractController;
import org.rzo.yajsw.wrapper.WrappedProcess;
import org.rzo.yajsw.wrapper.WrappedRuntimeProcess;

public class RuntimeController extends AbstractController
{
	static final int		STATE_IDLE				= 0;
	static final int		STATE_RUNNING			= 1;
	public static final int	STATE_STOPPED			= 2;
	static final int		STATE_USER_STOP_REQUEST	= 3;
	public static final int	STATE_USER_STOPPED		= 4;
	static final int		STATE_STARTUP_TIMEOUT	= 5;

	public RuntimeController(WrappedProcess process)
	{
		super(process);
	}

	public boolean start()
	{
		return true;
	}

	public void processStarted()
	{
		getLog().info("process started");

		executor.execute(new Runnable()
		{
			public void run()
			{
				getLog().info("process run started");
				_wrappedProcess.setAppReportedReady(true);
				setState(STATE_RUNNING);
				((WrappedRuntimeProcess) _wrappedProcess)._osProcess.waitFor();
				getLog().info("process exited");
				if (_state == STATE_USER_STOP_REQUEST)
					setState(STATE_USER_STOPPED);
				else
					setState(STATE_STOPPED);
				getLog().info("all terminated");
			}
		});

	}

	public void stop(int state, String reason)
	{
		setState(state);
	}

	public void reset()
	{
		_listeners = Collections.synchronizedMap(new MultiHashMap());
		setState(STATE_IDLE);

	}

	public String stateAsStr(int state)
	{
		switch (state)
		{
		case STATE_IDLE:
			return "IDLE";
		case STATE_RUNNING:
			return "RUNNING";
		case STATE_STOPPED:
			return "STOPPED";
		case STATE_USER_STOP_REQUEST:
			return "USER_STOP_REQUEST";
		case STATE_USER_STOPPED:
			return "USER_STOPPED";
		case STATE_STARTUP_TIMEOUT:
			return "STARTUP_TIMEOUT";

		default:
			return "?";

		}
	}

	public void logStateChange(int state)
	{
		if (state == STATE_STARTUP_TIMEOUT)
			getLog().warning("startup of java application timed out. if this is due to server overload consider increasing wrapper.startup.timeout");

	}

	public void processFailed()
	{
		stop(STATE_STOPPED, null);
	}


	public void beginWaitForStartup()
	{
		// TODO Auto-generated method stub
		
	}

}
