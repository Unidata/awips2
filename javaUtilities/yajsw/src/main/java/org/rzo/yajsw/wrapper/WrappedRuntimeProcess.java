package org.rzo.yajsw.wrapper;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.controller.AbstractController.ControllerListener;
import org.rzo.yajsw.controller.runtime.RuntimeController;
import org.rzo.yajsw.os.OperatingSystem;

public class WrappedRuntimeProcess extends AbstractWrappedProcess
{

	@Override
	void configProcess()
	{
		super.configProcess();
		List<String> command = new ArrayList();
		String c = _config.getString("wrapper.image", null);
		if (c == null || c.length() == 0)
		{
			getInternalWrapperLogger().error("wrapper.image not set -> abort");
			return;
		}
		// TODO check if c exists - search in PATH
		command.add(c);
		for (Iterator it = _config.getKeys("wrapper.app.parameter"); it.hasNext();)
		{
			String p = _config.getString((String)it.next());
			if (p != null)
			{
				p = p.trim();
				if (p.length() > 0)
					command.add(p);
			}	
		}
		String[] arrCmd = new String[command.size()];
		for (int i = 0; i < arrCmd.length; i++)
			arrCmd[i] = (String) command.get(i);

		_osProcess.setCommand(arrCmd);
		//_osProcess.setPipeStreams(true, false);
		// set this to false at your own risk.
		boolean pipeStreams = _config.getBoolean("wrapper.console.pipestreams",true);
		_osProcess.setPipeStreams(pipeStreams, pipeStreams);
	}

	@Override
	void postStart()
	{
	}

	@Override
	void stopController(int timeout, String reason)
	{
		_controller.stop(RuntimeController.STATE_USER_STOPPED, reason);
		_osProcess.stop(timeout, 999);
	}

	public boolean reconnect(int pid)
	{
		try
		{
			_osProcess = OperatingSystem.instance().processManagerInstance().getProcess(pid);
			if (!_osProcess.stop(10, 0))
			{
				getWrapperLogger().severe("native processes must be restarted to consume the out and err streams. stopping of process failed");
			}

			// this.start();
		}
		catch (Throwable ex)
		{
			getWrapperLogger().severe(
					"native processes must be restarted to consume the out and err streams. stopping of process failed: " + ex.getMessage());
			ex.printStackTrace();
		}
		return true;
	}

	public void init()
	{
		super.init();
		if (_controller == null)
		{
			_controller = new RuntimeController(this);
			configController();
		}
	}

	private final ControllerListener	listenerStopped	= new ControllerListener()
														{
															public void fire()
															{
																getWrapperLogger().info("listener stopped");
																if (_osProcess.isRunning())
																	stop();
																if (allowRestart() && exitCodeRestart() && !exitCodeShutdown())
																{
																	restartInternal();
																}
																else
																{
																	setState(STATE_IDLE);
																	if (_debug)
																	{
																		getWrapperLogger().info("giving up after " + _restartCount + " retries");
																	}
																}

															}

														};

	void configController()
	{
		_controller.setLogger(getWrapperLogger());
		_controller.addListener(RuntimeController.STATE_STOPPED, listenerStopped);

	}

	public String getType()
	{
		return "Native-" + super.getType();
	}

	public static void main(String[] args)
	{
		WrappedRuntimeProcess p = new WrappedRuntimeProcess();
		Configuration c = p.getLocalConfiguration();
		c.setProperty("wrapper.image", "notepad");// "test.bat");//notepad");//"c:/temp/test.bat");//
		c.setProperty("wrapper.working.dir", "c:/");
		p.init();
		p.start();
		p.waitFor(10000);
		System.out.println("stopping");
		p.stop();
		System.out.println("stopped " + p.getExitCode());
	}

}
