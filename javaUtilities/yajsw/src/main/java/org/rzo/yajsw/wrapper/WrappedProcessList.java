package org.rzo.yajsw.wrapper;

import java.util.ArrayList;

public class WrappedProcessList extends ArrayList<WrappedProcess>
{
	public void startAll()
	{
		for (WrappedProcess p : this)
		{
			p.start();
		}
	}

	public void stopAll(String reason)
	{
		for (WrappedProcess p : this)
		{
			p.stop(reason);
		}
	}

	public void onStopWrapper()
	{
		for (WrappedProcess p : this)
		{
			if (p.isHaltAppOnWrapper())
				p.stop();
		}
	}

	public void initAll()
	{
		for (WrappedProcess p : this)
		{
			p.init();
		}
	}

	public void restartAll()
	{
		for (WrappedProcess p : this)
		{
			p.restart();
		}
	}

	public void removeStateChangeListener(int state)
	{
		for (WrappedProcess p : this)
		{
			p.removeStateChangeListener(state);
		}
	}

	public void shutdown()
	{
		for (WrappedProcess p : this)
		{
			p.shutdown();
		}
	}

}
