package org.rzo.yajsw.timer;

import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.wrapper.WrappedProcess;

public class TimerFactory
{
	public static Timer createTimer(YajswConfigurationImpl config, WrappedProcess wp)
	{
		try
		{
			return new TimerImpl(config, wp);
		}
		catch (Throwable ex)
		{
		}
		return new DummyTimer();
	}

}
