package org.rzo.yajsw.timer;

public class DummyTimer implements Timer
{

	public void init()
	{
	}

	public boolean isHasTrigger()
	{
		return false;
	}

	public boolean isStartImmediate()
	{
		return true;
	}

	public boolean isTriggered()
	{
		return false;
	}

	public void start()
	{
	}

	public void stop()
	{
	}

}
