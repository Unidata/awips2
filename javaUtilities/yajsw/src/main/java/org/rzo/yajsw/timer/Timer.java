package org.rzo.yajsw.timer;

public interface Timer
{

	void init();

	boolean isHasTrigger();

	boolean isTriggered();

	boolean isStartImmediate();

	void start();

	void stop();

}
