package org.rzo.yajsw.controller.jvm;

import java.util.logging.Logger;

import org.rzo.yajsw.controller.AbstractController.ControllerListener;

public interface Controller
{

	void setDebug(boolean debug);

	void setLogger(Logger wrapperLogger);

	boolean start();

	void stop(int state, String reason);

	void addListener(int stateStopped, ControllerListener listenerStopped);

	void reset();

	void processStarted();

	void processFailed();
	
	void beginWaitForStartup();

}
