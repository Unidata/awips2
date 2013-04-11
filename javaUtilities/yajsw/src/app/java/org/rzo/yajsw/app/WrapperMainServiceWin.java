/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.app;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import jnacontrib.win32.Win32Service;

import org.rzo.yajsw.Constants;
import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.StopableService;
import org.rzo.yajsw.wrapper.WrappedProcess;
import org.rzo.yajsw.wrapper.WrappedProcessFactory;
import org.rzo.yajsw.wrapper.WrappedProcessList;

// TODO: Auto-generated Javadoc
/**
 * The Class WrapperMainService.
 */
public class WrapperMainServiceWin extends Win32Service implements StopableService
{

	/** The w. */
	static volatile WrappedProcessList			wList = new WrappedProcessList();
	static volatile WrappedProcess w;
	static volatile boolean _waitOnStop = false;

	/** The service. */
	static WrapperMainServiceWin	service;
	static ExecutorService			pool	= Executors.newFixedThreadPool(5);
	
	/**
	 * Instantiates a new wrapper main service.
	 */
	public WrapperMainServiceWin()
	{
	}

	// this is the wrapper for services
	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		String wrapperJar = WrapperLoader.getWrapperJar();
		// set home dir of the service to the wrapper jar parent, so that we may find required libs
		String homeDir = new File(wrapperJar).getParent();
		if (!OperatingSystem.instance().setWorkingDir(homeDir))
			System.out.println("could not set working dir. pls check configuration or user rights :"+homeDir);
		YajswConfigurationImpl _config = new YajswConfigurationImpl(false);
		boolean debug = _config.getBoolean("wrapper.debug", false);
		service = new WrapperMainServiceWin();
		// set service shutdown timeout
		service.setServiceName(_config.getString("wrapper.ntservice.name"));
		long timeout = _config.getInt("wrapper.shutdown.timeout", Constants.DEFAULT_SHUTDOWN_TIMEOUT) * 1000;
		timeout += _config.getInt("wrapper.script.STOP.timeout", 0) * 1000;
		timeout += _config.getInt("wrapper.script.SHUTDOWN.timeout", 0) * 1000;
		timeout += _config.getInt("wrapper.script.IDLE.timeout", 0) * 1000;
		timeout += _config.getInt("wrapper.script.ABORT.timeout", 0) * 1000;
		if (timeout > Integer.MAX_VALUE)
			timeout = Integer.MAX_VALUE;
		service.setStopTimeout((int) timeout);
		
		timeout = _config.getInt("wrapper.startup.timeout", Constants.DEFAULT_STARTUP_TIMEOUT) * 1000;
		if (timeout > Integer.MAX_VALUE)
			timeout = Integer.MAX_VALUE;
		service.setStartupTimeout((int) timeout);
		
		service.setAutoReportStartup(_config.getBoolean("wrapper.ntservice.autoreport.startup", true));
		
		if (_config.containsKey("wrapperx.config"))
		{
			List<Object> configs = _config.getList("wrapperx.config");
			wList = WrappedProcessFactory.createProcessList(new HashMap(), configs, true);
			for (WrappedProcess p : wList)
			{
				p.setService(service);
			}
		}
		else
		{
			WrappedProcess w = WrappedProcessFactory.createProcess(_config);
			// set service in wrapper so that we may stop the service in case the application terminates and we need to shutdown the wrapper
			w.setService(service);
			w.init();
			wList.add(w);
		}
		
		w = wList.get(0);

			

		// start the applications
		// the wrapper may have to wait longer for the application to come up ->
		// start the application
		// in a separate thread and then check that the wrapper is up after a
		// max timeout
		// but return as soon as possible to the windows service controller
		final long maxStartTime = w.getMaxStartTime();
		final Future future = pool.submit(new Runnable()
		{
			public void run()
			{
				try
				{
					Thread.yield();
					wList.startAll();
				}
				catch (Throwable ex)
				{
					ex.printStackTrace();
					w.getWrapperLogger().info("Win Service: error starting wrapper " + ex.getMessage());
					Runtime.getRuntime().halt(999);
				}
			}
		});
		pool.execute(new Runnable()
		{
			public void run()
			{
				try
				{
					future.get(maxStartTime, TimeUnit.MILLISECONDS);
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
					w.getWrapperLogger().info("Win Service: wrapper did not start within " + maxStartTime + " ms " + ex.getMessage());
					Runtime.getRuntime().halt(999);
				}
			}
		});
		if (debug)
		w.getWrapperLogger().info("Win service: before service init");
		
		service.setDebug(debug);

		// init the service for signaling with services.exe. app will hang
		// here until service is stopped
		service.init();
		// service has terminated -> halt the wrapper jvm
		if (debug)
		w.getWrapperLogger().info("Win service: terminated correctly");
		try
		{
		if (_config.getBoolean("wrapper.update.auto", false))
		{
			w.update(null, false);
		}
		}
		catch (Exception ex)
		{
			
		}
		Runtime.getRuntime().halt(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see jnacontrib.win32.Win32Service#onStart()
	 */
	@Override
	public void onStart()
	{
		log("onstart");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see jnacontrib.win32.Win32Service#onStop()
	 */
	@Override
	public void onStop()
	{
		// execute in a separate thread so that the controller callback can return
		pool.execute(new Runnable()
		{
			public void run()
			{
			
		try
		{
			w.getWrapperLogger().info("Win service stop - timeout: "+service.getStopTimeout());
			if (w.isHaltAppOnWrapper())
			{
				w.getWrapperLogger().info("Win service wrapper.control -> stopping application");
				// remove the listener, so it does not call System.exit
				wList.removeStateChangeListener(WrappedProcess.STATE_IDLE);
				wList.stopAll(_stopReason);
			}
			wList.shutdown();
			w.getWrapperLogger().info("Win service stop - after shutdown");
			synchronized (waitObject)
			{
				w.getWrapperLogger().info("Win service stop - before notify");
				waitObject.notifyAll();
			}
			w.getWrapperLogger().info("Win service terminated");
		}
		catch (Exception e)
		{
			e.printStackTrace();
			w.getWrapperLogger().throwing(this.getClass().getName(), "error in win service doStop", e);
		}
			}
		});


	}
	
	public void log(String txt)
	{
		if (_debug && w != null && w.getWrapperLogger() != null)
			w.getWrapperLogger().info(txt);
	}

	public void waitOnStop() 
	{
		int i = 0;
		try
		{
		while (!_waitOnStop && i++ < 20)
				Thread.sleep(500);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
			}
		
	}

}
