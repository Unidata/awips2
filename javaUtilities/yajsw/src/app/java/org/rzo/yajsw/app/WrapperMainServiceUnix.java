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
public class WrapperMainServiceUnix implements StopableService
{

	/** The w. */
	static WrappedProcess	w;
	static volatile WrappedProcessList			wList = new WrappedProcessList();


	/**
	 * Instantiates a new wrapper main service.
	 */
	public WrapperMainServiceUnix()
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
		String homeDir = new File(wrapperJar).getParent();
		if (!OperatingSystem.instance().setWorkingDir(homeDir))
		{
			System.out.println("could not set working dir. pls check configuration or user rights :"+homeDir);
		}

		StopableService service = new WrapperMainServiceUnix();
		YajswConfigurationImpl _config = new YajswConfigurationImpl(false);
		//w = WrappedProcessFactory.createProcess(_config);
		// start the application
		//w.setDebug(true);
		//w.init();
		//w.setService(service);
		
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

		
		/* use wrapper.control
		Runtime.getRuntime().addShutdownHook(new Thread()
		{
			public void run()
			{
				w.stop();
				w.shutdown();
				// give scripts time to terminate
				try
				{
					Thread.sleep(5000);
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
				}
			}
		});
		*/
		wList.startAll();
	}

	public void onStop()
	{
		// give any running scripts time to terminate
		try
		{
			Thread.sleep(5000);
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void waitOnStop()
	{
		// TODO Auto-generated method stub
		
	}

	public void signalStopping(long waitHint)
	{
		// TODO Auto-generated method stub
		
	}

}
