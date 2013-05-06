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
package org.rzo.yajsw.script;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.jboss.netty.util.HashedWheelTimer;
import org.jboss.netty.util.Timeout;
import org.jboss.netty.util.Timer;
import org.jboss.netty.util.TimerTask;
import org.rzo.yajsw.util.DaemonThreadFactory;
import org.rzo.yajsw.wrapper.WrappedProcess;

// TODO: Auto-generated Javadoc
/**
 * The Class AbstractScript.
 */
public abstract class AbstractScript implements Script
{

	/** The _name. */
	String					_name;

	/** The _timeout. */
	int						_timeout	= 30000;

	WrappedProcess			_process;

	String					_id;

	String[]				_args;
	
	final static Timer TIMER = new HashedWheelTimer();
	static final ExecutorService	EXECUTOR		= (ThreadPoolExecutor) new ThreadPoolExecutor(0, 50, 120L, TimeUnit.SECONDS,
			new SynchronousQueue<Runnable>(), new DaemonThreadFactory("scriptExecutorInternal"));
	volatile Future _future;
	volatile Timeout _timerTimeout;



	/**
	 * Instantiates a new abstract script.
	 * 
	 * @param script
	 *            the script
	 * @param timeout
	 */
	public AbstractScript(String script, String id, WrappedProcess process, String[] args, int timeout)
	{
		_name = script;
		_process = process;
		_id = id;
		_args = args;
		if (timeout > 0)
			_timeout = timeout * 1000;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.script.Script#execute(java.lang.String,
	 * java.lang.String, java.lang.String, java.lang.String, java.lang.String,
	 * java.lang.String, java.lang.Object)
	 */
	public abstract Object execute(String line);
	public abstract void interrupt();
	abstract void log(String msg);

	synchronized public void executeWithTimeout(final String line)
	{
		Object result = null;
		_timerTimeout = TIMER.newTimeout(new TimerTask()
		{

			public void run(Timeout arg0) throws Exception
			{
				log("script takes too long -> interrupt");
				try
				{
				interrupt();
				}
				catch (Throwable e)
				{
					
				}
			}
			
		}
		, _timeout, TimeUnit.MILLISECONDS);
		_future = EXECUTOR.submit(new Callable<Object>()
				{
					public Object call()
					{
						Object result = execute(line);
						if (_timerTimeout != null)
							_timerTimeout.cancel();
						_timerTimeout = null;
						return result;
					}
				});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.script.Script#getScript()
	 */
	public String getScript()
	{
		return _name;
	}

	/**
	 * Gets the timeout.
	 * 
	 * @return the timeout
	 */
	public int getTimeout()
	{
		return _timeout;
	}

	/**
	 * Sets the timeout.
	 * 
	 * @param timeout
	 *            the new timeout
	 */
	public void setTimeout(int timeout)
	{
		_timeout = timeout;
	}

}
