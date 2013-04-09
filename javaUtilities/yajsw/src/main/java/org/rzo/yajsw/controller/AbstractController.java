package org.rzo.yajsw.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.logging.Logger;

import org.apache.commons.collections.map.MultiValueMap;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.controller.jvm.Controller;
import org.rzo.yajsw.util.DaemonThreadFactory;
import org.rzo.yajsw.wrapper.WrappedProcess;

public abstract class AbstractController implements Constants, Controller
{
	Logger							_logger		= Logger.getLogger(getClass().getName());
	/** The _listeners. */
	protected Map					_listeners	= Collections.synchronizedMap(MultiValueMap.decorate(new HashMap(), HashSet.class));
	/** The _wrapped java process. */
	public WrappedProcess			_wrappedProcess;
	protected static final Executor	executor	= Executors.newCachedThreadPool(new DaemonThreadFactory("controller"));
	/** The _state. */
	public int						_state		= 0;
	/** The _debug. */
	protected boolean				_debug		= false;

	/**
	 * The listener interface for receiving controller events. The class that is
	 * interested in processing a controller event implements this interface,
	 * and the object created with that class is registered with a component
	 * using the component's <code>addControllerListener<code> method. When
	 * the controller event occurs, that object's appropriate
	 * method is invoked.
	 * 
	 * @see ControllerEvent
	 */
	public interface ControllerListener
	{

		/**
		 * Fire.
		 */
		public void fire();
	}

	public AbstractController(WrappedProcess wrappedProcess)
	{
		_wrappedProcess = wrappedProcess;
	}

	public void setDebug(boolean debug)
	{
		_debug = debug;
	}

	public void setLogger(Logger logger)
	{
		_logger = logger;
	}

	public Logger getLog()
	{
		return _logger;
	}

	abstract public boolean start();

	/**
	 * Adds the listener.
	 * 
	 * @param state
	 *            the state
	 * @param listener
	 *            the listener
	 */
	public void addListener(int state, ControllerListener listener)
	{
		_listeners.put(state, listener);
	}

	/**
	 * Handle listeners.
	 * 
	 * @param state
	 *            the state
	 */
	protected void handleListeners(int state)
	{
		// synchronized(_listeners)
		{
			if (_listeners != null)
			{
			Collection<ControllerListener> listeners =  (Collection<ControllerListener>)_listeners.get(state);
			if (listeners == null)
				return;
			listeners = new ArrayList(listeners);
				//synchronized (_listeners)
				{
					for (ControllerListener listener : listeners)
						try
						{
							listener.fire();
						}
						catch (Exception ex)
						{
							ex.printStackTrace();
						}
				}
			}
		}
	}

	/**
	 * Gets the state.
	 * 
	 * @return the state
	 */
	public int getState()
	{
		return _state;
	}

	/**
	 * Sets the state.
	 * 
	 * @param state
	 *            the new state
	 */
	public void setState(int state)
	{
		synchronized (this)
		{
			if (_state == state)
				return;
			if (_debug)
			_logger.info("Controller State: " + stateAsStr(_state) + " -> " + stateAsStr(state));
			_state = state;
		}
		logStateChange(state);
		handleListeners(state);
	}

	public abstract void logStateChange(int state);

	public abstract String stateAsStr(int state);
	
	

}
