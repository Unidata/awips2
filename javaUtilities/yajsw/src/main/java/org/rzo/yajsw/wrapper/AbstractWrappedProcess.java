package org.rzo.yajsw.wrapper;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;
import javax.management.remote.JMXAuthenticator;
import javax.management.remote.JMXConnectorServer;
import javax.management.remote.JMXConnectorServerFactory;
import javax.management.remote.JMXPrincipal;
import javax.management.remote.JMXServiceURL;
import javax.security.auth.Subject;

import org.apache.commons.collections.MultiMap;
import org.apache.commons.collections.map.MultiValueMap;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.vfs2.FileObject;
import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.JdkLogger2Factory;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.YajswVersion;
import org.rzo.yajsw.action.Action;
import org.rzo.yajsw.action.ActionFactory;
import org.rzo.yajsw.cache.Cache;
import org.rzo.yajsw.condition.Condition;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.controller.Message;
import org.rzo.yajsw.controller.jvm.Controller;
import org.rzo.yajsw.io.CircularBuffer;
import org.rzo.yajsw.log.DateFileHandler;
import org.rzo.yajsw.log.MyFileHandler;
import org.rzo.yajsw.log.MyLogger;
import org.rzo.yajsw.log.PatternFormatter;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.StopableService;
import org.rzo.yajsw.os.ms.win.w32.ClusterNodeChangeListener;
import org.rzo.yajsw.os.posix.bsd.BSDProcess;
import org.rzo.yajsw.script.Script;
import org.rzo.yajsw.script.ScriptFactory;
import org.rzo.yajsw.timer.Timer;
import org.rzo.yajsw.timer.TimerFactory;
import org.rzo.yajsw.tray.WrapperTrayIconFactory;
import org.rzo.yajsw.tray.ahessian.server.AHessianJmxServer;
import org.rzo.yajsw.updater.UpdateAction;
import org.rzo.yajsw.util.CaseInsensitiveMap;
import org.rzo.yajsw.util.DaemonThreadFactory;
import org.rzo.yajsw.util.MyReentrantLock;
import org.rzo.yajsw.util.Utils;
import org.rzo.yajsw.util.VFSUtils;

import com.sun.jna.Platform;
import com.sun.jna.PlatformEx;

public abstract class AbstractWrappedProcess implements WrappedProcess, Constants, AbstractWrappedProcessMBean
{

	/** The _os process. */
	volatile public Process				_osProcess;
	/** The _controller. */
	protected Controller				_controller;
	/** The _debug. */
	protected boolean					_debug						= false;
	/** The _config. */
	protected YajswConfigurationImpl	_config;
	/** The _restarts. */
	// protected int _restarts;
	/** The _gobler_err. */
	volatile protected Gobler			_gobler_err;
	/** The _gobler_in. */
	volatile protected Gobler			_gobler_in;
	/** The Constant executor. */
	protected static final Executor		executor					= Executors.newCachedThreadPool(new DaemonThreadFactory("wrappedProcess"));

	protected static final ThreadPoolExecutor		scriptExecutor				= (ThreadPoolExecutor) Executors.newCachedThreadPool(new DaemonThreadFactory("scriptExecutor"));

	/** The _first restart time. */
	protected long						_firstRestartTime;
	/** The _state. */
	protected volatile int				_state						= STATE_IDLE;
	/** The _startup exit codes. */
	Set									_startupExitCodes			= new HashSet();
	/** The _shutdown exit codes. */
	Set									_shutdownExitCodes			= new HashSet();
	Set									_stopExitCodes				= new HashSet();
	/** The _exit code default restart. */
	boolean								_exitCodeDefaultRestart		= false;
	/** The _local configuration. */
	protected Configuration				_localConfiguration			= new BaseConfiguration();
	/** The _app logger. */
	Logger								_appLogger;
	/** The _tmp path. */
	protected String					_tmpPath;
	/** The _wrapper logger. */
	Logger								_wrapperLogger;
	/** The _wrapper logger name. */
	String								_wrapperLoggerName			= "wrapper";
	/** The _app logger name. */
	String								_appLoggerName;
	String								_appLoggerPid;
	/** The _use system properties. */
	boolean								_useSystemProperties		= true;
	/** The Constant PATHSEP. */
	protected static final String		PATHSEP						= System.getProperty("path.separator");
	/** The _restart count. */
	int									_restartCount;
	int									_totalRestartCount			= 0;
	/** The _timer. */
	Timer								_timer;
	/** The _lock. */
	FileLock							_lock;
	/** The _lock file. */
	File								_lockFile;
	/** The _lock file channel. */
	FileChannel							_lockFileChannel;
	/** The _pid file. */
	File								_pidFile;
	/** The _successful invocation time. */
	long								_successfulInvocationTime;
	MultiMap							_listeners					= MultiValueMap.decorate(new HashMap(), HashSet.class);
	MultiMap							_userListeners				= MultiValueMap.decorate(new HashMap(), HashSet.class);

	String								_triggerLine;
	Condition							_condition;
	Process								_trayIconProcess;
	volatile Date						_appStarted;
	volatile Date						_appStopped;
	Date								_wrapperStarted;
	volatile boolean					_drainActive				= false;
	volatile int						_exitCode					= -99;
	boolean								_haltWrapperOnApp			= false;
	boolean								_haltAppOnWrapper			= false;
	Object								_cluster					= null;
	ClusterNodeChangeListener			_clusterListener;
	boolean								_clusterTriggered			= false;
	Cache								_cache						= null;
	volatile boolean					_exiting					= false;
	volatile TrayIconProxy				_trayIconMessages			= null;
	Script								_restartDelayScript			= null;
	Object								_service					= null;
	volatile boolean					_stopRequested				= false;
	volatile boolean					_startRequested				= false;
	MBeanServer							_mbeanServer				= null;
	AHessianJmxServer					_ahessianServer				= null;
	boolean								_reconnecting				= false;
	boolean								_init						= false;

	/*
	 * bkowal - added constant.
	 */
    public static final int             INFINITE_PROCESS_LOGGING        = -1;
    /*
     * bkowal - switched from 40 to infinite constant
     */
	public static final int				MIN_PROCESS_LINES_TO_LOG	= INFINITE_PROCESS_LOGGING;
	
	List<Thread>						_shutdownHooks = new ArrayList<Thread>();
	
	volatile boolean _stopper = false;
	
	Lock _stoppingHintLock = new MyReentrantLock();
	volatile long _stoppingHint = 0;
	volatile long _stoppingHintSetTime = 0;
	volatile boolean _appReportedReady = false;
	volatile String _stopReason;
	
	JdkLogger2Factory _internalLoggerFactory = null;

	/**
	 * Inits the.
	 */
	public void init()
	{
		if (_init)
			return;
		Map utils = new HashMap();
		utils.put("util", new Utils(this));
		_config = new YajswConfigurationImpl(_localConfiguration, _useSystemProperties, utils);
		getTmpPath();
		getWrapperLogger().warning("YAJSW: "+YajswVersion.YAJSW_VERSION);
		getWrapperLogger().warning("OS   : "+YajswVersion.OS_VERSION);
		getWrapperLogger().warning("JVM  : "+YajswVersion.JAVA_VERSION);

		if (!_config.isLocalFile())
			if (_cache == null)
			{
				_cache = new Cache();
				_cache.load(_config);
			}

		String dbg = _config.getString("wrapper.debug");
		_debug = dbg == null ? false : dbg.equals("true");
		_successfulInvocationTime = _config.getLong("wrapper.successful_invocation_time", DEFAULT_SUCCESSFUL_INVOCATION_TIME) * 1000;

		String control = _config.getString("wrapper.control", DEFAULT_CONTROL);
		if ("TIGHT".equals(control) || "WRAPPER".equals(control))
			_haltWrapperOnApp = true;

		if ("TIGHT".equals(control) || "APPLICATION".equals(control))
			_haltAppOnWrapper = true;

		for (Iterator it = _config.getKeys("wrapper.on_exit"); it.hasNext();)
		{
			String key = (String) it.next();
			String value = _config.getString(key);
			if ("RESTART".equals(value))
			{
				String postfix = key.substring(key.lastIndexOf(".") + 1);
				if ("default".equals(postfix))
					_exitCodeDefaultRestart = true;
				else
					try
					{
						_startupExitCodes.add(Integer.parseInt(postfix));
					}
					catch (Exception ex)
					{
						getWrapperLogger().info("error evaluating " + key + " " + ex.getMessage());
					}

			}
			if ("SHUTDOWN".equals(value))
			{
				String postfix = key.substring(key.lastIndexOf(".") + 1);
				if ("default".equals(postfix))
				// do nothing
				{
				}
				else
					try
					{
						_shutdownExitCodes.add(Integer.parseInt(postfix));
					}
					catch (Exception ex)
					{
						getWrapperLogger().info("error evaluating " + key + " " + ex.getMessage());
					}

			}
			if ("STOP".equals(value))
			{
				String postfix = key.substring(key.lastIndexOf(".") + 1);
				if ("default".equals(postfix))
				// do nothing
				{
				}
				else
					try
					{
						_stopExitCodes.add(Integer.parseInt(postfix));
					}
					catch (Exception ex)
					{
						getWrapperLogger().info("error evaluating " + key + " " + ex.getMessage());
					}

			}
		}

		if (_timer == null)
			_timer = TimerFactory.createTimer(_config, this);
		_timer.init();
		if (_condition == null)
			_condition = new Condition(_config, this, getInternalWrapperLogger());
		_condition.init();
		_restartCount = 0;

		// in case of active triggers control == LOOSE
		if (_timer.isHasTrigger() || _condition.isHasTrigger())
		{
			_haltWrapperOnApp = false;
			// do not halt app on wrapper -> service stop takes too long
			// _haltAppOnWrapper = false;
		}

		// if we need the tray or jmx -> create and register a wrapper mbean
		if (_config.getBoolean("wrapper.tray", false) || _config.getBoolean("wrapper.jmx", false))
			registerMBean();

		// if we need a try -> start asynch hessian jmx remote service & create
		// a tray proxy for displaying messages
		if (_config.getBoolean("wrapper.tray", false))
		{
			startAhessianService();
			_trayIconMessages = new TrayIconProxy();
		}

		// if we are not running as a sevice -> spawn the tray icon as a
		// separate process
		if ((!_reconnecting) && _config.getBoolean("wrapper.tray", false) && _trayIconProcess == null && !isService() && _config.getBoolean("wrapper.tray.spawn_process", true))
		{
			_trayIconProcess = WrapperTrayIconFactory.startTrayIconProcess(_config, getWrapperLogger());
		}

		// if jmx is required -> start jmx rmi remote service
		if (_config.getBoolean("wrapper.jmx", false))
			startJMXRmiService();

		// -> redo if we reload the configuration
		configStateChangeListeners(); 
		configShutdownHook();

		String clusterScript = _config.getString("wrapper.windows.cluster.script", null);
		configClusterScript(clusterScript);
		cleanupTmp();
		_init = true;

	}
	
	private void cleanupTmp()
	{
		if (_config != null && _config.getBoolean("wrapper.cleanup_tmp", true) && (_tmpPath != null))
		{
			File t = new File(_tmpPath);
			if (t.exists())
			{
				cleanupFolder(t);
			}
		}
	}

	private void cleanupFolder(File t)
	{
		File[] files = t.listFiles();
		for (File f : files)
		{
			if (f.getName().startsWith("err_"))
				f.delete();
			else if (f.getName().startsWith("in_"))
			f.delete();
			else if (f.getName().startsWith("out_"))
			f.delete();
		}
	}

	private void configClusterScript(String clusterScript)
	{
		if (clusterScript != null && !"".equals(clusterScript))
		{
			List args = _config.getList("wrapper.windows.cluster.script.args", new ArrayList());
			int timeout = _config.getInt("wrapper.windows.cluster.script.timeout", 0);
			final Script script = ScriptFactory.createScript(clusterScript, "", this, args, getInternalWrapperLogger(), timeout, _config.getString("wrapper.script.encoding"), _config.getBoolean("wrapper.script.reload", false), _debug);
			if (script == null)
				return;
			try
			{
				Class clazz = this.getClass().getClassLoader().loadClass("org.rzo.yajsw.os.ms.win.w32.Cluster");
				_cluster = clazz.newInstance();
				_clusterListener = new ClusterNodeChangeListener()
				{
					public void nodeChanged()
					{
						script.execute();
					}
				};
				Method m = clazz.getMethod("addNodeChangeListener", ClusterNodeChangeListener.class);
				m.invoke(_cluster, _clusterListener);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}

		}
	}

	private void startCluster()
	{
		try
		{
			Class clazz = this.getClass().getClassLoader().loadClass("org.rzo.yajsw.os.ms.win.w32.Cluster");
			_clusterTriggered = true;
			_clusterListener.nodeChanged();
			Method m = clazz.getMethod("start");
			m.invoke(_cluster);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

	}

	private void configShutdownHook()
	{
		// on windows services: shutdown is handeled by the onStop method not by
		// the hook
		if (OperatingSystem.instance().getOperatingSystemName().toLowerCase().startsWith("windows") && isService())
			return;
		if (_haltAppOnWrapper)
		{
			Thread hook = new Thread()
			{
				public void run()
				{
					getWrapperLogger().info("Shutting down Wrapper");
					if (!_exiting)
					{
						setExiting();
						AbstractWrappedProcess.this.stop("WRAPPER SHUTDOWN");
						AbstractWrappedProcess.this.shutdown();
					}
					// give eventually running scripts time to terminate
					try
					{
						Thread.sleep(5000);
					}
					catch (InterruptedException e)
					{
						e.printStackTrace();
						Thread.currentThread().interrupt();
					}
				}

			};
			try
			{
			Runtime.getRuntime().addShutdownHook(hook);
			// remember the hook so that we can remove it to avoid mem leaks
			_shutdownHooks.add(hook);
			}
			catch (IllegalStateException ex)
			{
				// ignore if we are already shutting down the jvm.
				// this may be the case if we are configuring a stopper process from within a shutdown hook.
			}
		}
			

	}

	private void configStateChangeListeners()
	{
		Iterator listenersIterator = _config.getKeys("wrapper.script");
		// remove existing listeners. these may have changed due to config reload
		List<String> listeners = new ArrayList();
		for (Iterator it = listenersIterator; it.hasNext();)
			listeners.add((String)it.next());
		Collections.sort(listeners, new AlphanumComparator());
		_listeners.clear();
		_listeners.putAll(_userListeners);
		for (String key : listeners)
		{
			if (!key.endsWith(".args") && !key.endsWith(".encoding")&& !key.endsWith(".reload")&& !key.endsWith(".timeout"))
			{
				String value = _config.getString(key);
				List args = _config.getList(key + ".args", new ArrayList());
				int timeout = _config.getInt(key + ".timeout", 0);

				String state = key.substring(key.lastIndexOf(".") + 1);
				final Script script = ScriptFactory.createScript(value, state, this, args, getInternalWrapperLogger(), timeout, _config.getString("wrapper.script.encoding"), _config.getBoolean("wrapper.script.reload", false), _debug);
				int iState = toIntState(state);
				if (iState >= 0 && script != null)
					addStateChangeListenerInternal(iState, new StateChangeListener()
					{

						public void stateChange(int newState, int oldState)
						{
							script.executeWithTimeout();
						}

					});
			}
		}

		if (_haltWrapperOnApp)
			addStateChangeListenerInternal(STATE_IDLE, new StateChangeListener()
			{
				public void stateChange(int newState, int oldState)
				{
					if (_exiting)
						return;
					_exiting = true;
					executor.execute(new Runnable()
					{

						public void run()
						{
							if (_stopper)
								return;
							// if this is a service: do not exit here so that we
							// can inform the service controller
							if (!isService())
								System.exit(0);
							else
							{
								Object service = getService();
								if (service != null)
								{
									// windows service
									getWrapperLogger().info("calling onStop");
									((StopableService) service).onStop();
									((StopableService) service).waitOnStop();
									Runtime.getRuntime().halt(0);
								}
								else if (isService() && _haltWrapperOnApp)
								{
									// posix service
									try
									{
										Thread.sleep(5000);
									}
									catch (InterruptedException e)
									{
										// TODO Auto-generated catch block
										e.printStackTrace();
									}
									Runtime.getRuntime().halt(0);
								}
							}
						}

					});
				}
			});

	}

	private int toIntState(String state)
	{
		if ("START".equals(state))
			return STATE_STARTING;
		else if ("RUN".equals(state))
			return STATE_RUNNING;
		else if ("RESTART".equals(state))
			return STATE_RESTART;
		else if ("STOP".equals(state))
			return STATE_USER_STOP;
		else if ("ABORT".equals(state))
			return STATE_ABORT;
		else if ("SHUTDOWN".equals(state))
			return STATE_SHUTDOWN;
		else if ("IDLE".equals(state))
			return STATE_IDLE;
		else
			return -1;
	}

	private void startJMXRmiService()
	{
		try
		{
			int port = _config.getInt("wrapper.jmx.rmi.port", Constants.DEFAULT_RMI_PORT);
			if (port > 0)
			{
				Registry rmiRegistry = LocateRegistry.createRegistry(port);
				JMXServiceURL url = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://localhost:" + port + "/server");
				Map environment = null;
				if (_config.getString("wrapper.jmx.rmi.user", null) != null)
				{
					final String myUser = _config.getString("wrapper.jmx.rmi.user");
					final String myPassword = _config.getString("wrapper.jmx.rmi.password", "");
					environment = new HashMap();
					JMXAuthenticator authenticator = new JMXAuthenticator()
					{

						public Subject authenticate(Object credentials)
						{
							if (!(credentials instanceof String[]))
								throw new SecurityException("Bad credentials");
							String[] creds = (String[]) credentials;
							if (creds.length != 2)
								throw new SecurityException("Bad credentials");

							String user = creds[0];
							String password = creds[1];

							if (password == null)
								password = "";

							if (!myUser.equals(user))
								throw new SecurityException("Unknown user " + user);
							if (!myPassword.equals(password))
								throw new SecurityException("Bad password");

							Set principals = new HashSet();
							principals.add(new JMXPrincipal(user));
							return new Subject(true, principals, Collections.EMPTY_SET, Collections.EMPTY_SET);
						}

					};
					environment.put(JMXConnectorServer.AUTHENTICATOR, authenticator);
				}

				JMXConnectorServer cs = JMXConnectorServerFactory.newJMXConnectorServer(url, environment, _mbeanServer);
				cs.start();

			}
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}

	}

	private void startAhessianService()
	{
		if (_ahessianServer != null)
			return;
		String canonName;
		try
		{
			canonName = new File(_config.getString("wrapper.config")).getCanonicalPath();
			_ahessianServer = new AHessianJmxServer(_mbeanServer, "+n:localhost, -n:*", canonName, _config.getInt("wrapper.tray.port", 0), getWrapperLogger());
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void registerMBean()
	{
		ArrayList servers = MBeanServerFactory.findMBeanServer(null);
		try
		{
			if (servers != null && servers.size() > 0)
				_mbeanServer = (MBeanServer) servers.get(0);
			if (_mbeanServer == null)
				_mbeanServer = MBeanServerFactory.createMBeanServer();
			if (_mbeanServer != null)
			{
				String name = _config.getString("wrapper.console.title");
				if (name == null)
					name = _config.getString("wrapper.ntservice.name");
				if (name == null)
					name = "yajsw.noname";
				ObjectName oName = new ObjectName("org.rzo.yajsw", "name", name);
				_mbeanServer.registerMBean(this, oName);
			}
			else
				getWrapperLogger().severe("ERROR: no mbean server found ");
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

	}

	/**
	 * Gets the priority.
	 * 
	 * @param priority
	 *            the priority
	 * 
	 * @return the priority
	 */
	protected int getPriority(String priority)
	{

		if ("LOW".equals(priority))
			return Process.PRIORITY_LOW;
		else if ("BELOW_NORMAL".equals(priority))
			return Process.PRIORITY_BELOW_NORMAL;
		else if ("NORMAL".equals(priority))
			return Process.PRIORITY_NORMAL;
		else if ("ABOVE_NORMAL".equals(priority))
			return Process.PRIORITY_ABOVE_NORMAL;
		else if ("HIGH".equals(priority))
			return Process.PRIORITY_HIGH;
		return Process.PRIORITY_UNDEFINED;
	}

	/**
	 * Gets the affinity.
	 * 
	 * @param affinity
	 *            the affinity
	 * 
	 * @return the affinity
	 */
	protected int getAffinity(String affinity)
	{
		try
		{
			return Integer.parseInt(affinity);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			return 0;
		}
	}

	/**
	 * Exit code restart.
	 * 
	 * @return true, if successful
	 */
	protected boolean exitCodeRestart()
	{
		if (_state == STATE_USER_STOP || _state == STATE_SHUTDOWN)
			return false;
		if (_startupExitCodes.contains(_osProcess.getExitCode()))
		{
			getWrapperLogger().info("restart process due to exit code rule");
			return true;
		}
		else if (_exitCodeDefaultRestart)
		{
			getWrapperLogger().info("restart process due to default exit code rule");
			return true;
		}

		return false;
	}

	/**
	 * Exit code shutdown.
	 * 
	 * @return true, if successful
	 */
	protected boolean exitCodeShutdown()
	{
		if (_shutdownExitCodes.contains(_osProcess.getExitCode()))
		{
			getWrapperLogger().info("shutdown wrapper due to exit code rule");
			return true;
		}

		return false;
	}

	protected boolean exitCodeStop()
	{
		if (_stopExitCodes.contains(_osProcess.getExitCode()))
		{
			getWrapperLogger().info("stop process due to exit code rule");
			return true;
		}

		return false;
	}

	/**
	 * Sets the state.
	 * 
	 * @param state
	 *            the new state
	 */
	protected void setState(int state)
	{
		int oldState = _state;
		if (_state != state)
		{
			if (_debug)
			getWrapperLogger().info("set state " + getStringState(_state) + "->" + getStringState(state));
			_state = state;
			// unset user color so we can display the state color
			// if user wants other color for state, he can set it in a script.
			if (_trayIconMessages != null)
			{
				_trayIconMessages.setUserColor(null);
				if (state == STATE_IDLE)
					_trayIconMessages.messages.clear();
			}
			if (state == STATE_IDLE)
			{
				removeLockFile();
			}
			Collection listeners = (Collection) _listeners.get(_state);
			Collection listeners999 = (Collection) _listeners.get(999);
			Collection allListeners = new HashSet();
			if (listeners != null)
				allListeners.addAll(listeners);
			if (listeners999 != null)
				allListeners.addAll(listeners999);

			for (Iterator it = allListeners.iterator(); it.hasNext();)
			{
				StateChangeListener listener = (StateChangeListener) it.next();
				if (listener != null)
					listener.stateChange(state, oldState);
			}
		if (_state == STATE_IDLE && _startRequested)
		{
			executor.execute(new Runnable()
			{
				public void run()
				{
					start();
				}
			});
			
		}
		}
	}

	/**
	 * String state.
	 * 
	 * @param state
	 *            the state
	 * 
	 * @return the string
	 */
	static public String getStringState(int state)
	{
		switch (state)
		{
		case STATE_IDLE:
			return "IDLE";
		case STATE_RESTART:
			return "RESTART";
		case STATE_RESTART_START:
			return "RESTART_START";
		case STATE_RESTART_STOP:
			return "RESTART_STOP";
		case STATE_RESTART_WAIT:
			return "RESTART_WAIT";
		case STATE_RUNNING:
			return "RUNNING";
		case STATE_STARTING:
			return "STARTING";
		case STATE_STOP:
			return "STOP";
		case STATE_USER_STOP:
			return "STATE_USER_STOP";
		case STATE_ABORT:
			return "STATE_ABORT";
		case STATE_SHUTDOWN:
			return "STATE_SHUTDOWN";
		default:
			return "?";
		}
	}

	/** The _start by timer. */
	boolean	_startByTimer	= false;

	/**
	 * Start by timer.
	 */
	public synchronized void startByTimer()
	{
		_startByTimer = true;
		start();
		_startByTimer = false;
	}

	public synchronized void start()
	{
		if (!_init)
			init();
		if (_debug)
			getWrapperLogger().info("start from Thread " + Thread.currentThread().getName());

		_startRequested = true;
		_stopRequested = false;
		if (_state != STATE_IDLE)
		{
			getWrapperLogger().info("Process not IDLE -> Delaying start request");
		}
		startInternal();
	}

	/**
	 * Start.
	 */
	public synchronized void startInternal()
	{
		setAppReportedReady(false);
		if (!saveLockFile())
			return;
		savePidFile();
		cleanupTmp();
		if (_timer.isHasTrigger() && !_timer.isTriggered())
			_timer.start();
		if (_condition.isHasTrigger() && !_condition.isTriggered())
		{
			_condition.start();
			return;
		}
		if (!_timer.isStartImmediate() && !_startByTimer)
			return;

		if (_cluster != null && !_clusterTriggered)
		{
			startCluster();
			return;
		}

		_startRequested = false;
		if (_state == STATE_IDLE)
			setState(STATE_STARTING);
		else if (_state == STATE_RESTART_WAIT)
			setState(STATE_RESTART_START);
		else
			return;

		if (_shutdownHooks.isEmpty())
			configShutdownHook();

		long startTimeout = _config.getLong("wrapper.startup.delay", 0);
		if (_state == STATE_STARTING && startTimeout > 0)
		{
			try
			{
				getWrapperLogger().info("startup delay " + startTimeout + "sec");
				Thread.sleep(startTimeout * 1000);
				if (_stopRequested)
				{
					setState(STATE_IDLE);
					return;
				}
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		}

		if (_debug)
			getWrapperLogger().info("starting Process");

		if (_wrapperStarted == null)
			_wrapperStarted = new Date();

		if (_config.getBoolean("wrapper.restart.reload_configuration", DEFAULT_RELOAD_CONFIGURATION))
		{
			reloadConfiguration();
			configStateChangeListeners();
		}

		if (_debug)
			getWrapperLogger().info("starting controller");
		if (_controller != null)
		{
			// release resources if controller was running before this call to
			// start
			// otherwise resources will be released by gc -> finalize
			_controller.reset();
			_controller.setDebug(isDebug());
			_controller.setLogger(getWrapperLogger());
			configController();
			if (!_controller.start())
			{
				getWrapperLogger().info("could not start controller -> abort");
				setState(STATE_ABORT);
				setState(STATE_IDLE);
				return;
			}
			else if (_debug)
				getWrapperLogger().info("controller started");

			try
			{
				Thread.sleep(100);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		}
		if (_osProcess == null)
		{
			createOSProcess();
		}
		else
		{
			// release resources if _osProcess was running before this call to
			// start
			// otherwise resources will be released by gc -> finalize
			if (_debug)
				getWrapperLogger().info("_osProcess destroyed");
			_osProcess.destroy();
		}
		configProcess();
		_firstRestartTime = System.currentTimeMillis();
		// _restartCount++;
		Map triggerActions = getTriggerActions();
		Map regexTriggerActions = getRegexTriggerActions();
		Map missingTriggerActions = getMissingTriggerActions();
		Map missingRegexTriggerActions = getMissingRegexTriggerActions();
		_osProcess.setLogger(getWrapperLogger());
		_exitCode = -3;
		if (_debug)
			getWrapperLogger().info("spawning wrapped process");
		_controller.beginWaitForStartup();
		if (_osProcess.start())
		{
			_controller.processStarted();
			_totalRestartCount++;
			postStart();
			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_debug)
			{
				getWrapperLogger().info("started process with pid " + _osProcess.getPid());
			}
			if (pipeStreams())
			{

				_gobler_in = new Gobler(_osProcess.getInputStream(), getAppLogger(), triggerActions, regexTriggerActions, missingTriggerActions,
						missingRegexTriggerActions, "OUTPUT " + _osProcess.getPid(), _osProcess.getPid());
				_gobler_err = new Gobler(_osProcess.getErrorStream(), getAppLogger(), triggerActions, regexTriggerActions, missingTriggerActions,
						missingRegexTriggerActions, "ERROR " + _osProcess.getPid(), _osProcess.getPid());
				executor.execute(_gobler_err);
				executor.execute(_gobler_in);
			}
			if (getState() != STATE_IDLE && getState() != STATE_RESTART)
			{
				_appStarted = new Date();
				setState(STATE_RUNNING);
				updateAppLoggerName();
			}
		}
		else
		{
			getWrapperLogger().severe("failed to spawn wrapped process");
			_controller.processFailed();
		}

		// win 64 test
		// WindowsXPProcess.getProcess(getPid());

	}

	protected void reloadConfiguration()
	{
		{
			Map utils = new HashMap();
			utils.put("util", new Utils(this));
			_config = new YajswConfigurationImpl(_localConfiguration, _useSystemProperties, utils);

			getWrapperLogger().info("reloaded configuration ");
		}
	}

	abstract void configController();

	abstract void postStart();

	void configProcess()
	{
		String priority = _config.getString("wrapper.priority");
		if (priority != null)
			_osProcess.setPriority(getPriority(priority));

		String affinity = _config.getString("wrapper.affinity");
		if (affinity != null)
			_osProcess.setCpuAffinity(getAffinity(affinity));

		String title = _config.getString("wrapper.console.title");
		if (title != null)
			_osProcess.setTitle(title);

		_osProcess.setVisible(_config.getBoolean("wrapper.console.visible", Constants.DEFAULT_CONSOLE_VISIBLE)
				&& !_config.getBoolean("wrapper.service", false));
		_osProcess.setMinimized(_config.getBoolean("wrapper.console.minimized", Constants.DEFAULT_CONSOLE_MINIMIZED)
				&& !_config.getBoolean("wrapper.service", false));
		_osProcess.setUser(_config.getString("wrapper.app.account"));
		_osProcess.setPassword(_config.getString("wrapper.app.password"));

		_osProcess.setDebug(_debug);

		String workingDir = _config.getString("wrapper.working.dir", ".");
		if (workingDir != null)
		{
			File wd = new File(workingDir);
			if (!wd.exists() || !wd.isDirectory())
				getWrapperLogger().warning("working directory " + workingDir + " not found");
			else
				_osProcess.setWorkingDir(wd.getAbsolutePath());
			getWrapperLogger().info("working dir " + wd.getAbsolutePath());
		}
		_osProcess.setEnvironment(getProcessEnvironment(_config));
		if (Platform.isWindows() && PlatformEx.isWinVista() && _config.getBoolean("wrapper.service", false) && _config.getBoolean("wrapper.ntservice.logon_active_session", false))
		{
			_osProcess.setLogonActiveSession(true);
			if (_debug)
				getWrapperLogger().info("setLogonActiveSession");
			if (!_config.getBoolean("wrapper.ntservice.autoreport.startup", true))
				getWrapperLogger().warning("WARNING: do not set autoreport.startup & wrapper.ntservice.logon_active_session");
			if (_config.getString("wrapper.ntservice.account", null) != null)
				getWrapperLogger().warning("WARNING: do not set wrapper.ntservice.account & wrapper.ntservice.logon_active_session");
			if (_config.getString("wrapper.app.account", null) != null)
				getWrapperLogger().warning("WARNING: do not set wrapper.app.account & wrapper.ntservice.logon_active_session");
		}
		String desktop = _config.getString("wrapper.ntservice.desktop", null);
		if (Platform.isWindows() && PlatformEx.isWinVista() && _config.getBoolean("wrapper.service", false) && desktop != null)
			_osProcess.setDesktop(desktop);

			

	}
	
	private List<String[]> getProcessEnvironment(YajswConfigurationImpl config)
	{
		// if user did not set env properties: use default.
		if (!config.getKeys("wrapper.app.env").hasNext())
		{
			return null;
		}
		// get env. of this process from java
		Map<String, String> jEnv = (Map<String, String>) (Platform.isWindows() ? new CaseInsensitiveMap(System.getenv()) : new HashMap<String, String>(System.getenv()));
		// overwrite with user settings
		for (Iterator keys = config.getKeys("wrapper.app.env"); keys.hasNext();)
		{
			String key = (String) keys.next();
			String value = config.getString(key);
			jEnv.put(key.substring("wrapper.app.env.".length()), value);
		}
		// change to string pair format
		List<String[]> result = new ArrayList<String[]>();
		for (Entry<String, String> entry : jEnv.entrySet())
		{
			result.add(new String[]{entry.getKey(), entry.getValue()});
		}
		return result;
	}

/*	private List<String[]> getProcessEnvironment(YajswConfigurationImpl config)
	{
		if (!config.getKeys("wrapper.app.env").hasNext())
		{
			//getWrapperLogger().info("env: no yajsw env");
			return null;
		}
		List<String[]> env = OperatingSystem.instance().processManagerInstance().getProcess(
				OperatingSystem.instance().processManagerInstance().currentProcessId()).getEnvironment();
		for (Iterator keys = config.getKeys("wrapper.app.env"); keys.hasNext();)
		{
			String key = (String) keys.next();
			String value = config.getString(key);
			String envKey = key.substring("wrapper.app.env.".length());
			//getWrapperLogger().info("env: "+envKey+"="+value);
			updateEnvKey(envKey, value, env);
		}
		return env;
	}

	private void updateEnvKey(String envKey, String value, List<String[]> env)
	{
		String[] entry = findEnvEntry(envKey, env);
		if (entry != null)
			entry[1] = value;
		else
			env.add(new String[]
			{ envKey, value });
	}

	private String[] findEnvEntry(String envKey, List<String[]> env)
	{
		for (String[] entry : env)
			if (envKeyEqual(envKey, entry[0]))
				return entry;
		return null;
	}

	private boolean envKeyEqual(String envKey1, String envKey2)
	{
		if (Platform.isWindows())
			return envKey1.toLowerCase().equals(envKey2.toLowerCase());
		else
			return envKey1.equals(envKey2);

	}
	*/

	/**
	 * Pipe streams.
	 * 
	 * @return true, if successful
	 */
	protected boolean pipeStreams()
	{
		return _config.getBoolean("wrapper.console.pipestreams",false);

	}

	/** The _file handler. */
	Handler	_fileHandler;
	/** The _console handler. */
	Handler	_consoleHandler;

	/**
	 * Restart log file.
	 */
	void restartLogFile()
	{
		if (_fileHandler == null)
			return;
		String rollMode = _config.getString("wrapper.logfile.rollmode", "");
		boolean append = !(rollMode.contains("WRAPPER") || rollMode.contains("JVM"));
		if (!append && _appLogger != null)
		{
			_fileHandler.close();
			getFileHandler();
		}
	}

	/**
	 * Gets the console handler.
	 * 
	 * @return the console handler
	 */
	public Handler getConsoleHandler()
	{
		if (_consoleHandler != null)
			return _consoleHandler;

		String consoleLogLevel = _config.getString("wrapper.console.loglevel", "INFO");
		if (consoleLogLevel.equals("NONE"))
			return null;

		// per default java console handler uses err -> use out instead
		_consoleHandler = new ConsoleHandler()
		{
			protected synchronized void setOutputStream(OutputStream out) throws SecurityException
			{
				super.setOutputStream(System.out);
			}
		};
		_consoleHandler.setFormatter(getConsoleFormatter());
		_consoleHandler.setLevel(getLogLevel(consoleLogLevel));
		
		String encoding = _config.getString("wrapper.log.encoding");
		if (encoding != null)
			try
			{
				_consoleHandler.setEncoding(encoding);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}


		return _consoleHandler;
	}

	/**
	 * Gets the file handler.
	 * 
	 * @return the file handler
	 */
	public Handler getFileHandler()
	{
		if (_fileHandler != null)
			return _fileHandler;

		String fileName = getLogFile();
		String fileLogLevel = _config.getString("wrapper.logfile.loglevel", "INFO");
		if ((fileName.equals("") || fileLogLevel.equals("NONE")))
			return null;

		/* parent folder will be created when creating the handler
		File f = new File(fileName).getParentFile();
		if (!f.exists())
			try
			{
				f.mkdirs();
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
			*/

		try
		{
			String rollMode = _config.getString("wrapper.logfile.rollmode", "");
			boolean append = !(rollMode.contains("WRAPPER") || rollMode.contains("JVM"));
			int count = _config.getInt("wrapper.logfile.maxfiles", 0);
			int limit = getLogLimit();
			if (count == 0 && limit > 0)
				count = 16192;
			else if (count == 0)
				count = 1;
			boolean rollDate = "DATE".equals(rollMode);
			String encoding = _config.getString("wrapper.log.encoding");
			int maxDays = _config.getInt("wrapper.logfile.maxdays", -1);
			_fileHandler = fileName.contains("%d") ? new DateFileHandler(fileName, limit, count, append, rollDate, getFileFormatter(), getLogLevel(fileLogLevel), encoding, maxDays) : new MyFileHandler(fileName, limit, count,
					append, getFileFormatter(), getLogLevel(fileLogLevel), encoding);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		return _fileHandler;

	}

	/**
	 * Update app logger name.
	 */
	private void updateAppLoggerName()
	{
		if (_appLogger == null)
			return;
		_appLoggerPid = getAppPid() + "/" + _restartCount;
		((MyLogger) _appLogger).setPID(_appLoggerPid);
		_appLoggerName = getName() == null ? "" : getName();
		((MyLogger) _appLogger).setName(_appLoggerName);
	}

	/**
	 * Gets the app logger.
	 * 
	 * @return the app logger
	 */
	protected Logger getAppLogger()
	{
		if (_appLogger != null)
			return _appLogger;

		if (_appLogger == null)
		{
			_appLogger = new MyLogger();
			updateAppLoggerName();
			_appLogger.setUseParentHandlers(false);
			if (_controller != null)
				_controller.setLogger(_appLogger);
		}
		if (getFileHandler() != null)
			_appLogger.addHandler(getFileHandler());
		if (getConsoleHandler() != null)
			_appLogger.addHandler(getConsoleHandler());
		_appLogger.setLevel(Level.ALL);
		return _appLogger;
	}

	/**
	 * Gets the log level.
	 * 
	 * @param logLevel
	 *            the log level
	 * 
	 * @return the log level
	 */
	private Level getLogLevel(String logLevel)
	{
		if (logLevel.equals("INFO"))
			return Level.ALL;
		else if (logLevel.equals("FATAL"))
			return Level.SEVERE;
		else if (logLevel.equals("ERROR"))
			return Level.WARNING;
		else if (logLevel.equals("STATUS"))
			return Level.INFO;

		return Level.INFO;
	}

	/**
	 * Gets the log limit.
	 * 
	 * @return the log limit
	 */
	private int getLogLimit()
	{
		String res = _config.getString("wrapper.logfile.maxsize");
		String units = "";
		if (res == null)
			return 0;
		res = res.toLowerCase();
		if (res.endsWith("m"))
		{
			res = res.substring(0, res.length() - 1);
			units = "m";
		}
		else if (res.endsWith("k"))
		{
			res = res.substring(0, res.length() - 1);
			units = "k";
		}
		int result = Integer.parseInt(res);
		if (units.equals("m"))
			result = result * 1024 * 1024;
		else if (units.equals("k"))
			result = result * 1024;
		return result;
	}

	/**
	 * Gets the file formatter.
	 * 
	 * @return the file formatter
	 */
	private PatternFormatter getFileFormatter()
	{
		String wFormat = _config.getString("wrapper.logfile.format", Constants.DEFAULT_LOG_FORMAT);
		return getFormatter(wFormat);

	}

	/**
	 * Gets the console formatter.
	 * 
	 * @return the console formatter
	 */
	private java.util.logging.Formatter getConsoleFormatter()
	{
		String wFormat = _config.getString("wrapper.console.format", Constants.DEFAULT_LOG_FORMAT);
		return getFormatter(wFormat);
	}

	/**
	 * Gets the formatter.
	 * 
	 * @param wFormat
	 *            the w format
	 * 
	 * @return the formatter
	 */
	private PatternFormatter getFormatter(String wFormat)
	{
		PatternFormatter formatter = new PatternFormatter();
		String pattern = "";
		if (wFormat.contains("Z"))
		{
			formatter.setTimeFormat("yy-MM-dd HH:mm:ss.SS");
		}
		else
		{
			formatter.setTimeFormat("yy-MM-dd HH:mm:ss");
		}
		for (int i = 0; i < wFormat.length(); i++)
		{
			char c = wFormat.charAt(i);
			if (i > 0 && c != '\r' && c != '\n')
				pattern += "|";
			switch (c)
			{
			case 'L':
				pattern += "%LEVEL%";
				break;
			case 'P':
				pattern += "%PARAM0%";
				break;
			case 'N':
				pattern += "%PARAM1%";
				break;
			case 'T':
			case 'Z':
				pattern += "%TIME%";
				break;
			case 'M':
				pattern += "%MESSAGE%";
				break;
			default:
				;
			}
		}
		if (wFormat.endsWith("\r\n"))
			pattern += "\r\n";
		else if (wFormat.endsWith("\r"))
			pattern += "\r";
		else
			pattern += System.getProperty("line.separator");
		formatter.setLogPattern(pattern);
		return formatter;
	}

	/**
	 * Gets the log file.
	 * 
	 * @return the log file
	 */
	private String getLogFile()
	{
		String result = _config.getString("wrapper.logfile", "log/wrapper.log");
		File r = new File(result);
		File f = null;
		if (!r.isAbsolute())
		{
			String wDir = _config.getString("wrapper.working.dir", ".");
			f = new File(wDir, result);
		}
		else
			f = new File(result);
		// parent folder will be created in the handler
		//if (!f.getParentFile().exists())
		//	f.getParentFile().mkdirs();
		result = f.getAbsolutePath();
		if (result.contains("ROLLNUM"))
			result = result.replace("ROLLNUM", "%g");
		//else
		//	result = result + ".%g";
		if (result.contains("YYYYMMDD"))
			result = result.replace("YYYYMMDD", "%d");
		return result;
	}

	protected Map getMissingTriggerActions()
	{
		Map result = new HashMap();
		for (Iterator it = _config.getKeys("wrapper.filter.missing.trigger"); it.hasNext();)
		{
			String tKey = (String) it.next();
			List lValue = _config.getList(tKey);
			if (lValue == null || lValue.size() != 3)
			{
				getWrapperLogger().info("check parameters for " + tKey);
				continue;
			}
			String tValue = (String) lValue.get(0);
			// commons configuration does no accept <space>, 1, 1 as valid list
			// -> using * instead of space
			if ("*".equals(tValue))
				tValue = "";
			int countValue = -1;
			try
			{
				countValue = Integer.parseInt((String) lValue.get(1));
			}
			catch (Exception ex)
			{
				getWrapperLogger().log(Level.SEVERE, "check parameters for " + tKey, ex);
			}
			long periodValue = -1;
			try
			{
				periodValue = Long.parseLong((String) lValue.get(2)) * 1000;
			}
			catch (Exception ex)
			{
				getWrapperLogger().log(Level.SEVERE, "check parameters for " + tKey, ex);
			}
			String tName = tKey.substring("wrapper.filter.missing.trigger.".length());
			boolean autoStop = _config.getBoolean("wrapper.filter.missing.autostop."+tName, true);
			String aKey = "wrapper.filter.missing.action." + tName;
			String aValue = _config.getString(aKey, "");
			Object action = getTriggerAction(aValue);
			String sKey = "wrapper.filter.missing.script." + tName;
			String sValue = _config.getString(sKey, "");
			List args = _config.getList(sKey + ".args", null);
			int timeout = _config.getInt(sKey + ".timeout", 0);
			String[] strArgs = null;
			if (args != null && args.size() > 0)
			{
				strArgs = new String[args.size()];
				for (int i = 0; i < strArgs.length; i++)
					strArgs[i] = args.get(i).toString();
			}
			Object script = getTriggerScript(sValue, tKey.substring(tKey.lastIndexOf('.')), strArgs, timeout);
			if (action != null || script != null)
			{
				result.put(tValue, new MissingTriggerAction(executor, periodValue, countValue, new TriggerAction[]
				{ (TriggerAction) script, (TriggerAction) action }, autoStop, getWrapperLogger()));
			}
		}
		return result;
	}

	/**
	 * Gets the regex trigger actions.
	 * 
	 * @return the regex trigger actions
	 */
	protected Map getMissingRegexTriggerActions()
	{
		Map result = new HashMap();
		for (Iterator it = _config.getKeys("wrapper.filter.missing.trigger-regex"); it.hasNext();)
		{
			String tKey = (String) it.next();
			List lValue = _config.getList(tKey);
			if (lValue == null || lValue.size() != 3)
			{
				getWrapperLogger().info("check parameters for " + tKey);
				continue;
			}
			String tValue = (String) lValue.get(0);
			int countValue = -1;
			try
			{
				countValue = Integer.parseInt((String) lValue.get(1));
			}
			catch (Exception ex)
			{
				getWrapperLogger().log(Level.SEVERE, "check parameters for " + tKey, ex);
			}
			long periodValue = -1;
			try
			{
				getWrapperLogger().info("check parameters for " + tKey);
				periodValue = Long.parseLong((String) lValue.get(2)) * 1000;
			}
			catch (Exception ex)
			{
				getWrapperLogger().log(Level.SEVERE, "check parameters for " + tKey, ex);
			}
			String tName = tKey.substring("wrapper.filter.missing.trigger-regex.".length());
			boolean autoStop = _config.getBoolean("wrapper.filter.missing.autostop."+tName, true);
			String aKey = "wrapper.filter.missing.action." + tName;
			String aValue = _config.getString(aKey, "");
			Object action = getTriggerAction(aValue);
			String sKey = "wrapper.filter.missing.script." + tName;
			String sValue = _config.getString(sKey, "");
			List args = _config.getList(sKey + ".args", null);
			int timeout = _config.getInt(sKey + ".timeout", 0);
			String[] strArgs = null;
			if (args != null && args.size() > 0)
			{
				strArgs = new String[args.size()];
				for (int i = 0; i < strArgs.length; i++)
					strArgs[i] = args.get(i).toString();
			}
			Object script = getTriggerScript(sValue, tKey.substring(tKey.lastIndexOf('.')), strArgs, timeout);
			if (action != null || script != null)
			{
				result.put(tValue, new MissingTriggerAction(executor, periodValue, countValue, new TriggerAction[]
				{ (TriggerAction) script, (TriggerAction) action }, autoStop, getWrapperLogger()));
			}
		}
		return result;
	}

	/**
	 * Gets the trigger actions.
	 * 
	 * @return the trigger actions
	 */
	protected Map getTriggerActions()
	{
		Map result = new HashMap();
		List configList = new ArrayList();
		for (Iterator it = _config.getKeys("wrapper.filter.trigger"); it.hasNext();)
		{
			configList.add(it.next());
		}
		Collections.sort(configList, new AlphanumComparator());

		for (Iterator it = configList.listIterator(); it.hasNext();)
		{
			String tKey = (String) it.next();
			String tValue = _config.getString(tKey);
			if (tValue == null || tValue.length() == 0)
				continue;
			String tName = tKey.substring("wrapper.filter.trigger.".length());
			String aKey = "wrapper.filter.action." + tName;
			String aValue = _config.getString(aKey, "");
			Object action = getTriggerAction(aValue);
			String sKey = "wrapper.filter.script." + tName;
			String sValue = _config.getString(sKey, "");
			List args = _config.getList(sKey + ".args", null);
			int timeout = _config.getInt(sKey + ".timeout", 0);
			String[] strArgs = null;
			if (args != null && args.size() > 0)
			{
				strArgs = new String[args.size()];
				for (int i = 0; i < strArgs.length; i++)
					strArgs[i] = args.get(i).toString();
			}
			Object script = getTriggerScript(sValue, tKey.substring(tKey.lastIndexOf('.')), strArgs, timeout);
			if (action != null && script != null)
			{
				addToActionMap(result, tValue, Arrays.asList(new Object[]
				{ script, action }));
			}
			else if (action != null)
				addToActionMap(result, tValue, action);
			else if (script != null)
				addToActionMap(result, tValue, script);
			else
				addToActionMap(result, aKey, "RESTART");
		}
		return result;
	}

	private void addToActionMap(Map actionsMap, String key, Object value)
	{
		Object c = actionsMap.get(key);
		if (c == null)
		{
			actionsMap.put(key, value);
		}
		else if (c instanceof Collection && value instanceof Collection)
		{
			ArrayList l = new ArrayList();
			l.addAll((Collection) c);
			l.addAll((Collection) value);
			actionsMap.put(key, l);
		}
		else if (c instanceof Collection && !(value instanceof Collection))
		{
			ArrayList l = new ArrayList();
			l.addAll((Collection) c);
			l.add(value);
			actionsMap.put(key, l);
		}
		else if (!(c instanceof Collection) && value instanceof Collection)
		{
			ArrayList l = new ArrayList();
			l.add((Collection) c);
			l.addAll((Collection) value);
			actionsMap.put(key, l);
		}
		else
		// c is not a collection && value is not a collection
		{
			actionsMap.put(key, Arrays.asList(new Object[]
			{ c, value }));
		}

	}

	/**
	 * Gets the regex trigger actions.
	 * 
	 * @return the regex trigger actions
	 */
	protected Map getRegexTriggerActions()
	{
		Map result = new HashMap();
		for (Iterator it = _config.getKeys("wrapper.filter.trigger-regex"); it.hasNext();)
		{
			String tKey = (String) it.next();
			String tValue = _config.getString(tKey);
			if (tValue == null || tValue.length() == 0)
				continue;
			String tName = tKey.substring("wrapper.filter.trigger-regex.".length());
			String aKey = "wrapper.filter.action." + tName;
			String aValue = _config.getString(aKey, "");
			Object action = getTriggerAction(aValue);
			String sKey = "wrapper.filter.script." + tName;
			String sValue = _config.getString(sKey, "");
			List args = _config.getList(sKey + ".args", null);
			int timeout = _config.getInt(sKey + ".timeout", 0);
			String[] strArgs = null;
			if (args != null && args.size() > 0)
			{
				strArgs = new String[args.size()];
				for (int i = 0; i < strArgs.length; i++)
					strArgs[i] = args.get(i).toString();
			}

			Object script = getTriggerScript(sValue, tKey.substring(tKey.lastIndexOf('.')), strArgs, timeout);
			if (action != null && script != null)
			{
				addToActionMap(result, tValue, Arrays.asList(new Object[]
				{ script, action }));
			}
			else if (action != null)
				addToActionMap(result, tValue, action);
			else if (script != null)
				addToActionMap(result, tValue, script);
			else
				addToActionMap(result, aKey, "RESTART");
		}
		return result;
	}

	/**
	 * Gets the trigger script.
	 * 
	 * @param script
	 *            the script
	 * @param key
	 *            the key
	 * 
	 * @return the trigger script
	 */
	private Object getTriggerScript(String script, String key, String[] args, int timeout)
	{
		if (script == null || "".equals(script))
			return null;
		final Script s = ScriptFactory.createScript(script, key, this, args, getInternalWrapperLogger(), timeout, _config.getString("wrapper.script.encoding"), _config.getBoolean("wrapper.script.reload", false), _debug);
		if (s == null)
		{
			this.getWrapperLogger().info("error initializing script " + script);
			return null;
		}
		if (_debug)
		this.getWrapperLogger().info("found script " + s.getScript());
		// final String id = key;

		return new TriggerAction()
		{
			public Object execute(final String line)
			{
				if (scriptExecutor.getActiveCount() > 20)
				{
					getInternalWrapperLogger().warn("executing too many scripts concurrently -> aborting script execution");
					return null;
				}
				scriptExecutor.execute(new Runnable()
				{

					public void run()
					{
						AbstractWrappedProcess.this.getWrapperLogger().info("start script " + s.getScript());
						s.executeWithTimeout(new String(line));
						AbstractWrappedProcess.this.getWrapperLogger().info("end script " + s.getScript());
					}
				});
				return null;
			}
		};
	}

	/**
	 * Gets the trigger action.
	 * 
	 * @param value
	 *            the value
	 * 
	 * @return the trigger action
	 */
	private Object getTriggerAction(String value)
	{
		if ("RESTART".equals(value))
			return new TriggerAction()
			{
				public Object execute(String line)
				{
					if (allowRestart())
						restartInternal();
					else if (getState() == STATE_RUNNING)
						stop("TRIGGER");
					return null;
				}
			};
		else if ("SHUTDOWN".equals(value))
			return new TriggerAction()
			{
				public Object execute(String line)
				{
					stop("TRIGGER");
					return null;
				}
			};

		return null;
	}

	/**
	 * Stop timer.
	 */
	public synchronized void stopTimer()
	{
		_timer.stop();
	}

	public synchronized void stopCondition()
	{
		_condition.stop();
	}
	
	public void stop()
	{
		stop("USER");
	}

	public void stop(String reason)
	{
		if (_debug)
			getWrapperLogger().info("stop from Thread " + Thread.currentThread().getName()+ " reason: "+reason);
		_stopReason = reason;
		_startRequested = false;
		if (_state != STATE_RUNNING && _state != STATE_IDLE)
		{
			getWrapperLogger().info("process not in state RUNNING -> Delaying stop");
			_stopRequested = true;
		}
		stopInternal();
	}

	/**
	 * Stop.
	 */
	public synchronized void stopInternal()
	{
		setAppReportedReady(false);
		if (_state == STATE_RESTART)
		{
			_appStopped = new Date();
			setState(STATE_RESTART_STOP);
			_stopReason = "RESTART";
		}
		else if (_state == STATE_RUNNING)
		{
			_appStopped = new Date();
			setState(STATE_USER_STOP);
		}
		else
			return;

		// if (_debug)
		long shutdownWaitTime = _config.getInt("wrapper.shutdown.timeout", Constants.DEFAULT_SHUTDOWN_TIMEOUT) * 1000;
		shutdownWaitTime += _config.getInt("wrapper.jvm_exit.timeout", Constants.DEFAULT_JVM_EXIT_TIMEOUT) * 1000;
		if (shutdownWaitTime > Integer.MAX_VALUE)
			shutdownWaitTime = Integer.MAX_VALUE;
		/*
		 * bkowal
		 * Suppress extraneous output unless debug is enabled.
		 */
		if (_debug)
		{
			getWrapperLogger().info(
				"stopping process with pid/timeout " + _osProcess.getPid() + " " + shutdownWaitTime);
		}

		stopController((int) shutdownWaitTime, _stopReason);
		stopOsProcess((int) shutdownWaitTime);

		_appStopped = new Date();
		if (_state == STATE_USER_STOP)
			if (!_exiting)
				setState(STATE_IDLE);
		removeShutdownHooks();
	}

	private void removeShutdownHooks()
	{
		if (_exiting)
			return;
		for (Thread hook : _shutdownHooks)
		{
			Runtime.getRuntime().removeShutdownHook(hook);
		}
		_shutdownHooks.clear();
	}

	private void stopOsProcess(int shutdownWaitTime)
	{
		boolean externalStop = false;
		String stopConfigName = _config.getString("wrapper.stop.conf");
		File stopConfigFile = null;
		if (stopConfigName != null)
		{
			getWrapperLogger().info("using stop configuration " + stopConfigName);
			stopConfigFile = new File(stopConfigName);
			stopConfigName = stopConfigFile.getAbsolutePath();
			externalStop = stopConfigFile.isFile() && stopConfigFile.exists();
			if (!externalStop)
				getWrapperLogger().severe("error accessing stop configuration "+stopConfigName);
		}
		WrappedProcess stopper = null;
		if (externalStop)
		{
			getWrapperLogger().info("starting stop application");
			Configuration stopLocalConf = new BaseConfiguration();
			stopLocalConf.setProperty("wrapper.config", stopConfigName);
			YajswConfigurationImpl stopConf = new YajswConfigurationImpl(stopLocalConf, _useSystemProperties);
			stopper = WrappedProcessFactory.createProcess(stopConf);
			stopper.getLocalConfiguration().setProperty("wrapper.config", stopConfigName);
			stopper.setUseSystemProperties(_useSystemProperties);
			stopper.setStopper(true);
			// stopper.setDebug(true);
			stopper.init();
			stopper.start();
		}

		// else normally process is stopped by the controller
		// _osProcess.stop(shutdownWaitTime, 999);

		if (shutdownWaitTime > 0)
			_osProcess.waitFor(shutdownWaitTime);
		
		long remainStopWait = getRemainStopWaitTime();
		while (_osProcess.isRunning() && remainStopWait > 0)
		{
			if (_debug)
				getAppLogger().info("extending wait time " + remainStopWait);

			if (_service != null)
				((StopableService)_service).signalStopping(remainStopWait);
			_osProcess.waitFor(remainStopWait);
			remainStopWait = getRemainStopWaitTime();
		}
		if (_osProcess.isRunning())
		{
			getWrapperLogger().info("process did not stop after " + shutdownWaitTime + " sec. -> hard kill");
		}
		_osProcess.kill(999);

		if (stopper != null && stopper.getState() != STATE_IDLE)
			stopper.stop();

		// give the OS some time to clean up
		try
		{
			Thread.sleep(500);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
			Thread.currentThread().interrupt();
		}
		_osProcess.destroy();

		/*
		 * bkowal
		 * Suppress extraneous output unless debug is enabled.
		 */
		if (_debug)
		{
			getWrapperLogger().info("process exit code: " + _osProcess.getExitCode());
		}
	}

	private long getRemainStopWaitTime()
	{
		_stoppingHintLock.lock();

		long d = _stoppingHintSetTime - System.currentTimeMillis();
		long result = _stoppingHint + d;
		
		_stoppingHintLock.unlock();
		return result;
	}

	abstract void stopController(int timeout, String reason);

	/**
	 * Allow restart.
	 * 
	 * @return true, if successful
	 */
	protected boolean allowRestart()
	{
		if (System.currentTimeMillis() - _firstRestartTime > _successfulInvocationTime)
			_restartCount = 0;
		if (_state == STATE_USER_STOP || _state == STATE_RESTART || _state == STATE_RESTART_STOP || _state == STATE_RESTART_WAIT)
			return false;
		if (_restartCount < _config.getInt("wrapper.max_failed_invocations", DEFAULT_MAX_FAILED_INVOCATIONS))
			return true;
		getWrapperLogger().info("too many restarts ");
		return false;
	}

	/**
	 * Restart.
	 */
	public void restart()
	{
		if (_state != STATE_RUNNING)
			return;
		restartInternal();
	}

	/**
	 * Restart by timer.
	 */
	boolean	_timerRestart	= false;

	public void restartByTimer()
	{
		if (_timerRestart)
			return;
		_timerRestart = true;
		stop("TIMER");
		try
		{
			Thread.sleep(getRestartDelay());
		}
		catch (InterruptedException e)
		{
			if (_debug)
				getWrapperLogger().log(Level.SEVERE, this.getClass().getName() + " restart", e);
			Thread.currentThread().interrupt();
		}

		startByTimer();
		_timerRestart = false;
	}

	/**
	 * Checks if is debug.
	 * 
	 * @return true, if is debug
	 */
	boolean isDebug()
	{
		return _debug;
	}

	/**
	 * Sets the debug.
	 * 
	 * @param debug
	 *            the new debug
	 */
	public void setDebug(boolean debug)
	{
		_debug = debug;
	}

	/**
	 * Gets the pid.
	 * 
	 * @return the pid
	 */
	public int getAppPid()
	{
		if (_osProcess != null)
			return _osProcess.getPid();
		else
			return -1;
	}

	/**
	 * Save pid file.
	 */
	private void savePidFile()
	{
		String file = _config.getString("wrapper.pidfile");
		if (file != null)
		{
			try
			{
				_pidFile = new File(file);
				if (!_pidFile.exists())
					_pidFile.createNewFile();
				FileWriter out = new FileWriter(_pidFile, false);
				out.write("" + OperatingSystem.instance().processManagerInstance().currentProcessId());
				out.flush();
				out.close();
				Thread hook = new Thread()
				{
					public void run()
					{
						removePidFile();
					}
				};
				Runtime.getRuntime().addShutdownHook(hook);
				_shutdownHooks.add(hook);
				if (_debug)
					getWrapperLogger().info("created pid file " + _pidFile.getAbsolutePath());
			}
			catch (Exception e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Removes the pid file.
	 */
	private void removePidFile()
	{
		if (_pidFile != null)
		{
			try
			{
				_pidFile.delete();
				if (_config.getBoolean("wrapper.service", false))
					this.stop();

				if (_debug)
					getWrapperLogger().info("removed pid file " + _pidFile.getAbsolutePath());
				_pidFile = null;
			}
			catch (Exception e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Save lock file.
	 * 
	 * @return true, if successful
	 */
	protected boolean saveLockFile()
	{
		if (_lock != null)
			return true;
		String file = _config.getString("wrapper.lockfile", null);
		if (file != null && !"".equals(file))
		{
			try
			{
				_lockFile = new File(file);
				// Check if the lock exist
				if (_lockFile.exists())
				{
					// if exist try to delete it
					_lockFile.delete();
				}
				// Try to get the lock
				_lockFileChannel = new RandomAccessFile(_lockFile, "rw").getChannel();
				_lock = _lockFileChannel.tryLock();
				if (_lock == null)
				{
					// File is lock by other application
					_lockFileChannel.close();
					getWrapperLogger().warning("Lock file " + file + " already locked by another application -> abort");
					return false;
				}
				if (_debug)
					getWrapperLogger().info("created lock file " + _lockFile.getAbsolutePath());

			}
			catch (IOException e)
			{
				e.printStackTrace();
				return false;
			}
		}
		return true;
	}

	/**
	 * Removes the lock file.
	 */
	private void removeLockFile()
	{
		if (_lock != null)
		{
			// release and delete file lock
			try
			{
				_lock.release();
				if (_debug)
					getWrapperLogger().info("removed lock file " + _lockFile.getAbsolutePath());
				_lock = null;
				_lockFileChannel.close();
				_lockFileChannel = null;
				_lockFile.delete();
				_lockFile = null;
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
	}

	/**
	 * File writer.
	 * 
	 * @param file
	 *            the file
	 * 
	 * @return the file writer
	 */
	private FileWriter FileWriter(String file)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Wait for.
	 */
	public void waitFor()
	{
		waitFor(Long.MAX_VALUE);
	}

	/**
	 * Wait for.
	 * 
	 * @param t
	 *            the t
	 */
	public void waitFor(long t)
	{
		if (_state == STATE_IDLE)
			return;
		final Lock lock = new MyReentrantLock();
		final java.util.concurrent.locks.Condition isIdle = lock.newCondition();
		StateChangeListener listener = new StateChangeListener()
		{

			public void stateChange(int newState, int oldState)
			{
				lock.lock();
				isIdle.signal();
				lock.unlock();
			}

		};
		this.addStateChangeListenerInternal(STATE_IDLE, listener);
		if (_state != STATE_IDLE)
			try
			{
				lock.lock();
				isIdle.await(t, TimeUnit.MILLISECONDS);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		lock.unlock();
		this.removeStateChangeListener(listener);
	}

	/**
	 * Gets the local configuration.
	 * 
	 * @return the local configuration
	 */
	public Configuration getLocalConfiguration()
	{
		return _localConfiguration;
	}

	public void setLocalConfiguration(Configuration config)
	{
		_localConfiguration = config;
	}

	/**
	 * Gets the exit code.
	 * 
	 * @return the exit code
	 */
	public int getExitCode()
	{
		return _exitCode;
	}

	/**
	 * Gets the wrapper logger.
	 * 
	 * @return the wrapper logger
	 */
	public Logger getWrapperLogger()
	{
		if (_wrapperLogger != null)
			return _wrapperLogger;
		_wrapperLogger = new MyLogger();
		((MyLogger) _wrapperLogger).setPID(_wrapperLoggerName);
		((MyLogger) _wrapperLogger).setName(getName());
		_wrapperLogger.setUseParentHandlers(false);
		if (_controller != null)
			_controller.setLogger(_appLogger);
		if (getFileHandler() != null)
			_wrapperLogger.addHandler(getFileHandler());
		if (getConsoleHandler() != null)
			_wrapperLogger.addHandler(getConsoleHandler());
		return _wrapperLogger;
	}
	
	public InternalLogger getInternalWrapperLogger()
	{
		if (_internalLoggerFactory == null)
			_internalLoggerFactory = new JdkLogger2Factory(getWrapperLogger());
		return _internalLoggerFactory.newInstance("");
	}

	/**
	 * Sets the use system properties.
	 * 
	 * @param useSystemProperties
	 *            the new use system properties
	 */
	public void setUseSystemProperties(boolean useSystemProperties)
	{
		_useSystemProperties = useSystemProperties;
	}

	/**
	 * Gets the tmp path.
	 * 
	 * @return the tmp path
	 */
	public String getTmpPath()
	{
		if (_tmpPath == null)
		{
			_tmpPath = _config.getString("wrapper.tmp.path");
			if (_tmpPath == null || _tmpPath.startsWith("?"))
				_tmpPath = System.getProperty("jna_tmpdir");
			if (_tmpPath == null || _tmpPath.startsWith("?"))
				_tmpPath = System.getProperty("java.io.tmpdir");
			if (_tmpPath == null || _tmpPath.startsWith("?"))
				_tmpPath = "tmp";
			File t = new File(_tmpPath);
			if (!t.exists())
				t.mkdirs();
		}
		return _tmpPath;
	}

	/**
	 * Start drain.
	 */
	public synchronized void startDrain()
	{
		if (_gobler_err != null)
			_gobler_err.setDrain(true);
		if (_gobler_in != null)
			_gobler_in.setDrain(true);
		_drainActive = true;
	}

	/**
	 * Stop drain.
	 */
	public synchronized void stopDrain()
	{
		if (_gobler_err != null)
			_gobler_err.setDrain(false);
		if (_gobler_in != null)
			_gobler_in.setDrain(false);
		_drainActive = false;
	}

	/**
	 * Read drain line.
	 * 
	 * @return the string
	 */
	public String readDrainLine()
	{
		String result = null;
		if (!_drainActive)
			return null;
		if (_gobler_err != null)
			try
			{
				if (!_gobler_err.isDrain())
					startDrain();
				result = _gobler_err.getDrainReader().readLine();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		if (result == null && _gobler_in != null)
			try
			{
				if (!_gobler_in.isDrain())
					startDrain();
				result = _gobler_in.getDrainReader().readLine();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		return result;

	}

	public int getState()
	{
		return _state;
	}

	/**
	 * Restart internal.
	 */
	public void restartInternal()
	{
		setAppReportedReady(false);
		getWrapperLogger().info("restart internal " + getStringState());
		if (_state == STATE_RUNNING || _state == STATE_STARTING || _state == STATE_RESTART_START)
			setState(STATE_RESTART);
		else
			return;
		_restartCount++;
		if (_debug)
		{
			getWrapperLogger().info("restarting " + _restartCount + " time");
		}
		stopInternal();
		if (!_stopRequested)
		{
			setState(STATE_RESTART_WAIT);
			try
			{
				Thread.sleep(getRestartDelay());
			}
			catch (InterruptedException e)
			{
				if (_debug)
					getWrapperLogger().log(Level.SEVERE, this.getClass().getName() + " restart", e);
				Thread.currentThread().interrupt();
			}
			if (!_stopRequested)
				startInternal();
			else
			{
				getWrapperLogger().log(Level.SEVERE, "Process " + getName() + " stop requested, setting IDLE in restart internal");
				_stopRequested = false;
				if (!_osProcess.isRunning())
					setState(STATE_IDLE);
			}
		}
		else
		{
			if (_debug)
				getWrapperLogger().info(" stop requested, setting IDLE in restart internal");
			_stopRequested = false;
			if (!_osProcess.isRunning())
				setState(STATE_IDLE);
		}
	}

	private long getRestartDelay()
	{
		Script script = getRestartDelayScript();
		if (script != null)
		{
			Object time = script.execute();
			if (time instanceof Number)
				return ((Number) time).longValue() * 1000;
		}
		return _config.getLong("wrapper.restart.delay", DEFAULT_RESTART_DELAY) * 1000;
	}

	private Script getRestartDelayScript()
	{
		if (_restartDelayScript != null)
			return _restartDelayScript;
		String script = _config.getString("wrapper.restart.delay.script", null);
		if (script != null)
		{
			List args = _config.getList("wrapper.restart.delay.script.args", null);
			int timeout = _config.getInt("wrapper.restart.delay.script.timeout", 0);
			_restartDelayScript = ScriptFactory.createScript(script, "", this, args, getInternalWrapperLogger(), 0, _config.getString("wrapper.script.encoding"), _config.getBoolean("wrapper.script.reload", false), _debug);
		}
		return _restartDelayScript;
	}

	/**
	 * The Class Gobler.
	 */
	protected class Gobler implements Runnable
	{

		/** The _input stream. */
		InputStream									_inputStream;

		/** The _name. */
		String										_name;

		/** The _gobler log. */
		Logger										_goblerLog;

		/** The _actions regex. */
		Map											_actionsRegex;
		Map											_missingActionsRegex;

		/** The _action triggers regex. */
		com.karneim.util.collection.regex.Pattern[]	_actionTriggersRegex;
		com.karneim.util.collection.regex.Pattern[]	_missingActionTriggersRegex;

		/** The _actions. */
		Map											_actions;
		Map											_missingActions;

		/** The _action triggers. */
		String[]									_actionTriggers;
		String[]									_missingActionTriggers;

		/** The _drain. */
		volatile boolean							_drain;

		/** The _drain buffer. */
		volatile CircularBuffer						_drainBuffer;

		/** The _drain reader. */
		volatile BufferedReader						_drainReader;
		volatile int								_pid;

		/**
		 * Sets the drain.
		 * 
		 * @param drain
		 *            the new drain
		 */
		public void setDrain(boolean drain)
		{
			if (drain && !_drain)
			{
				_drainBuffer = new CircularBuffer(16384, false);
				_drainReader = new BufferedReader(_drainBuffer);
			}
			else if (!drain && _drain)
			{
				try
				{
					_drainReader.close();
					_drainBuffer = null;
				}
				catch (IOException e)
				{
					e.printStackTrace();
				}
			}

			_drain = drain;
		}

		/**
		 * Gets the drain reader.
		 * 
		 * @return the drain reader
		 */
		public BufferedReader getDrainReader()
		{
			return _drainReader;
		}

		/**
		 * Instantiates a new gobler.
		 * 
		 * @param stream
		 *            the stream
		 * @param log
		 *            the log
		 * @param events
		 *            the events
		 * @param eventsRegex
		 *            the events regex
		 * @param name
		 *            the name
		 */
		public Gobler(InputStream stream, Logger log, Map events, Map eventsRegex, Map missingEvents, Map missingEventsRegex, String name, int pid)
		{
			_inputStream = stream;
			_name = name;
			_goblerLog = log;
			_actions = events;
			if (events != null)
			{
				_actionTriggers = new String[events.size()];
				int i = 0;
				for (Iterator it = events.keySet().iterator(); it.hasNext(); i++)
				{
					_actionTriggers[i] = (String) it.next();
				}
			}
			_actionsRegex = eventsRegex;
			if (eventsRegex != null)
			{
				_actionTriggersRegex = new com.karneim.util.collection.regex.Pattern[eventsRegex.size()];
				int i = 0;
				for (Iterator it = eventsRegex.keySet().iterator(); it.hasNext(); i++)
				{
					String s = (String) it.next();
					try
					{
						_actionTriggersRegex[i] = new com.karneim.util.collection.regex.Pattern(s);
					}
					catch (Throwable ex)
					{
						getWrapperLogger().log(Level.SEVERE, "error in regular expression " + s, ex);
					}
				}
			}
			_missingActions = missingEvents;
			if (_missingActions != null)
			{
				_missingActionTriggers = new String[_missingActions.size()];
				int i = 0;
				for (Iterator it = _missingActions.keySet().iterator(); it.hasNext(); i++)
				{
					_missingActionTriggers[i] = (String) it.next();
				}
			}
			_missingActionsRegex = missingEventsRegex;
			if (_missingActionsRegex != null)
			{
				_missingActionTriggersRegex = new com.karneim.util.collection.regex.Pattern[_missingActionsRegex.size()];
				int i = 0;
				for (Iterator it = missingEventsRegex.keySet().iterator(); it.hasNext(); i++)
				{
					String s = (String) it.next();
					try
					{
						_missingActionTriggersRegex[i] = new com.karneim.util.collection.regex.Pattern(s);
					}
					catch (Throwable ex)
					{
						getWrapperLogger().log(Level.SEVERE, "error in regular expression " + s, ex);
					}
				}
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Runnable#run()
		 */
		public void run()
		{
			try
			{
				if (_inputStream == null)
				{
					_goblerLog.info("cannot run stream gobler with a null stream");
					return;
				}
				String encoding = _config.getString("wrapper.log.encoding");
				InputStreamReader isr;
				if (encoding == null)
					isr = new InputStreamReader(_inputStream);
				else
					isr = new InputStreamReader(_inputStream, encoding);
					
				BufferedReader br = new BufferedReader(isr);
				String line = null;
				int k = 0;
				if (_missingActions != null)
					for (Iterator it = _missingActions.values().iterator(); it.hasNext();)
					{
						((MissingTriggerAction) it.next()).start();
					}
				if (_missingActionsRegex != null)
					for (Iterator it = _missingActionsRegex.values().iterator(); it.hasNext();)
					{
						((MissingTriggerAction) it.next()).start();
					}
				while ((line = br.readLine()) != null)
				{
					if (_drain)
                                        {
						_drainBuffer.write(line);
                                        }

										/*
										 * bkowal - added logic to check for the infinite indicator
										 */
                                        if (MIN_PROCESS_LINES_TO_LOG == INFINITE_PROCESS_LOGGING ||
                                            k <= MIN_PROCESS_LINES_TO_LOG)
                                        {
                                        	_goblerLog.info(line);
						k++;
                                        }
                                        else
                                        {
						_goblerLog.finest(line);
                                        }


					if (_actionTriggers != null)
						for (int i = 0; i < _actionTriggers.length; i++)
						{
							if (line.contains(_actionTriggers[i]))
							{
								Object obj = _actions.get(_actionTriggers[i]);
								if (obj instanceof TriggerAction)
								{
									TriggerAction action = (TriggerAction) obj;
									getWrapperLogger().info("Trigger found: " + _actionTriggers[i] + " in line: ");
									getWrapperLogger().info(line);
									action.execute(new String(line));
								}
								else if (obj instanceof Collection)
								{
									Collection c = (Collection) obj;
									for (Iterator it = c.iterator(); it.hasNext();)
									{
										TriggerAction action = (TriggerAction) it.next();
										getWrapperLogger().info("Trigger found: " + action + " in line: ");
										getWrapperLogger().info(line);
										action.execute(new String(line));
									}
								}
								break;
							}
						}

					if (_actionTriggersRegex != null)
						for (int i = 0; i < _actionTriggersRegex.length; i++)
						{
							if (_actionTriggersRegex[i].contains(line))
							{
								Object obj = (TriggerAction) _actionsRegex.get(_actionTriggersRegex[i].getRegEx());
								if (obj instanceof TriggerAction)
								{
									TriggerAction action = (TriggerAction) obj;
									getWrapperLogger().info("Trigger found: " + _actionTriggers[i] + " in line: ");
									getWrapperLogger().info(line);
									action.execute(new String(line));
								}
								else if (obj instanceof Collection)
								{
									Collection c = (Collection) obj;
									for (Iterator it = c.iterator(); it.hasNext();)
									{
										TriggerAction action = (TriggerAction) it.next();
										getWrapperLogger().info("Trigger found: " + _actionTriggers[i] + " in line: ");
										getWrapperLogger().info(line);
										action.execute(new String(line));
									}
								}
								break;
							}
						}

					if (_missingActionTriggers != null)
						for (int i = 0; i < _missingActionTriggers.length; i++)
						{
							if ("".equals(_missingActionTriggers[i]) || line.contains(_missingActionTriggers[i]))
							{
								Object obj = (TriggerAction) _missingActions.get(_missingActionTriggers[i]);
								if (obj instanceof TriggerAction)
								{
									TriggerAction action = (TriggerAction) obj;
									if (_debug)
									  getWrapperLogger().info("found missing trigger : " + _missingActionTriggers[i]);
									action.execute(new String(line));
								}
								// break;
							}
						}

					if (_missingActionTriggersRegex != null)
						for (int i = 0; i < _missingActionTriggersRegex.length; i++)
						{
							if (_missingActionTriggersRegex[i].contains(line))
							{
								Object obj = (TriggerAction) _actionsRegex.get(_missingActionTriggersRegex[i].getRegEx());
								if (obj instanceof TriggerAction)
								{
									TriggerAction action = (TriggerAction) obj;
									if (_debug)
									  getWrapperLogger().info("found missing trigger : " + _missingActionTriggers[i]);
									action.execute(new String(line));
								}
								// break;
							}
						}

					Thread.yield();
				}
			}
			catch (Exception ioe)
			{
				// ioe.printStackTrace();
				// _goblerLog.info("gobler execption " + _name + " " +
				// ioe.getMessage());
				if (_debug)
					_goblerLog.log(Level.INFO, " gobler terminated " + _name + " "+ioe);
			}
			if (AbstractWrappedProcess.this._osProcess != null && _pid == AbstractWrappedProcess.this._osProcess.getPid()
					&& AbstractWrappedProcess.this._osProcess.isRunning())
				AbstractWrappedProcess.this.executor.execute(new Runnable()
				{
					public void run()
					{
						if (AbstractWrappedProcess.this._state != STATE_RESTART_START && AbstractWrappedProcess.this._state != STATE_RESTART
								&& AbstractWrappedProcess.this._state != STATE_RESTART_STOP
								&& AbstractWrappedProcess.this._state != STATE_RESTART_WAIT)
						{
							_goblerLog.warning("yajsw panicking: gobler terminated but process is still running -> restart process");
							AbstractWrappedProcess.this.restartInternal();
						}
					}
				});

			if (_debug)
				_goblerLog.info("gobler terminated " + _name);
			if (_missingActions != null)
				for (Iterator it = _missingActions.values().iterator(); it.hasNext();)
				{
					((MissingTriggerAction) it.next()).stop();
				}
			if (_missingActionsRegex != null)
				for (Iterator it = _missingActionsRegex.values().iterator(); it.hasNext();)
				{
					((MissingTriggerAction) it.next()).stop();
				}

		}

		public boolean isDrain()
		{
			return _drain;
		}
	}

	private void addStateChangeListenerInternal(int state, StateChangeListener listener)
	{
		_listeners.put(state, listener);
	}

	private void addStateChangeListenerInternal(StateChangeListener listener)
	{
		_listeners.put(999, listener);
	}

	public void addStateChangeListener(int state, StateChangeListener listener)
	{
		_userListeners.put(state, listener);
	}

	public void addStateChangeListener(StateChangeListener listener)
	{
		_userListeners.put(999, listener);
	}

	public void removeStateChangeListener(StateChangeListener listener)
	{

	}

	public void removeStateChangeListener(int state)
	{
		_listeners.remove(state);
	}

	public int getRestartCount()
	{
		return _restartCount;
	}

	public String getStringState()
	{
		return getStringState(_state);
	}

	public String getName()
	{
		String result = "";
		if (_config == null)
			return result;
		if (_config.getBoolean("wrapper.service", false))
			result += "Service ";
		String name = _config.getString("wrapper.console.title");
		if (name == null)
			name = _config.getString("wrapper.ntservice.name");
		if (name == null)
			name = _config.getString("wrapper.image");
		if (name == null)
			name = _config.getString("wrapper.groovy");
		if (name == null)
			name = _config.getString("wrapper.java.app.mainclass");
		if (name == null)
			name = _config.getString("wrapper.java.app.jar");
		if (name == null)
			name = "";
		result += name;
		return result;

	}

	public OutputStream getOutputStream()
	{
		if (_osProcess != null)
			return _osProcess.getOutputStream();
		return null;
	}

	public Date getAppStarted()
	{
		return _appStarted;
	}

	public Date getAppStopped()
	{
		return _appStopped;
	}

	public Date getWrapperStarted()
	{
		return _wrapperStarted;
	}

	public int getAppThreads()
	{
		if (_osProcess != null)
			return _osProcess.getCurrentThreads();
		else
			return -1;
	}

	public long getAppVMemory()
	{
		if (_osProcess != null)
			return _osProcess.getCurrentVirtualMemory();
		else
			return -1;
	}

	public long getAppPMemory()
	{
		if (_osProcess != null)
			return _osProcess.getCurrentPhysicalMemory();
		else
			return -1;
	}

	public int getAppCpu()
	{
		if (_osProcess != null)
			return _osProcess.getCurrentCpu();
		else
			return -1;
	}

	public int getAppHandles()
	{
		if (_osProcess != null)
			return _osProcess.getCurrentHandles();
		else
			return -1;
	}

	public void addTriggerListener(TriggerListener listener)
	{
		// TODO ;
	}

	public int getWrapperPid()
	{
		return OperatingSystem.instance().processManagerInstance().currentProcessId();
	}

	public boolean isTimerActive()
	{
		return (_timer != null) && _timer.isTriggered();
	}

	public boolean isConditionActive()
	{
		return (_condition != null) && _condition.isTriggered();
	}

	public void threadDump()
	{
		if (_osProcess != null && this instanceof WrappedJavaProcess && _osProcess.isRunning())
			((WrappedJavaProcess) this).requestThreadDump();

	}

	public void gc()
	{
		if (_osProcess != null && this instanceof WrappedJavaProcess && _osProcess.isRunning())
			((WrappedJavaProcess) this).requestGc();

	}

	public void dumpHeap(String fileName)
	{
		if (_osProcess != null && this instanceof WrappedJavaProcess && _osProcess.isRunning())
			((WrappedJavaProcess) this).requestDumpHeap(fileName);

	}

	public void wrapperThreadDump()
	{
		Message m = new Message(Constants.WRAPPER_MSG_THREAD_DUMP, null);
		Action a = ActionFactory.getAction(m);
		try
		{
			ByteArrayOutputStream str = new ByteArrayOutputStream();
			PrintStream pr = new PrintStream(str);
			a.execute(m, null, pr, null);
			pr.flush();
			getWrapperLogger().info(str.toString());
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
	}

	public void stopTimerCondition()
	{
		stopTimer();
		stopCondition();
	}

	public boolean isOSProcessRunning()
	{
		if (_osProcess == null)
			return false;
		else
			return _osProcess.isRunning();
	}

	public int getTotalRestartCount()
	{
		return _totalRestartCount;
	}

	public boolean isService()
	{
		return _config.getBoolean("wrapper.service", false);
	}

	public String getType()
	{
		if (_config.getBoolean("wrapper.service", false))
			return "Service";
		else if (_config.getBoolean("wrapper.console.visible", false))
			return "Console";
		else
			return "Process";
	}

	public void shutdown()
	{
		setState(STATE_SHUTDOWN);
	}

	public void osProcessTerminated()
	{
		Process process = _osProcess;
		if (process != null)
			_exitCode = process.getExitCode();
	}

	public boolean isHaltWrapperOnApp()
	{
		return _haltWrapperOnApp;
	}

	public boolean isHaltAppOnWrapper()
	{
		return _haltAppOnWrapper;
	}

	public void setExiting()
	{
		_exiting = true;
	}

	public boolean isExiting()
	{
		return _exiting;
	}

	public TrayIconProxy getTrayIcon()
	{
		return _trayIconMessages;
	}

	public String[][] getTrayIconMessages()
	{
		if (_trayIconMessages == null)
			return null;
		String[][] result =  _trayIconMessages.toArrayAndClear();
		if (_debug && result != null)
			getWrapperLogger().info("sending tray icon messages: #"+result.length);
		return result;
	}

	public void stopWrapper()
	{
		if (_haltAppOnWrapper)
			stop("WRAPPER SHUTDOWN");
		shutdown();
		try
		{
			Thread.sleep(5000);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
		}
		System.exit(0);
	}

	public boolean hasOutput()
	{
		return getOutputStream() != null;
	}

	public void writeOutput(String txt)
	{
		((PrintStream) getOutputStream()).println(txt);
	}

	public void writeInquireResponse(String s)
	{
		if (_trayIconMessages != null)
		{
			_trayIconMessages._inquireResponse = s;
			_trayIconMessages._inquireMessage = null;
		}
	}

	public String getInquireMessage()
	{
		if (_trayIconMessages != null)
			return _trayIconMessages._inquireMessage;
		else
			return null;
	}

	public void setService(Object service)
	{
		_service = service;
	}

	public Object getService()
	{
		return _service;
	}

	public void setProperty(String key, String value)
	{
		getWrapperLogger().info("set property " + key + " " + value);
		_localConfiguration.setProperty(key, value);
	}

	public void resetCache()
	{
		getWrapperLogger().info("reset cache ");
		_cache = null;
	}

	public long getMaxStartTime()
	{
		if (_config == null)
			return 0;
		long startDelay = _config.getLong("wrapper.startup.delay", 0);
		long startupTimeout = _config.getInt("wrapper.startup.timeout", DEFAULT_STARTUP_TIMEOUT) * 1000;
		if (startDelay < Integer.MAX_VALUE && startupTimeout < Integer.MAX_VALUE)
			return startDelay + startupTimeout;
		else
			return Integer.MAX_VALUE;
	}

	public boolean isReconnecting()
	{
		return _reconnecting;
	}

	void setReconnecting(boolean reconnecting)
	{
		_reconnecting = reconnecting;
	}
	
	public void setStopper(boolean b)
	{
		_stopper = b;
	}
	
	public void monitorConf()
	{
		boolean monitor = _config.getBoolean("wrapper.monitor.config", false);
		if (!monitor)
			return;
		if (_config.isStopper())
			return;
		final long fileTime = _config.getConfigFileTime();
		if (fileTime <= 0)
		{
			getWrapperLogger().info("wrapper.monitor.config: cannot start: could not get file time");
			return;
		}

		_localConfiguration.setProperty("wrapper.restart.reload_configuration", true);

		executor.execute(new Runnable()
			{
				public void run()
				{
					getWrapperLogger().info("wrapper.monitor.config: start");
					while (getState() == STATE_RUNNING)
					{
						long t = _config.getConfigFileTime();
						if (t > fileTime)
						{
							getWrapperLogger().info("wrapper.monitor.config: config file changed: "+new Date(fileTime)+" -> "+new Date(t)+" Restarting Application");
							executor.execute(new Runnable()
							{
								public void run()
								{
									restart();
								}
							});
							break;
						}
						try
						{
						Thread.sleep(5000);
						}
						catch (Exception ex)
						{
							break;
						}
					}
					getWrapperLogger().info("wrapper.monitor.config: end");
				}
			});

	}

	public Object getCluster()
	{
		return _cluster;
	}
	
	public Configuration getYajswConfig() {
		        return _config;
		    }
	
	public void signalStopping(long waitHint)
	{
		if (_debug)
			getAppLogger().info("received signalStopping hint " + waitHint);

		if (_state == STATE_RUNNING)
			this.stop("APPLICATION");
		_stoppingHintLock.lock();
		_stoppingHint = waitHint;
		_stoppingHintSetTime = System.currentTimeMillis();
		_stoppingHintLock.unlock();
	}
	
	public boolean isAppReportedReady()
	{
		//System.out.println("isAppReportedReady "+_appReportedReady);
		return _appReportedReady;
	}

	public void setAppReportedReady(boolean appReportedReady)
	{
		_appReportedReady = appReportedReady;
	}
	
	public Color getUserTrayColor()
	{
		if (_trayIconMessages == null)
			return null;
		return _trayIconMessages.getUserColor();
	}
	
	protected void createOSProcess()
	{
		if (_config.getBoolean("wrapper.fork_hack", false))
			_osProcess = new BSDProcess();
		else
			_osProcess = OperatingSystem.instance().processManagerInstance().createProcess();

	}
	
	public void update()
	{
		update(null);
	}
	
	public void update(String updateConfFile)
	{
		update(updateConfFile, true);
	}
	
	public void update(String updateConfFile, boolean autostart)
	{
		getInternalWrapperLogger().info("service update invoked: "+ updateConfFile == null ? "null" : updateConfFile);
		if (!isService())
		{
			getInternalWrapperLogger().warn("service update can only be invoked for a service -> abort ");
			return;
		}
		// if we have defined a default file ignore input
		String internUpdateConfFile = _config.getString("wrapper.update.conf", updateConfFile);
		if (internUpdateConfFile == null || internUpdateConfFile.length() == 0)
		{
			getInternalWrapperLogger().warn("missing update configuration file -> abort");
			return;
		}
		// is this a pattern: choose newest version
		try
		{
			List<FileObject> files = VFSUtils.resolveFiles(internUpdateConfFile);
			FileObject found = null;
			for (FileObject f : files)
			{
				if (found == null)
					found = f;
				else if (f.getContent().getLastModifiedTime() > found.getContent().getLastModifiedTime())
					found = f;
			}
			if (found == null)
			{
				getInternalWrapperLogger().warn("did not find matching file for "+internUpdateConfFile +" -> abort");
				return;
			}
			else
				internUpdateConfFile = found.getName().getPath();
			
		}
		catch (Exception e)
		{
			getInternalWrapperLogger().warn("error in update() ", e);
			return;
		}
		
		UpdateAction.setUpdateConfig(internUpdateConfFile);
		UpdateAction.setCurrentConfig(_config);
		if (autostart)
			UpdateAction.setAutostart();
		getInternalWrapperLogger().info("spawing update process for configuration "+internUpdateConfFile);
		UpdateAction.run();
	}
	


}
