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
package org.rzo.yajsw.config;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationBinding;
import org.apache.commons.configuration.ConfigurationConverter;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.FileOptionsProvider;
import org.apache.commons.configuration.FileSystem;
import org.apache.commons.configuration.GInterpolator;
import org.apache.commons.configuration.Interpolator;
import org.apache.commons.configuration.MapConfiguration;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.VFSFileSystem;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.VFS;
import org.apache.commons.vfs2.impl.DefaultFileSystemManager;
import org.apache.commons.vfs2.provider.local.LocalFile;
import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;
import org.rzo.yajsw.config.jnlp.JnlpSupport;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.script.GroovyScript;
import org.rzo.yajsw.util.CaseInsensitiveMap;
import org.rzo.yajsw.util.CommonsLoggingAdapter;
import org.rzo.yajsw.util.VFSUtils;

import com.sun.jna.Platform;

// TODO: Auto-generated Javadoc
/**
 * The Class AjswConfigurationImpl.
 */
public class YajswConfigurationImpl extends CompositeConfiguration implements YajswConfiguration
{

	/** The log. */
	InternalLogger					log						= InternalLoggerFactory.getInstance(this.getClass().getName());

	/** The _system properties. */
	Configuration			_systemProperties;

	/** The _local configuration. */
	Configuration			_localConfiguration;

	/** The _system configuration. */
	CompositeConfiguration	_systemConfiguration	= new CompositeConfiguration();

	/** The debug. */
	boolean					debug					= false;

	/** The _use system properties. */
	boolean					_useSystemProperties	= true;

	boolean					_isStopper				= false;

	boolean					_init					= false;

	PropertiesConfiguration	_fileConfiguration		= null;

	Interpolator			_interpolator;

	Set						_interpolated			= new HashSet();

	Map						_scriptUtils			= null;

	boolean					_isJavaDebug			= false;

	/**
	 * Instantiates a new ajsw configuration impl.
	 */
	public YajswConfigurationImpl()
	{
		init();
	}

	/**
	 * Instantiates a new ajsw configuration impl.
	 * 
	 * @param debug
	 *            the debug
	 */
	public YajswConfigurationImpl(boolean debug)
	{
		setDebug(debug);
		init();
	}

	public YajswConfigurationImpl(Configuration localConfiguration, boolean useSystemProperties)
	{
		this(localConfiguration, useSystemProperties, null);
	}

	/**
	 * Instantiates a new ajsw configuration impl.
	 * 
	 * @param localConfiguration
	 *            the local configuration
	 * @param useSystemProperties
	 *            the use system properties
	 */
	public YajswConfigurationImpl(Configuration localConfiguration, boolean useSystemProperties, Map scriptUtils)
	{
		_localConfiguration = localConfiguration;
		_useSystemProperties = useSystemProperties;
		_scriptUtils = scriptUtils;
		init();
	}
	
	public Interpolator getYajswInterpolator()
	{
		return _interpolator;
	}


	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.AjswConfiguration#init()
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
  public void init()
	{
		if (_init)
			return;
		/*
		 * ConfigurationInterpolator in = (ConfigurationInterpolator)
		 * getSubstitutor().getVariableResolver(); StrLookup orgLookup =
		 * in.getDefaultLookup(); in.setDefaultLookup(new
		 * MyStrLookup(orgLookup));
		 */
		if (_scriptUtils == null)
			_scriptUtils = new HashMap();
		_interpolator = new GInterpolator(this, true, null, _scriptUtils);
		try
		{
			this.setInterpolator(_interpolator);
		}
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		
		if (_localConfiguration != null)
		{
			_systemConfiguration.addConfiguration(_localConfiguration);
			if (debug)
				log.debug("added local configuration ");
		}
		// order of adding configurations to composite is important
		// first added hides the others

		// load configuration from System Properties
		if (_useSystemProperties)
		{
			_systemProperties = ConfigurationConverter.getConfiguration((Properties) System.getProperties().clone());
			_systemConfiguration.addConfiguration(_systemProperties);
			if (debug)
				log.debug("added system configuration ");
		}
		//_systemConfiguration.addConfiguration(new EnvironmentConfiguration());
    _systemConfiguration.addConfiguration(new MapConfiguration(!OperatingSystem.instance().isPosix() ? new CaseInsensitiveMap(System.getenv()) : new HashMap(System.getenv())));


		addConfiguration(_systemConfiguration);
		// check if we have config file
		String configFile = (String) getProperty("wrapper.config");
		if (configFile != null && configFile.contains("\""))
			configFile = configFile.replaceAll("\"", "");

		// load configuration from file
		if (configFile == null)
		{
			if (debug)
				log.warn("configuration file not set");
		}
		else if (!fileExists(configFile))
			log.error("configuration file not found: " + configFile);
		else
		{
			// check if we have a jnlp file
			if (configFile.endsWith(".jnlp"))
				try
				{
					JnlpSupport jnlp = new JnlpSupport(configFile);
					_fileConfiguration = jnlp.toConfiguration((String) getProperty("wrapperx.default.config"));
					_fileConfiguration.setFileName(configFile);
					addConfiguration(_fileConfiguration);
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
					return;
				}
			// else try a standard configuration
			if (_fileConfiguration == null)
				try
				{
					// enable VFS
					FileSystem fs = new VFSFileSystem();
					fs.setLogger(new CommonsLoggingAdapter(log));
					fs.setFileOptionsProvider(new FileOptionsProvider()
					{

						public Map getOptions()
						{
							Map result = new HashMap();
							String httpProxy = System.getProperty("http.proxyHost");
							String httpPort = System.getProperty("http.proxyPort");
							if (httpProxy != null)
							{
								int port = 8080;
								if (httpPort != null)
									try
									{
										port = Integer.parseInt(httpPort);
									}
									catch (Exception ex)
									{
										ex.printStackTrace();
									}
								result.put(FileOptionsProvider.PROXY_HOST, httpProxy);
								result.put(FileOptionsProvider.PROXY_PORT, port);
							}
							return result;

						}

					});
					FileSystem.setDefaultFileSystem(fs);
					// allow for conditional incldues -> first createn an empty
					// properties conf
					_fileConfiguration = new PropertiesConfiguration();
					// then set the file name and load it
					_fileConfiguration.setFileName(configFile);
					/*
					try
					{
						_fileConfiguration.setBasePath(new File(".").getCanonicalPath());
					}
					catch (IOException e)
					{
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					*/
					
					_fileConfiguration.append(_systemConfiguration);
					if (_interpolator != null)
						try
						{
							_fileConfiguration.setInterpolator(new GInterpolator(_fileConfiguration, true, null, _scriptUtils));
						}
						catch (Exception e)
						{
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						//System.out.println("platform "+_systemConfiguration.getString("platform"));
					_fileConfiguration.load();
					String encoding = _fileConfiguration.getString("wrapper.conf.encoding");
					// if we have an encoding: reload the file with the given encoding.
					if (encoding != null)
					{
						_fileConfiguration = new PropertiesConfiguration();
						_fileConfiguration.setEncoding(encoding);
						// then set the file name and load it
						_fileConfiguration.setFileName(configFile);
						/*
						try
						{
							_fileConfiguration.setBasePath(new File(".").getCanonicalPath());
						}
						catch (IOException e)
						{
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						*/
						_fileConfiguration.append(_systemConfiguration);
						if (_interpolator != null)
							try
							{
								_fileConfiguration.setInterpolator(new GInterpolator(_fileConfiguration, true, null, _scriptUtils));
							}
							catch (Exception e)
							{
								// TODO Auto-generated catch block
								e.printStackTrace();
							}

						_fileConfiguration.load();
					}

					addConfiguration(_fileConfiguration);
				}
				catch (ConfigurationException e)
				{
					log.error("error loading configuration file <init>AsjwConfiguration", e);
				}
			if (!isLocalFile())
			{
				// if no working dir is defined: set working dir to the cache
				if (_fileConfiguration.getProperty("wrapper.working.dir") == null)
					try
					{
						_fileConfiguration.setProperty("wrapper.working.dir", new File(getCache()).getCanonicalPath().replaceAll("\\\\", "/"));
					}
					catch (IOException e)
					{
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				// if no cache path is defined in the file configuration then
				// set it, so it can be accessed by the wrapper for example to
				// get
				// a system tray icon
				if (_fileConfiguration.containsKey("wrapper.cache"))
					_fileConfiguration.setProperty("wrapper.cache", getCache());
			}

		}

		// load configuration from System Environement
		//addConfiguration(getConfiguration(System.getenv()));
		//_systemConfiguration.addConfiguration(new EnvironmentConfiguration());
		_isStopper = this.getBoolean("wrapper.stopper", false);
		try
		{
			_isJavaDebug = this.getInt("wrapper.java.debug.port", -1) != -1;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		
		for (Iterator it = getKeys("wrapper.config.script"); it.hasNext(); )
		try
		{
			String key = (String) it.next();
			String script = getString(key);
			String bind = key.substring(key.lastIndexOf(".")+1);
			_scriptUtils.put(bind, new GroovyScript(script, "", null, null, 0, log, null, false));
		}
		catch (Exception e)
		{
			log.error("error reading script", e);
		}
		

		_init = true;
	}

	private boolean fileExists(String file)
	{
		try
		{
			// this hack is no longer required, changed VFS to init without providers.xm.
			//String current = System.getProperty("javax.xml.parsers.DocumentBuilderFactory");
			//System.setProperty("javax.xml.parsers.DocumentBuilderFactory", "com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderFactoryImpl");
			DefaultFileSystemManager fsManager = (DefaultFileSystemManager) VFS.getManager();
			//if (current != null)
			//	System.setProperty("javax.xml.parsers.DocumentBuilderFactory", current);
			//else
			//	System.clearProperty("javax.xml.parsers.DocumentBuilderFactory");
			FileObject f = VFSUtils.resolveFile(".", file);
			return f.exists();
		}
		catch (FileSystemException e)
		{
			e.printStackTrace();
			return false;
		}
	}

	/**
	 * Checks if is debug.
	 * 
	 * @return true, if is debug
	 */
	boolean isDebug()
	{
		return debug;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.AjswConfiguration#setDebug(boolean)
	 */
	public void setDebug(boolean debug)
	{
		this.debug = debug;
	}

	protected Object resolveContainerStore(String key)
	{
		Object result = null;
		if (key == null)
			result = null;
		if (_isJavaDebug)
		{
			if (key.equals("wrapper.startup.timeout"))
				result = Integer.MAX_VALUE / 1000;
            /*
             * djohnson
             * 
             * Allow the wrapper to kill the process using the shutdown timeout,
             * even when in debug mode (old wrapper behavior). The shutdown hook
             * of the wrapped process cannot and will not be invoked if the
             * wrapper process is signalled to shutdown prior to the wrapped
             * application starting.
             */
            // else if (key.equals("wrapper.shutdown.timeout"))
            //    result = (Integer.MAX_VALUE / 1000);
			else if (key.equals("wrapper.ping.timeout"))
				result = (Integer.MAX_VALUE / 1000);
		}
		if (result != null)
			return result;
		if (!_isStopper)
			result = super.resolveContainerStore(key);
		else if (key.startsWith("wrapper.on_exit"))
			return null;
		else if (key.startsWith("wrapper.exit_on_main_terminate"))
			result = "0";
		else if (key.startsWith("wrapper.daemon"))
			result = null;
		else if (key.contains(".script"))
			result = null;
		else if (key.contains(".filter"))
			result = null;
		else if (key.contains(".pidfile"))
			result = null;
		// allow stopper to have its own logging
		// else if (key.contains(".log"))
		// return null;
		else if (key.contains(".ntservice"))
			result = null;
		else if (key.contains(".jmx"))
			result = null;
		else if (key.contains(".lockfile"))
			result = null;
		else if (key.contains(".stop.conf"))
			result = null;
		else if (key.equals("wrapper.tray"))
			result = null;
		else
			result = super.resolveContainerStore(key);

		if (_interpolator != null && result != null && !result.equals(_interpolator.interpolate(result)))
			_interpolated.add(key);
		return result;
	}

	public Set getLookupSet()
	{
		return _interpolated;
	}
	
	public Map<String, String> getEnvLookupSet()
	{
		if (_interpolator != null)
		return ((ConfigurationBinding)((GInterpolator)_interpolator).getBinding()).getUsedEnvVars();
		return new HashMap<String, String>();
	}

	public static void main(String[] args)
	{
		YajswConfigurationImpl c = new YajswConfigurationImpl();
		c.setProperty("t1", "x");
		c.setProperty("t2", "${t1}");
		System.out.println(c.getString("t2"));
		for (Iterator it = c.getInterpolator().prefixSet().iterator(); it.hasNext();)
			System.out.println(it.next());
	}


	public CompositeConfiguration getSystemConfiguration()
	{
		return _systemConfiguration;
	}

	public void reload()
	{
		if (_fileConfiguration != null)
			_fileConfiguration.reload();
	}

	public boolean isLocalFile()
	{
		if (_fileConfiguration == null)
			return true;
		try
		{
			String name = _fileConfiguration.getFileName();
			if (name.endsWith(".jnlp"))
				return false;

			FileObject f = VFSUtils.resolveFile(".", name);
			return f instanceof LocalFile;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			return true;
		}

	}

	public String getCache()
	{
		return getString("wrapper.cache", "yajsw_cache");
	}

	public String getCachedPath()
	{
		return getCachedPath(true);
	}

	public String getCachedPath(boolean save)
	{
		if (_fileConfiguration == null)
			return null;
		if (isLocalFile())
			try
			{
				return new File(_fileConfiguration.getURL().toURI()).getCanonicalPath();
			}
			catch (IOException e)
			{
				e.printStackTrace();
				return _fileConfiguration.getFileName();
			}
			catch (URISyntaxException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		try
		{
			String cache = getCache() + "/conf";
			String fileName = _fileConfiguration.getFileSystem().getFileName(_fileConfiguration.getFileName());
			File cf = new File(cache);
			if (!cf.exists())
				cf.mkdirs();
			if (fileName.endsWith(".jnlp"))
				fileName = fileName + ".conf";
			// configuration file in the cache
			File cn = new File(cf, fileName);

			if (save)
			{
				// interpolate the file so that no includes are required
				PropertiesConfiguration c2 = (PropertiesConfiguration) _fileConfiguration.interpolatedConfiguration();

				// save the file
				c2.save(cn);
			}
			return cn.getCanonicalPath();

		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			return _fileConfiguration.getFileName();
		}

	}

	public Configuration getFileConfiguration()
	{
		// TODO Auto-generated method stub
		return _fileConfiguration;
	}

	public String getString(String key)
	{
		String value = super.getString(key);
		/* changed upon request, but should we really do this ? cross platform Portability of configurations ?
		if (value != null && 
				!key.contains("account") && 
				!key.contains("wrapper.image")  && 
				!key.contains("wrapper.app.env") &&
				!key.contains("wrapper.java.monitor.gc"))
			value = value.replaceAll("\\\\", "/");
			*/
		return value;
	}

	public long getConfigFileTime()
	{
		// TODO Auto-generated method stub
		return -1;
	}
	
	public boolean isStopper()
	{
		return _isStopper;
	}
	

}
