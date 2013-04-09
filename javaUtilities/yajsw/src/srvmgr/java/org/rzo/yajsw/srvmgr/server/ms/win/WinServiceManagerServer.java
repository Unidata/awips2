package org.rzo.yajsw.srvmgr.server.ms.win;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.Constants;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Service;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.os.ServiceInfoImpl;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32.PROCESSENTRY32;
import org.rzo.yajsw.srvmgr.server.ServiceManagerServer;
import org.rzo.yajsw.tools.JCLParser;
import org.rzo.yajsw.wrapper.AbstractWrappedProcessMBean;
import org.rzo.yajsw.wrapper.WrappedService;

import com.sun.jna.Native;

public class WinServiceManagerServer implements ServiceManagerServer
{
	Map<String, ServiceInfo> _services;
	long _lastListGet = 0;
	long _listTTL = 5000;
	Map<Integer, Process> _processes = new HashMap<Integer, Process>();

	JMXConnector jmxc;
	
	
	public ServiceInfo getService(String name)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public synchronized Map<String, ServiceInfo> getServiceList()
	{
		if (_services == null || (System.currentTimeMillis() - _listTTL) > _lastListGet)
		{
			_services = OperatingSystem.instance().serviceManagerInstance().getServiceList();
			setServicesAppPid();
			_services.putAll(getYAJSWConsoles());
			_lastListGet = System.currentTimeMillis();
		}
		return _services;
	}

	private void setServicesAppPid()
	{
		for (ServiceInfo service : _services.values())
		{
			if (service.getWrapped() != null && !"-".equals(service.getWrapped()))
				((ServiceInfoImpl)service).setWrapperAppPid(getAppPid(service.getPid()));
		}
	}

	private int getAppPid(int pid)
	{
		int result = 0;
		List<Integer> pids = OperatingSystem.instance().processManagerInstance().getProcessTree(pid);
		if (pids != null && !pids.isEmpty())
		{
				for (Integer pp :  pids)
					{
					if (pp != pid)
					{
					 Process ppp = OperatingSystem.instance().processManagerInstance().getProcess(pp);
					 if (ppp == null)
					 {
						 System.out.println("no access right to pid "+pp);
						 continue;
					 }
					 if (ppp.getCommand().trim().endsWith("Main")||ppp.getCommand().trim().endsWith("Main\""))
						 result = pp;
					}
		}
	}
		return result;
}

	private Map<String, ServiceInfo> getYAJSWConsoles()
	{
		Map<String, ServiceInfo> result = new HashMap<String, ServiceInfo>();
		Map<Integer, Integer> processes = WindowsXPProcess.getProcessMaps(0)[0];
		for (Integer pid : processes.keySet())
		{
			Process p = _processes.get(pid);
			if (p == null)
			{
				p = WindowsXPProcess.getProcess(pid);
				if (p == null)
					continue;
				_processes.put(pid, p);
			}
			String cmd = p.getCommand();
			if (cmd != null && cmd.contains("wrapper.jar\" -c"))
			{
				JCLParser jp = JCLParser.parse(cmd);
				ServiceInfoImpl service = new ServiceInfoImpl();
				service.setCommand(cmd);
				String confFile = jp.getArgs().get(1);
				if (confFile == null)
					continue;
				File f = new File(confFile);
				if (!f.exists())
					f = new File(p.getWorkingDir(), confFile);
				service.setDisplayName(f.getAbsolutePath());
				service.setName(jp.getArgs().get(1));
				service.setPid(pid);
				service.setWrapperAppPid(getAppPid(pid));
				service.setAccount(p.getUser());
				result.put(service.getName(), service);
			}
			Set<Integer> newPids = new HashSet<Integer>(processes.keySet());
			Set<Integer> toRemove = new HashSet<Integer>(_processes.keySet());
			toRemove.removeAll(newPids);
			for (Integer x : toRemove)
				_processes.remove(x);
		}
		
		return result;
	}

	public boolean start(String name)
	{
		if (isService(name))
		return OperatingSystem.instance().serviceManagerInstance().getService(name).start();
		else
			return startYAJSW(name);			
	}

	private boolean startYAJSW(String name)
	{
		AbstractWrappedProcessMBean proxy = getJmxProxy(name);
		if (proxy == null)
			return false;
		try
		{
		proxy.start();
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		try
		{
			jmxc.close();
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	private boolean isService(String name)
	{
		ServiceInfo s = _services.get(name);
		return !"Console".equals(s.getWrapped());
	}

	public boolean stop(String name)
	{
		if (isService(name))
		return OperatingSystem.instance().serviceManagerInstance().getService(name).stop();
		else
			return stopYAJSW(name);
	}

	private boolean stopYAJSW(String name)
	{
		AbstractWrappedProcessMBean proxy = getJmxProxy(name);
		if (proxy == null)
			return false;
		try
		{
		proxy.stop();
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		try
		{
			jmxc.close();
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	private AbstractWrappedProcessMBean getJmxProxy(String name)
	{
		AbstractWrappedProcessMBean	proxy = null;
		try
		{
		String config = name;
		Process p = _processes.get(_services.get(name).getPid());
		File f = new File(p.getWorkingDir(), config);
		if (!f.exists())
		{
			System.out.println("file not found "+f.getAbsolutePath());
			//return null;
		}
		Configuration localConf = new BaseConfiguration();
		localConf.addProperty("wrapper.config", config);
		YajswConfigurationImpl _config = new YajswConfigurationImpl(localConf, true);
		int port = _config.getInt("wrapper.jmx.rmi.port", Constants.DEFAULT_RMI_PORT);
		String xname = _config.getString("wrapper.console.title");
		if (xname == null)
			xname = _config.getString("wrapper.ntservice.name");
		if (xname == null)
			xname = "yajsw.noname";
		ObjectName oName = new ObjectName("org.rzo.yajsw", "name", xname);
		JMXServiceURL url = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://localhost:"+port+"/server");
		 jmxc = JMXConnectorFactory.connect(url, null);
	    MBeanServerConnection mbsc = jmxc.getMBeanServerConnection();
	    	proxy = (AbstractWrappedProcessMBean)
	        MBeanServerInvocationHandler.newProxyInstance( 
	                                       mbsc, 
	                                       oName, 
	                                       AbstractWrappedProcessMBean.class, 
	                                       false);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		return proxy;

	}

	public boolean yajswInstall(String configuration)
	{
			WrappedService w = new WrappedService();
			Configuration c = w.getLocalConfiguration();
			c.setProperty("wrapper.config", configuration);
			w.init();
			return w.install();
	}

	public boolean yajswUninstall(String name)
	{
		if (_services.get(name) != null && "Service".equals(_services.get(name).getWrapped()))
		{
			Service service = OperatingSystem.instance().serviceManagerInstance().getService(name);
			service.stop();
			return service.uninstall();
		}
		else
			return false;
	}
	
	public boolean yajswReloadConsole(String name, String newConfig)
	{
		AbstractWrappedProcessMBean proxy = getJmxProxy(name);
		if (proxy == null)
			return false;
		try
		{
		proxy.setProperty("wrapper.config", newConfig);
		proxy.resetCache();
		proxy.init();
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			return false;
		}
		return true;

	}

	public boolean isServiceManager()
	{
		return true;
	}


}
