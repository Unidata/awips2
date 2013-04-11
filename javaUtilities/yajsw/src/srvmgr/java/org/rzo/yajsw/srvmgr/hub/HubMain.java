package org.rzo.yajsw.srvmgr.hub;


import org.rzo.netty.ahessian.rpc.client.HessianProxyFactory;
import org.rzo.netty.mcast.discovery.DiscoveryClient;
import org.rzo.netty.mcast.discovery.DiscoveryListener;
import org.rzo.netty.mcast.discovery.DiscoveryServer;

import java.awt.BorderLayout;
import java.awt.Dialog.ModalityType;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.jboss.netty.channel.socket.oio.OioClientSocketChannelFactory;

import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.os.ServiceInfoImpl;
import org.rzo.yajsw.srvmgr.client.AsyncServiceManagerServer;
import org.rzo.yajsw.srvmgr.client.Host;
import org.rzo.yajsw.srvmgr.client.RPCClientPipelineFactory;
import org.rzo.yajsw.srvmgr.server.ServiceManagerServer;

public class HubMain
{
	static List<ServiceInfo> servicesList = new ArrayList<ServiceInfo>();
	static Map<String, Host> hostsList = new HashMap<String, Host>();
	static Set<String> hiddenList = new HashSet();
	static HashMap<String, AsyncServiceManagerServer>proxies = new HashMap<String, AsyncServiceManagerServer>();
	static Set<String> configurations =  new HashSet<String>();
	static ExecutorService executor = Executors.newCachedThreadPool();
    static DiscoveryClient discovery = new DiscoveryClient();
    static HubServiceServer hubServiceServer;


	public static void main(String[] args) throws Exception
	{
	    ExecutorService executor = Executors.newCachedThreadPool();
	    
	    loadData();
	    
	    new Timer("hosts updater", true).schedule(new TimerTask()
	    {

			@Override
			public void run()
			{
		        updateHosts();
		        updateServices();
			}

	    	
	    }, 0, 500);


        discovery.setName("serviceManagerServer");
        discovery.addListener(new DiscoveryListener()
        {

			public void newHost(String serviceName, String host)
			{
				try
				{
					String[] x = host.split(":");
				int port = Integer.parseInt(x[1]);
				String name = InetAddress.getByName(x[0]).getHostName();
				synchronized(hostsList)
				{
					System.out.println("new host "+name+":"+port);
					Host newHost = new Host(name, port);
					Host oldHost = hostsList.get(newHost.getName());
					if (oldHost != null)
					{
						newHost.setIncluded(oldHost.isIncluded());
					}
				hostsList.put(newHost.getName(), newHost);

	    		saveData();
				}
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
				
			}
        	
        });
        discovery.init();
        discovery.start();


        hubServiceServer = new HubServiceServer(Integer.parseInt(args[0]), null, proxies);

	}

	protected static void doDeleteHost(String host)
	{
		synchronized(hostsList)
		{
		synchronized(servicesList)
		{
			hostsList.remove(host);
			for (ServiceInfo service : new ArrayList<ServiceInfo>(servicesList))
				if (host.equals(service.getHost()))
						servicesList.remove(service);
		saveData();
		}
		}
	}
	

	protected static void doNewHost(String host, int port)
	{
		synchronized(hostsList)
		{
			Host newHost = new Host(host, port);
			Host oldHost = hostsList.get(newHost.getName());
			if (oldHost != null)
			{
				newHost.setIncluded(oldHost.isIncluded());
			}
			newHost.setState("CONNECTED");
			hostsList.put(newHost.getName(), newHost);

		saveData();
		}

    	
	}

	private static void saveData()
	{
		Map data = new HashMap();
		data.put("hosts", hostsList);
		data.put("hidden", hiddenList);
		data.put("configurations", configurations);
		File f = new File("ServiceManager.ser");
		try
		{
		if (!f.exists())
			f.createNewFile();
		ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(f));
		out.writeObject(data);
		out.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}	
	}
	
	private static void loadData()
	{
		File f = new File("ServiceManagerHub.ser");
		try
		{
		if (!f.exists())
			return;
		ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
		Map data = (Map) in.readObject();
		for (Iterator it = ((Collection)data.get("hosts")).iterator(); it.hasNext(); )
		{
			Host host = (Host) it.next();
			hostsList.put(host.getName(), host);
		}
		for (Iterator it = ((Collection)data.get("hidden")).iterator(); it.hasNext(); )
			hiddenList.add((String)it.next());
		configurations = (Set<String>) data.get("configurations");
		in.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}	
		
	}
	
	private static void updateServices()
	{
		synchronized(hostsList)
		{
		synchronized(proxies)
			{

		synchronized(servicesList)
		{
		servicesList.clear();
		for (String host : hostsList.keySet())
		{
			AsyncServiceManagerServer proxy = proxies.get(host);
			Collection<ServiceInfo> services = null;
			try
			{
				 services = (Collection<ServiceInfo>) ((Map<String, ServiceInfo>) proxy.getServiceList()).values();
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
			if (services != null)
			for (ServiceInfo service : services)
			{
				synchronized(hiddenList)
				{
				if (! hiddenList.contains(service.getName()))
				{
					((ServiceInfoImpl)service).setHost(host);
					servicesList.add(service);
				}
				}
			}
		}
		}
		}
		}
		System.out.println("update services: #"+servicesList.size());
	}


	private static void updateHosts() 
	{
		boolean changed = false;
		synchronized(hostsList)
		{
		for (Host host : hostsList.values())
		{
			AsyncServiceManagerServer proxy = null;
			synchronized(proxies)
			{
			 proxy = proxies.get(host.getName());
			}
			boolean connected = false;
			if (proxy != null)
			{
				try
				{
					connected = ((Boolean)proxy.isServiceManager()).booleanValue();
				}
				catch (Exception e)
				{
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			else
			{
			    ClientBootstrap bootstrap = new ClientBootstrap(
			            new NioClientSocketChannelFactory(
			            		executor,
			            		executor));
			    
			    HessianProxyFactory factory = new HessianProxyFactory(executor, host.getName()+":"+host.getPort());
			    bootstrap.setPipelineFactory(
			            new RPCClientPipelineFactory(executor, factory));
			    
			     // Start the connection attempt.
			    ChannelFuture future = bootstrap.connect(new InetSocketAddress(host.getName(), host.getPort()));
			    try
				{
					future.await(10000);
					connected = future.isSuccess();


					if (connected)
					{
					    Map options = new HashMap();
					    options.put("sync", true);
					    options.put("timeout", new Long(10000));
						proxy = (AsyncServiceManagerServer) factory.create(AsyncServiceManagerServer.class, HubMain.class.getClassLoader(), options);
						connected = ((Boolean)proxy.isServiceManager()).booleanValue();
						if (connected)
						{
							synchronized(proxies)
							{
						proxies.put(host.getName(), proxy);
							}
						Host newHost = new Host(host.getName(), host.getPort());
						newHost.setIncluded(true);
						newHost.setState("CONNECTED");
						hostsList.remove(host.getName());
						hostsList.put(newHost.getName(), newHost);
						//if (host.isIncluded())
						// TODO	servicesList.addService(host.getName(), proxy);
						}
						else
							future.getChannel().close();
					}
				}
				catch (Exception e)
				{
					System.out.println("error accessing "+host.getName());
					e.printStackTrace();
					
					connected = false;
					if (future != null)
						future.getChannel().close();
				}
				
			}
				
			if (!connected)
			{
				synchronized(proxies)
				{
				disconnect(host, proxies.remove(host.getName()));
				}
				changed = true;
			}
			else if (proxy == null && !"DISCONNECTED".equals(host.getState()))
			{
				Host newHost = new Host(host.getName(), host.getPort());
				newHost.setIncluded(host.isIncluded());
				newHost.setState("DISCONNECTED");
				hostsList.put(newHost.getName(), newHost);
			}
		}
		}
	}
	
	private static void removeServices(String host)
	{
		
	}
	
	private static void disconnect(Host host, AsyncServiceManagerServer proxy)
	{
		if (proxy != null)
			removeServices(host.getName());
		host.setState("DISCONNECTED");
		try
		{
			discovery.removeHost(InetAddress.getByName(host.getName()).getHostAddress()+":"+host.getPort());
		}
		catch (Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	



}
