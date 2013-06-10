package org.rzo.yajsw.srvmgr.client;


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
import org.rzo.yajsw.srvmgr.server.ServiceManagerServer;

public class ClientMain
{
	static ServicesTable servicesTable;
	static HostsTable hosts;
	static HiddenTable hidden;
	static HashMap<String, AsyncServiceManagerServer>proxies = new HashMap<String, AsyncServiceManagerServer>();
	static final JFrame frame = new JFrame("Services Manager");
	static Set<String> configurations =  new HashSet<String>();
	static ExecutorService executor = Executors.newCachedThreadPool();
    static DiscoveryClient discovery = new DiscoveryClient();


	public static void main(String[] args) throws Exception
	{
	    ExecutorService executor = Executors.newCachedThreadPool();
	    ServicesForm form = new ServicesForm();
	    
	    servicesTable = new ServicesTable(form._SERVICES_TABLE);
	    hosts = new HostsTable(form._HOSTS_TABLE);
	    hidden = new HiddenTable(form._HIDDEN_TABLE);
	    loadData();
	    
	    new Timer("hosts updater", true).schedule(new TimerTask()
	    {

			@Override
			public void run()
			{
		        updateHosts();
			}
	    	
	    }, 0, 500);


	    form._NEW_HOST_BUTTON.setAction(new AbstractAction("+")
	    {
			public void actionPerformed(ActionEvent e)
			{
				doNewHost();
			}	    	
	    });
	    

	    form._DELETE_HOST_BUTTON.setAction(new AbstractAction("-")
	    {
			public void actionPerformed(ActionEvent e)
			{
				doDeleteHost();
			}	    	
	    });
	    

	    
	    form._ADD_HOSTS_BUTTON.setAction(new AbstractAction(">>")
	    {
			public void actionPerformed(ActionEvent e)
			{
				for (Host host : hosts.getSelection())
				{
					Host newHost = new Host(host.getName(), host.getPort());
					newHost.setState(host.getState());
					newHost.setIncluded(true);
					hosts.updateObject(newHost);
					servicesTable.addService(host.getName(), proxies.get(host.getName()));
				}
				saveData();
			}	    	
	    });
	    
	    form._REMOVE_HOSTS_BUTTON.setAction(new AbstractAction("<<")
	    {
			public void actionPerformed(ActionEvent e)
			{
				for (Host host : hosts.getSelection())
				{
					Host newHost = new Host(host.getName(), host.getPort());
					newHost.setState(host.getState());
					newHost.setIncluded(false);
					hosts.updateObject(newHost);
					servicesTable.removeService(host.getName());
				}
				saveData();
			}	    	
	    });
	    
	    form._ADD_HIDDEN_BUTTON.setAction(new AbstractAction("<<")
	    {
			public void actionPerformed(ActionEvent e)
			{
				for (ServiceInfo service : servicesTable.getSelection())
				{
					hidden.updateObject(service);
				}
				saveData();
			}	    	
	    });
	    
	    form._REMOVE_HIDDEN_BUTTON.setAction(new AbstractAction(">>")
	    {
			public void actionPerformed(ActionEvent e)
			{
				for (ServiceInfo service : hidden.getSelection())
				{
					hidden.removeObject(service);
				}
				saveData();
			}	    	
	    });
	    

	    form._START_BUTTON.setAction(new AbstractAction("Start")
	    {
			public void actionPerformed(ActionEvent e)
			{
				for (ServiceInfo service : servicesTable.getSelection())
				try {
					proxies.get(service.getHost()).start(service.getName());
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
			}	    	
	    });
	    
	    form._STOP_BUTTON.setAction(new AbstractAction("Stop")
	    {
			public void actionPerformed(ActionEvent e)
			{
				for (ServiceInfo service : servicesTable.getSelection())
				try {
					proxies.get(service.getHost()).stop(service.getName());
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
			}	    	
	    });
	    
	    form._INSTALL_BUTTON.setAction(new AbstractAction("Install")
	    {
			public void actionPerformed(ActionEvent e)
			{
				doInstall();
			}	    	
	    });
	    
	    form._UNINSTALL_BUTTON.setAction(new AbstractAction("Uninstall")
	    {
			public void actionPerformed(ActionEvent e)
			{
				doUninstall();
			}	    	
	    });
	    
	    form._RELOAD_CONSOLE.setAction(new AbstractAction("Reload Console App")
	    {
			public void actionPerformed(ActionEvent e)
			{
				doReloadConsole();
			}	    	
	    });
	    
	    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
	    frame.setSize(910, 690);
	    frame.setLocationRelativeTo(null);
	    frame.getContentPane().add(form);
	    frame.setVisible(true);
	    
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
				synchronized(hosts)
				{
					Host newHost = new Host(name, port);
					Host oldHost = hosts.getObject(newHost);
					if (oldHost != null)
					{
						newHost.setIncluded(oldHost.isIncluded());
						hosts.removeObject(oldHost);
					}
					newHost.setState("CONNECTED");
					hosts.updateObject(newHost);

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



	}

	protected static void doDeleteHost()
	{
		synchronized(hosts)
		{
		for (Host host : hosts.getSelection())
		{
			hosts.removeObject(host);
			servicesTable.removeService(host.getName());
		}
		saveData();
		}
	}

	protected static void doNewHost()
	{
		final NewHostDialog newDialog = new NewHostDialog();
		
    	final JDialog dialog = new JDialog(frame);
    	final Vector<String> listData = new Vector<String>();
    	newDialog._OK_BUTTON.setAction(new AbstractAction("ADD")
    	{

			public void actionPerformed(ActionEvent e)
			{
				String name = newDialog._HOST.getText();
				String stPort = newDialog._PORT.getText();
				if (name == null || name.length() == 0 || stPort == null || stPort.length() == 0)
					return;
				try
				{
				int port = Integer.parseInt(stPort);
				synchronized(hosts)
				{
				hosts.updateObject(new Host(name, port));
	    		saveData();
				}
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
			}
    		
    	});
    	newDialog._CANCEL_BUTTON.setAction(new AbstractAction("CLOSE")
    	{

			public void actionPerformed(ActionEvent e)
			{
				dialog.setVisible(false);
			}
    		
    	});
    	dialog.add(newDialog);
    	dialog.setLocationRelativeTo(frame);
    	dialog.setSize(350, 180);
    	dialog.setModalityType(ModalityType.APPLICATION_MODAL);
    	dialog.setVisible(true);
    	
	}

	protected static void doUninstall()
	{
		final UninstallDialog uninstall = new UninstallDialog();
		
    	final JDialog dialog = new JDialog(frame);
    	
    	final List<ServiceInfo> selected = servicesTable.getSelection();
		uninstall._SERVICES.setText("");
    	for (ServiceInfo info : selected)
    	{
    		uninstall._SERVICES.setText(uninstall._SERVICES.getText()+info.getHost()+"/"+info.getDisplayName()+ " ");
    	}

    	uninstall._OK_BUTTON.setAction(new AbstractAction("UNINSTALL")
    	{

			public void actionPerformed(ActionEvent e)
			{
				uninstall._OK_BUTTON.setEnabled(false);
				uninstall._SERVICES.setText("");
	        	for (ServiceInfo info : selected)
	        	{
	    			AsyncServiceManagerServer proxy = proxies.get(info.getHost());
	    			if (proxy != null)
	    			{
	    				boolean result = false;
						try
						{
							result = ((Boolean) ((Future)proxy.yajswUninstall(info.getName())).get(10, TimeUnit.SECONDS)).booleanValue();
						}
						catch (Exception e1)
						{
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
	    				if (result)
	    					uninstall._SERVICES.setText(uninstall._SERVICES.getText() + "success");
	    				else
	    					uninstall._SERVICES.setText(uninstall._SERVICES.getText() + "error");
	    			}
	    			else
	    				uninstall._SERVICES.setText(uninstall._SERVICES.getText() + "no connection");
	    				
	    		}
	        	
			}
    		
    	});
    	uninstall._CANCEL_BUTTON.setAction(new AbstractAction("CLOSE")
    	{

			public void actionPerformed(ActionEvent e)
			{
				dialog.setVisible(false);
			}
    		
    	});

    	
    	dialog.add(uninstall);
    	dialog.setLocationRelativeTo(frame);
    	dialog.setSize(500, 170);
    	dialog.setModalityType(ModalityType.APPLICATION_MODAL);
    	dialog.setVisible(true);
	}

	protected static void doReloadConsole()
	{
		final ReloadConsoleDialog reloadConsole = new ReloadConsoleDialog();
		
    	final JDialog dialog = new JDialog(frame);
    	if (servicesTable.getSelection().size() == 0)
    		return;
    	final ServiceInfo selected = servicesTable.getSelection().get(0);
    	reloadConsole._SERVICE.setText(selected.getName());
    	reloadConsole._CONFIGURATION.setModel(new DefaultComboBoxModel(new Vector(configurations)));

    	reloadConsole._OK_BUTTON.setAction(new AbstractAction("Reload")
    	{

			public void actionPerformed(ActionEvent e)
			{
				reloadConsole._OK_BUTTON.setEnabled(false);
    			AsyncServiceManagerServer proxy = proxies.get(selected.getHost());
				boolean result = false;
				try
				{
					result = ((Boolean)((Future)proxy.yajswReloadConsole(selected.getName(), (String) reloadConsole._CONFIGURATION.getSelectedItem())).get(10, TimeUnit.SECONDS)).booleanValue();
				}
				catch (Exception e1)
				{
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				if (result)
					reloadConsole._MESSAGE.setText("success");
				else
					reloadConsole._MESSAGE.setText("error");
    			if (!configurations.contains(reloadConsole._CONFIGURATION.getSelectedItem()))
    			{
    				configurations.add((String) reloadConsole._CONFIGURATION.getSelectedItem());
    				saveData();
    			}	    				
			}
    		
    	});
    	reloadConsole._CANCEL_BUTTON.setAction(new AbstractAction("CLOSE")
    	{

			public void actionPerformed(ActionEvent e)
			{
				dialog.setVisible(false);
			}
    		
    	});

    	
    	dialog.add(reloadConsole);
    	dialog.setLocationRelativeTo(frame);
    	dialog.setSize(550, 200);
    	dialog.setModalityType(ModalityType.APPLICATION_MODAL);
    	dialog.setVisible(true);
	}

	protected static void doInstall()
	{
		final InstallDialog install = new InstallDialog();
		
    	final JDialog dialog = new JDialog(frame);
    	final Vector<String> listData = new Vector<String>();
    	for (Host host : hosts.getObjectList())
    	{
    		listData.add(host.getName());
    	}
    	install._HOSTS_LIST.setListData(listData);
    	for (int i=0; i<listData.size(); i++)
    		install._HOSTS_LIST.setSelectedIndex(i);
    	List<ServiceInfo> selected = servicesTable.getSelection();
    	if (selected.size() >0)
    	{
    		ServiceInfo selection = selected.get(0);
    	}
		install._CONFIGURATION.setModel(new DefaultComboBoxModel(new Vector(configurations)));
    	install._OK_BUTTON.setAction(new AbstractAction("INSTALL")
    	{

			public void actionPerformed(ActionEvent e)
			{
	    		int[] selInd = install._HOSTS_LIST.getSelectedIndices();
	    		install._OK_BUTTON.setEnabled(false);
	    		install._MESSAGE.setText("installing");
	    		for (int i : selInd)
	    		{
	    			install._MESSAGE.setText(install._MESSAGE.getText()+ " - " + listData.get(i) + ": ");
	    			AsyncServiceManagerServer proxy = proxies.get(listData.get(i));
	    			if (proxy != null)
	    			{
	    				boolean result = false;
						try
						{
							result = ((Boolean)((Future)proxy.yajswInstall((String) install._CONFIGURATION.getSelectedItem())).get(10, TimeUnit.SECONDS)).booleanValue();
						}
						catch (Exception e1)
						{
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
	    				if (result)
	    					install._MESSAGE.setText(install._MESSAGE.getText() + "success");
	    				else
	    					install._MESSAGE.setText(install._MESSAGE.getText() + "error");
	    			}
	    			else
	    					install._MESSAGE.setText(install._MESSAGE.getText() + "no connection");
	    			if (!configurations.contains(install._CONFIGURATION.getSelectedItem()))
	    			{
	    				configurations.add((String) install._CONFIGURATION.getSelectedItem());
	    			}	    				
	    		}
	    		saveData();
			}
    		
    	});
    	install._CANCEL_BUTTON.setAction(new AbstractAction("CLOSE")
    	{

			public void actionPerformed(ActionEvent e)
			{
				dialog.setVisible(false);
			}
    		
    	});
    	dialog.add(install);
    	dialog.setLocationRelativeTo(frame);
    	dialog.setSize(570, 320);
    	dialog.setModalityType(ModalityType.APPLICATION_MODAL);
    	dialog.setVisible(true);
    	
	}

	private static void saveData()
	{
		Map data = new HashMap();
		data.put("hosts", hosts.getObjectList());
		data.put("hidden", hidden.getObjectList());
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
		File f = new File("ServiceManager.ser");
		try
		{
		if (!f.exists())
			return;
		ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
		Map data = (Map) in.readObject();
		for (Iterator it = ((Collection)data.get("hosts")).iterator(); it.hasNext(); )
			hosts.updateObject((Host)it.next());
		for (Iterator it = ((Collection)data.get("hidden")).iterator(); it.hasNext(); )
			hidden.updateObject((ServiceInfo)it.next());
		configurations = (Set<String>) data.get("configurations");
		in.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}	
		
	}

	private static void updateHosts() 
	{
		boolean changed = false;
		synchronized(hosts)
		{
		for (Host host : hosts.getObjectList())
		{
			AsyncServiceManagerServer proxy = proxies.get(host.getName());
			boolean connected = false;
			if (proxy != null)
			{
				try
				{
					connected = ((Boolean)((Future)proxy.isServiceManager()).get(10, TimeUnit.SECONDS)).booleanValue();
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
						proxy = (AsyncServiceManagerServer) factory.create(AsyncServiceManagerServer.class, ClientMain.class.getClassLoader(), options);
						connected = ((Boolean)((Future)proxy.isServiceManager()).get(10, TimeUnit.SECONDS)).booleanValue();
						if (connected)
						{
						proxies.put(host.getName(), proxy);
						Host newHost = new Host(host.getName(), host.getPort());
						newHost.setIncluded(host.isIncluded());
						newHost.setState("CONNECTED");
						hosts.updateObject(newHost);
						if (host.isIncluded())
							servicesTable.addService(host.getName(), proxy);
						}
						else
							future.getChannel().close();
					}
				}
				catch (Exception e)
				{
					System.out.println("error accessing "+host.getName());
					connected = false;
					if (future != null)
						future.getChannel().close();
				}
				
			}
				
			if (!connected)
			{
				disconnect(host, proxies.remove(host.getName()));
				changed = true;
			}
			else if (proxy == null && !"DISCONNECTED".equals(host.getState()))
			{
				Host newHost = new Host(host.getName(), host.getPort());
				newHost.setIncluded(host.isIncluded());
				newHost.setState("DISCONNECTED");
				hosts.updateObject(newHost);
			}
		}
		}
	}
	
	private static void disconnect(Host host, AsyncServiceManagerServer proxy)
	{
		if (proxy != null)
			servicesTable.removeService(host.getName());
		Host newHost = new Host(host.getName(), host.getPort());
		newHost.setIncluded(host.isIncluded());
		newHost.setState("DISCONNECTED");
		hosts.updateObject(newHost);
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
