package org.rzo.yajsw.srvmgr.hub;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.socket.oio.OioServerSocketChannelFactory;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;
import org.rzo.netty.ahessian.rpc.server.ImmediateInvokeService;
import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.srvmgr.client.AsyncServiceManagerServer;
import org.rzo.yajsw.srvmgr.client.Host;

public class HubServiceServer implements HubService
{
	HashMap<String, AsyncServiceManagerServer> _proxies;
	
	Comparator<Host> hostsComparator = new Comparator<Host>()
	{
		public int compare(Host o1, Host o2)
		{
			return o1.getName().compareTo(o2.getName());
		}		
	};

	Comparator<ServiceInfo> servicesComparator = new Comparator<ServiceInfo>()
	{
		public int compare(ServiceInfo o1, ServiceInfo o2)
		{
			return o1.getName().compareTo(o2.getName());
		}		
	};

	HubServiceServer(int port, String acl, HashMap<String, AsyncServiceManagerServer>proxies)
	{
		_proxies = proxies;
        Executor executor = Executors.newFixedThreadPool(200);

        // Configure the server.
        ServerBootstrap bootstrap = new ServerBootstrap(
                new OioServerSocketChannelFactory(
                		executor,
                		executor));

        HessianRPCServiceHandler factory =  new HessianRPCServiceHandler(executor);
        factory.addService("default", new ImmediateInvokeService(this, HubService.class, factory));

        
        bootstrap.setPipelineFactory(
               new RPCServerPipelineFactory(executor, factory, acl));

        // Bind and start to accept incoming connections.
        Channel channel =	bootstrap.bind(new InetSocketAddress(port));
	}

	public List<Host> getHosts()
	{
		List<Host> result;
		synchronized(HubMain.hostsList)
		{
		  result = new ArrayList(HubMain.hostsList.values());
		}
		Collections.sort(result, hostsComparator);
		System.out.println("getHosts #"+result.size());
		return result;
	}

	public List<ServiceInfo> getServices()
	{
		List<ServiceInfo> result;
		synchronized(HubMain.servicesList)
		{
		  result = new ArrayList(HubMain.servicesList);
		}
		Collections.sort(result, servicesComparator);
		System.out.println("getServices #"+result.size());
		return result;
	}

	public void hide(String serviceName, String hostName)
	{
		synchronized(HubMain.hiddenList)
		{
		System.out.println("hiding "+serviceName);
		HubMain.hiddenList.add(serviceName);
		}
	}

	public void start(String serviceName, String hostName)
	{
		AsyncServiceManagerServer proxy = _proxies.get(hostName);
		if (proxy != null)
			proxy.start(serviceName);
	}

	public void stop(String serviceName, String hostName)
	{
		AsyncServiceManagerServer proxy = _proxies.get(hostName);
		if (proxy != null)
			proxy.stop(serviceName);
	}

}
