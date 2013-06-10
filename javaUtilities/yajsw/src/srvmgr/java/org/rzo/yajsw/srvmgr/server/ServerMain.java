package org.rzo.yajsw.srvmgr.server;

import org.rzo.netty.ahessian.rpc.server.ContinuationService;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;
import org.rzo.netty.ahessian.rpc.server.ImmediateInvokeService;
import org.rzo.netty.mcast.discovery.DiscoveryServer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;
import org.jboss.netty.channel.socket.oio.OioServerSocketChannelFactory;
import org.rzo.yajsw.srvmgr.server.ms.win.WinServiceManagerServer;

public class ServerMain
{
	public static void main(String[] args) throws IOException, IOException, ClassNotFoundException, Exception
	{
		int serverPort = 0;
		try
		{
			serverPort = Integer.parseInt(args[0]);
		}
		catch (Exception ex)
		{
			// no port -> bind to 0 port and annouce port per multicast
		}
		String acl = null;
		int aclParPos = 1;
		if (serverPort == 0)
			aclParPos = 0;
			
		if (args.length == aclParPos+1)
			acl = args[aclParPos];
		List clientHosts = new ArrayList();
		File f = new File("serviceManagerServer.ser");
		if (f.exists())
		{
			ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
			clientHosts = (List) in.readObject();
			in.close();
		}
		
		
        Executor executor = Executors.newFixedThreadPool(200);

        // Configure the server.
        ServerBootstrap bootstrap = new ServerBootstrap(
                new OioServerSocketChannelFactory(
                		executor,
                		executor));

        HessianRPCServiceHandler factory =  new HessianRPCServiceHandler(executor);
        factory.addService("default", new ImmediateInvokeService(getServiceManagerServer(), ServiceManagerServer.class, factory));

        
        bootstrap.setPipelineFactory(
               new RPCServerPipelineFactory(executor, factory, acl));

        // Bind and start to accept incoming connections.
        Channel channel =	bootstrap.bind(new InetSocketAddress(serverPort));
        if (serverPort == 0)
        	serverPort = ((InetSocketAddress)channel.getLocalAddress()).getPort();
        
        System.out.println("bound to port "+serverPort);
        
        DiscoveryServer discovery = new DiscoveryServer();
        discovery.setName("serviceManagerServer");
        discovery.setPort(serverPort);
        discovery.init();
	}

	private static ServiceManagerServer getServiceManagerServer()
	{
		return new WinServiceManagerServer();
	}

}
