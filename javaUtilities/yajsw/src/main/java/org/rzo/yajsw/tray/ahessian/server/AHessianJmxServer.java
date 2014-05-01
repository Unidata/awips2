package org.rzo.yajsw.tray.ahessian.server;

import java.net.InetSocketAddress;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.management.MBeanServer;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;
import org.jboss.netty.handler.ipfilter.IpFilterRuleHandler;
import org.jboss.netty.handler.ipfilter.IpFilterRuleList;
import org.rzo.netty.mcast.discovery.DiscoveryServer;

public class AHessianJmxServer
{
	public AHessianJmxServer(MBeanServer mbeanServer, String ipFilter, String serviceDiscoveryName, int port, Logger log)
	{
		Executor executor = Executors.newFixedThreadPool(10);

		// Configure the server.
		ServerBootstrap bootstrap = new ServerBootstrap(new NioServerSocketChannelFactory(executor, executor));

		bootstrap
				.setPipelineFactory(new AHessianServerPipelineFactory(executor, new IpFilterRuleHandler(new IpFilterRuleList(ipFilter)), mbeanServer, log));

		int serverPort = port;
		// Bind and start to accept incoming connections.
		Channel channel = bootstrap.bind(new InetSocketAddress(serverPort));
		if (serverPort == 0)
			serverPort = ((InetSocketAddress) channel.getLocalAddress()).getPort();

		log.info("ahessian jmx service bound to port " + serverPort);

		DiscoveryServer discovery = new DiscoveryServer();
		// allow discovery only from localhost. other computers will be ignored
		discovery.setIpSet(new IpFilterRuleList("+n:localhost, -n:*"));
		discovery.setName(serviceDiscoveryName);
		discovery.setPort(serverPort);
		try
		{
			discovery.init();
		}
		catch (Exception e)
		{
			log.log(Level.SEVERE, "error starting tray icon server", e);
		}

	}

}
