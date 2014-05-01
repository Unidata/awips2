package org.rzo.yajsw.tray.ahessian.client;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.management.MBeanServerConnection;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.jboss.netty.util.HashedWheelTimer;
import org.rzo.netty.ahessian.application.jmx.remote.service.AsyncMBeanServerConnection;
import org.rzo.netty.ahessian.application.jmx.remote.service.MBeanServerConnectionAsyncAdapter;
import org.rzo.netty.ahessian.rpc.client.BootstrapProvider;
import org.rzo.netty.ahessian.rpc.client.HessianProxyFactory;
import org.rzo.netty.ahessian.utils.MyReentrantLock;
import org.rzo.netty.mcast.discovery.DiscoveryClient;
import org.rzo.netty.mcast.discovery.DiscoveryListener;

public class AHessianJmxClient implements BootstrapProvider
{
	boolean					stop		= false;
	DiscoveryClient			discovery	= null;
	ExecutorService			executor	= Executors.newCachedThreadPool();
	ClientBootstrap			bootstrap	= new ClientBootstrap(new NioClientSocketChannelFactory(executor, executor));
	MBeanServerConnection	mbeanServer;
	final Lock				lock		= new MyReentrantLock();
	final Condition			connected	= lock.newCondition();
	String					currentHost	= null;
	HessianProxyFactory		factory;

	public AHessianJmxClient(String discoveryName, int port) throws Exception
	{
		factory = new HessianProxyFactory(executor, "AHessianJMX", null);

		// in case we are disconnected: start the discovery
		factory.setDisconnectedListener(new Runnable()
		{
			public void run()
			{
				try
				{
					close();
					if (discovery != null)
					{
						if (currentHost != null)
							discovery.removeHost(currentHost);
						discovery.start();
					}
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
			}
		});

		factory.setConnectedListener(new Runnable()
		{

			public void run()
			{
				// if (discovery == null)
				doConnected();
			}

		});

		bootstrap.setOption("reuseAddress", true);

		// if we do not have a port: use discovery
		if (port == 0)
		{
			discovery = new DiscoveryClient();
			bootstrap.setPipelineFactory(new AHessianClientPipelineFactory(executor, factory, null));

			discovery.setName(discoveryName);
			discovery.addListener(new DiscoveryListener()
			{
				// we have discovered our mbean server
				public void newHost(String name, String host)
				{
					try
					{
						// get the port - hostName should be the local host
						String[] x = host.split("&");
						int port = Integer.parseInt(x[2]);
						String hostName = x[1];
						// try to connect
						ChannelFuture future = bootstrap.connect(new InetSocketAddress(hostName, port));
						// future.await(10000);

						// stop discovery
						discovery.stop();
						// doConnected();
						currentHost = host;
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
		// if we have a port connect to that port
		else
		{
			bootstrap.setOption("remoteAddress", new InetSocketAddress("localhost", port));
			bootstrap.setPipelineFactory(new AHessianClientPipelineFactory(executor, factory, this, new HashedWheelTimer()));
			bootstrap.connect();
		}
	}

	private void doConnected()
	{
		lock.lock();
		Map options = new HashMap();
		options.put("sync", true);
		options.put("timeout", (long) 2000);
		// we will be using a synchronous service
		AsyncMBeanServerConnection asyncService = (AsyncMBeanServerConnection) factory.create(AsyncMBeanServerConnection.class,
				AHessianJmxClient.class.getClassLoader(), options);
		mbeanServer = new MBeanServerConnectionAsyncAdapter(asyncService);
		connected.signal();
		lock.unlock();
	}

	public MBeanServerConnection getMBeanServer()
	{
		while (mbeanServer == null && !stop)

		{
			lock.lock();
			try
			{
				connected.await(1000, TimeUnit.MILLISECONDS);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
			}
			lock.unlock();
		}

		return mbeanServer;
	}

	public void close()
	{
		factory.setBlocked(true);
		factory.invalidateAllPendingCalls();
		factory.invalidateProxies();
		mbeanServer = null;
		if (factory.getChannel() != null && factory.getChannel().isConnected())
			factory.getChannel().close();
	}

	public void open()
	{
		factory.setBlocked(false);
	}

	public void stop()
	{
		stop = true;
	}

	public ClientBootstrap getBootstrap()
	{
		return bootstrap;
	}

}
