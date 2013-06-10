package org.rzo.netty.ahessian.application.jmx.remote.client;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.rzo.netty.ahessian.application.jmx.remote.service.AsyncMBeanServerConnection;
import org.rzo.netty.ahessian.application.jmx.remote.service.MBeanServerConnectionAsyncAdapter;
import org.rzo.netty.ahessian.rpc.client.HessianProxyFactory;


public class Client
{
    static boolean stop = false;
    static MBeanServerConnection server;    

    public static void main(String[] args) throws Exception
    {
	
    final ExecutorService executor = Executors.newCachedThreadPool();

    // Configure the client.
    ClientBootstrap bootstrap = new ClientBootstrap(
            new NioClientSocketChannelFactory(
            		executor,
            		executor));

    bootstrap.setOption(
            "remoteAddress", new InetSocketAddress("localhost", 8080));

    bootstrap.setOption("reuseAddress", true);

    
    final HessianProxyFactory factory = new HessianProxyFactory(executor, "localhost:8080");
    bootstrap.setPipelineFactory(
            new RPCClientSessionPipelineFactory(new RPCClientMixinPipelineFactory(executor, factory), bootstrap));
    

    factory.setDisconnectedListener(new Runnable()
    {
    	public void run()
    	{
    	//stop = true;
    	}
    });

factory.setNewSessionListener(new Runnable()
{
	public void run()
	{
		stop = false;
		executor.execute(new Runnable()
		{
			public void run()
			{
			    System.out.println("started work thread");
			    Map options = new HashMap();
			    options.put("sync", true);
			    options.put("timeout", (long)10000);
			    AsyncMBeanServerConnection service = (AsyncMBeanServerConnection) factory.create(AsyncMBeanServerConnection.class, Client.class.getClassLoader(), options);
			    server = new MBeanServerConnectionAsyncAdapter(service);

				    while (!stop)
				    {
			    	try
			    	{
			    		ObjectName on = new ObjectName("java.lang:type=ClassLoading");
			    Object x  =   server.getAttribute(on, "LoadedClassCount");
			    System.out.println(x);
			    	}
			    	catch(Exception ex)
			    	{
			    		ex.printStackTrace();
			    		System.out.println(ex);
			    	}
			    try
				{
					Thread.sleep(1000);
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
				}
			    }
				    System.out.println("stopped work thread");
			}
		});
	}
});
    
     // Start the connection attempt.
    ChannelFuture future = bootstrap.connect(new InetSocketAddress("localhost", 8080));
    // Wait until the connection attempt succeeds or fails.
    Channel channel = future.awaitUninterruptibly().getChannel();
    if (future.isSuccess())
    	System.out.println("connected");
    
    // get a proxy 

    }

}
