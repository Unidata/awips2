package org.rzo.netty.mcast;


import java.net.*;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Random;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ConnectionlessBootstrap;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.socket.DatagramChannel;
import org.jboss.netty.channel.socket.DatagramChannelFactory;
import org.jboss.netty.channel.socket.oio.OioDatagramChannelFactory;

public class MulticastEndpoint
{
	
    private String mcastGroupIp = "228.10.10.10";
    private int mcastGroupPort = 12345;
    private String bindAddress = "192.168.0.10";
    
    private DatagramChannel datagramChannel;
    private ConnectionlessBootstrap connectionlessBootstrap;
    private InetSocketAddress multicastAddress;
    private static Executor executor = Executors.newCachedThreadPool();
    byte[] id;
    boolean init = false;
    
	public void  init(ChannelPipelineFactory factory) throws Exception
	{
			id = String.format("%1$020d", Math.abs(new Random(System.currentTimeMillis()).nextLong())).getBytes();
			DatagramChannelFactory datagramChannelFactory = new
	        OioDatagramChannelFactory(executor);

	         connectionlessBootstrap = new
	        ConnectionlessBootstrap(datagramChannelFactory);
	        connectionlessBootstrap.setOption("broadcast", true);
	        connectionlessBootstrap.setPipelineFactory(factory);
	        datagramChannel = (DatagramChannel)
	        connectionlessBootstrap.bind(new InetSocketAddress(mcastGroupPort));
	        multicastAddress = new InetSocketAddress(mcastGroupIp, mcastGroupPort);
	        NetworkInterface networkInterface =
	        NetworkInterface.getByInetAddress(InetAddress.getByName(bindAddress));
	        //for (Enumeration nifs = NetworkInterface.getNetworkInterfaces(); nifs.hasMoreElements(); )
	        datagramChannel.joinGroup(multicastAddress, null);//(NetworkInterface) nifs.nextElement());
	        init = true;
	}
	
	public boolean isInit()
	{
		return init;
	}
	
	public void send(ChannelBuffer msg) throws Exception
	{
		ChannelBuffer idbuf = ChannelBuffers.wrappedBuffer(id);
		datagramChannel.write(ChannelBuffers.wrappedBuffer(idbuf, msg), multicastAddress);
	}

	public String getMcastGroupIp()
	{
		return mcastGroupIp;
	}

	public int getMcastGroupPort()
	{
		return mcastGroupPort;
	}

	public String getBindAddress()
	{
		return bindAddress;
	}

	public void setMcastGroupIp(String mcastGroupIp)
	{
		this.mcastGroupIp = mcastGroupIp;
	}

	public void setMcastGroupPort(int mcastGroupPort)
	{
		this.mcastGroupPort = mcastGroupPort;
	}

	public void setBindAddress(String bindAddress)
	{
		this.bindAddress = bindAddress;
	}
	
	public void close()
	{
		datagramChannel.close();
		connectionlessBootstrap.releaseExternalResources();
	}
	
	public ChannelBuffer getMessage(MessageEvent e)
	{
		if (checkMessage(e))
		{
			ChannelBuffer m = (ChannelBuffer) e.getMessage();
			return m.slice(id.length, m.readableBytes()-id.length);
		}
		return null;
	}
	
	public String getStringMessage(MessageEvent e)
	{
		ChannelBuffer m = getMessage(e);
		if (m == null)
			return null;
		return m.toString(Charset.defaultCharset());
	}
	
	public boolean checkMessage(MessageEvent e)
	{
		byte[] eId = new byte[id.length];
		((ChannelBuffer) e.getMessage()).getBytes(0, eId, 0, eId.length); 
		return (! Arrays.equals(id, eId));
	}
	

}
