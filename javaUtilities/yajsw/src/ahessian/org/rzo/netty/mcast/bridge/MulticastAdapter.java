package org.rzo.netty.mcast.bridge;

import java.net.ConnectException;
import java.net.InetSocketAddress;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.rzo.netty.mcast.MulticastEndpoint;

public class MulticastAdapter
{
	private static Channel channel;
	private static MulticastEndpoint mcast = new MulticastEndpoint();
	private static long RECONNECT_DELAY = 5000; 
	private static Timer timer = new Timer();
	private static ClientBootstrap bootstrap;
	
	public static void main(String[] args) throws Exception
	{
		String host = args[0];
		int port = Integer.parseInt(args[1]);
		
		ChannelFactory factory =
            new NioClientSocketChannelFactory(
                    Executors.newCachedThreadPool(),
                    Executors.newCachedThreadPool());

        bootstrap = new ClientBootstrap(factory);
        bootstrap.setOption(
                "remoteAddress", new InetSocketAddress(host, port));

        bootstrap.setPipelineFactory(new ChannelPipelineFactory() {
            public ChannelPipeline getPipeline() {
                return Channels.pipeline(new SimpleChannelUpstreamHandler()
                {
    				@Override
    				public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
    				{
    					if (mcast != null && mcast.isInit())
    						mcast.send((ChannelBuffer) e.getMessage());
    				}
    				
    				@Override
    			    public void channelClosed(ChannelHandlerContext ctx, ChannelStateEvent e) {
    			        timer.schedule(new TimerTask() {
    			            public void run() {
     			                bootstrap.connect();
    			            }
    			        }, RECONNECT_DELAY);
    			    }
    				
    				@Override
    			    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e) {
    			        Throwable cause = e.getCause();
    			        if (cause instanceof ConnectException) 
    			        {
    			        	System.out.println("conection lost: reconnecting...");
    			        }
    			        ctx.getChannel().close();
    			    }


                	
                });
            }
        });
        
        ChannelFuture f = bootstrap.connect();
        channel = f.getChannel();
        
        mcast.init(new ChannelPipelineFactory() {
            public ChannelPipeline getPipeline() {
                return Channels.pipeline(new SimpleChannelUpstreamHandler()
                {
    				@Override
    				public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
    				{
    					ChannelBuffer b = mcast.getMessage(e);
    					if (b == null)
    						return;
    					if (channel != null && channel.isConnected())
    						channel.write(b);
    				}
                	
                });
            }
        });

		
	}

}
