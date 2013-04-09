package org.rzo.netty.mcast.bridge;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.ChildChannelStateEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.mcast.MulticastEndpoint;

public class MulticastAccessPoint
{
	
	private static List<Channel> remoteChannels = Collections.synchronizedList(new ArrayList<Channel>());
	private static MulticastEndpoint mcast = new MulticastEndpoint();

	public static void main(String[] args)
	{
		int port = Integer.parseInt(args[0]);
		
		ChannelFactory factory =
            new NioServerSocketChannelFactory(
                    Executors.newCachedThreadPool(),
                    Executors.newCachedThreadPool());

        ServerBootstrap bootstrap = new ServerBootstrap(factory);
        
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
    			    public void childChannelOpen(ChannelHandlerContext ctx, ChildChannelStateEvent e) 
    				{
    					remoteChannels.add(ctx.getChannel());
    			    }
    				
    				@Override
    			    public void childChannelClosed(ChannelHandlerContext ctx, ChildChannelStateEvent e) 
    				{
    					remoteChannels.add(ctx.getChannel());
    			    }
    				
    				@Override
    			    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e) {
    			        Throwable cause = e.getCause();
    			        System.out.println(e);    				
    			    }
                });
            }
        });
        bootstrap.bind(new InetSocketAddress(port));
        
        try
		{
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
							for (Channel c : remoteChannels)
							{
								if (c.isConnected())
									c.write(b);
							}
						}
			        	
			        });
			    }
			});
		}
		catch (Exception e)
		{
			Constants.ahessianLogger.warn("", e);
		}

	}

}
