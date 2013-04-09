package org.rzo.netty.ahessian.application.jmx.remote.client;

import static org.jboss.netty.channel.Channels.pipeline;

import java.net.ConnectException;
import java.util.Timer;
import java.util.TimerTask;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.rzo.netty.ahessian.log.OutLogger;
import org.rzo.netty.ahessian.session.ClientSessionFilter;

public class RPCClientSessionPipelineFactory implements ChannelPipelineFactory
{

	ChannelPipelineFactory _mixinFactory;
	ClientSessionFilter _sessionFilter;
	private static Timer timer = new Timer();
	private static long RECONNECT_DELAY = 5000;
	ClientBootstrap _bootstrap;


	
	RPCClientSessionPipelineFactory(ChannelPipelineFactory mixinFactory, ClientBootstrap bootstrap)
	{
		_mixinFactory = mixinFactory;
		_sessionFilter = new ClientSessionFilter(_mixinFactory);
		_bootstrap = bootstrap;
	}
	
	public ChannelPipeline getPipeline() throws Exception
	{	
    ChannelPipeline pipeline = pipeline(); // Note the static import.
    pipeline.addLast("logger",new OutLogger("1"));
    pipeline.addLast("reconnector", new SimpleChannelUpstreamHandler()
                {
    				
    				@Override
    			    public void channelClosed(ChannelHandlerContext ctx, ChannelStateEvent e) {
    					ctx.sendUpstream(e);
    					System.out.println("channel closed wait to reconnect ...");
    			        timer.schedule(new TimerTask() {
    			            public void run() {
    			            	System.out.println("reconnecting...");
     			              ChannelFuture f = _bootstrap.connect();
     			              try
							{
     			            	  System.out.println("future wait");
								f.awaitUninterruptibly();
   			            	  System.out.println("future wait terminated");
							}
							catch (Exception e)
							{
								// TODO Auto-generated catch block
								e.printStackTrace();
	 							}
   			              if (f.isSuccess())
 			            	  System.out.println("connected");
   			              else
   			              {
   			            	  System.out.println("not connected");
   			            	 // f.getChannel().close();
   			              }
    			               
    			            }
    			        }, RECONNECT_DELAY);
    			    }
    				
    				@Override
    			    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e) {
    			        Throwable cause = e.getCause();
    			        if (cause instanceof ConnectException) 
    			        {
    			        	System.out.println("conection lost");
    			        }
    			        ctx.getChannel().close();
    			    }
                }
);
    pipeline.addLast("sessionFilter", _sessionFilter);

    return pipeline;
	}

}
