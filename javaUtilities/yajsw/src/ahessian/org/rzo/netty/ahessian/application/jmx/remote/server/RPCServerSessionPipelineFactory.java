package org.rzo.netty.ahessian.application.jmx.remote.server;

import static org.jboss.netty.channel.Channels.pipeline;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.rzo.netty.ahessian.log.OutLogger;
import org.rzo.netty.ahessian.session.ServerSessionFilter;

public class RPCServerSessionPipelineFactory implements ChannelPipelineFactory
{

	ChannelPipelineFactory _mixinFactory;
	
	RPCServerSessionPipelineFactory(ChannelPipelineFactory mixinFactory)
	{
		_mixinFactory = mixinFactory;
	}
	
	public ChannelPipeline getPipeline() throws Exception
	{	
    ChannelPipeline pipeline = pipeline(); // Note the static import.
    pipeline.addLast("logger",new OutLogger("1"));
    pipeline.addLast("sessionFilter", new ServerSessionFilter(_mixinFactory));
    return pipeline;
	}

}
