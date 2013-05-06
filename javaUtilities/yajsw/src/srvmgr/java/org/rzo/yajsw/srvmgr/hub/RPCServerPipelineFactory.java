package org.rzo.yajsw.srvmgr.hub;

import static org.jboss.netty.channel.Channels.pipeline;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallDecoder;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyEncoder;
import org.rzo.netty.ahessian.rpc.message.OutputProducer;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;
import org.rzo.netty.ahessian.auth.EncryptedAuthToken;
import org.rzo.netty.ahessian.auth.ServerAuthFilter;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.io.PushInputStreamConsumer;
import org.rzo.netty.ahessian.log.OutLogger;

import java.util.concurrent.Executor;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.handler.ipfilter.IpFilterRuleHandler;
import org.jboss.netty.handler.ipfilter.IpFilterRuleList;

import org.rzo.netty.ahessian.rpc.server.HessianSkeleton;

public class RPCServerPipelineFactory implements ChannelPipelineFactory
{
	Executor _executor;
	HessianRPCServiceHandler _handler;
	String _acl = null;

	RPCServerPipelineFactory(Executor executor, HessianRPCServiceHandler handler, String acl)
	{
		_executor = executor;
		_handler = handler;
		_acl = acl;
	}
	
	public ChannelPipeline getPipeline() throws Exception
	{
	    ChannelPipeline pipeline = pipeline(); // Note the static import.
	    if (_acl != null)
	    {
	    	
	    	pipeline.addFirst("firewall", new IpFilterRuleHandler(new IpFilterRuleList(_acl)));
	    }
        pipeline.addLast("logger",new OutLogger("server"));
        pipeline.addLast("inputStream", new InputStreamDecoder());
         pipeline.addLast("outputStream", new OutputStreamEncoder());
         pipeline.addLast("callDecoder", new PushInputStreamConsumer(new HessianRPCCallDecoder(), _executor));
         pipeline.addLast("replyEncoder", new HessianRPCReplyEncoder());
         pipeline.addLast("outputProducer", new OutputProducer(_executor));
        pipeline.addLast("hessianRPCServer", _handler);
        
        //bootstrap.getPipeline().addLast("logger4",new OutLogger("4"));
        return pipeline;
	}

}
