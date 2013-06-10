package org.rzo.netty.ahessian.application.jmx.remote.client;

import java.util.concurrent.Executor;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.rzo.netty.ahessian.application.jmx.remote.service.JmxSerializerFactory;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.io.PullInputStreamConsumer;
import org.rzo.netty.ahessian.rpc.client.HessianProxyFactory;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallEncoder;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyDecoder;
import org.rzo.netty.ahessian.rpc.message.OutputProducer;
import org.rzo.netty.ahessian.session.MixinPipeline;

import com.caucho.hessian4.io.SerializerFactory;

public class RPCClientMixinPipelineFactory implements ChannelPipelineFactory
{
	
	Executor _executor;
	HessianProxyFactory _factory;
	SerializerFactory _serializerFactory = new JmxSerializerFactory();

	RPCClientMixinPipelineFactory(Executor executor, HessianProxyFactory factory)
	{
		_executor = executor;
		_factory = factory;
	}
	

	
	public ChannelPipeline getPipeline() throws Exception
	{
        ChannelPipeline pipeline = new MixinPipeline();
        // InputStreamDecoder returns an input stream and calls the next handler in a separate thread

        pipeline.addLast("inputStream", new InputStreamDecoder());

        //pipeline.addLast("logger2",new OutLogger1("2"));
        pipeline.addLast("outputStream", new OutputStreamEncoder());
        
        pipeline.addLast("hessianReplyDecoder", new PullInputStreamConsumer(new HessianRPCReplyDecoder(_factory, _serializerFactory), _executor));
        pipeline.addLast("hessianCallEncoder", new HessianRPCCallEncoder(_serializerFactory));
		pipeline.addLast("outputProducer", new OutputProducer(_executor));
        //pipeline.addLast("logger3",new OutLogger("3"));
        pipeline.addLast("hessianHandler", _factory);
        
        return pipeline;

	}


}
