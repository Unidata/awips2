package org.rzo.netty.ahessian.application.jmx.remote.server;

import java.util.ArrayList;
import java.util.concurrent.Executor;

import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.rzo.netty.ahessian.application.jmx.remote.service.JmxSerializerFactory;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.io.PullInputStreamConsumer;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallDecoder;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyEncoder;
import org.rzo.netty.ahessian.rpc.message.OutputProducer;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;
import org.rzo.netty.ahessian.rpc.server.ImmediateInvokeService;
import org.rzo.netty.ahessian.session.MixinPipeline;

import com.caucho.hessian4.io.SerializerFactory;

public class RPCServerMixinPipelineFactory implements ChannelPipelineFactory
{
	
	Executor _executor;
	SerializerFactory _serializerFactory = new JmxSerializerFactory();

	RPCServerMixinPipelineFactory(Executor executor)
	{
		_executor = executor;
	}
	
	public ChannelPipeline getPipeline() throws Exception
	{
		ChannelPipeline pipeline = new MixinPipeline();
        pipeline.addLast("inputStream", new InputStreamDecoder());
        //pipeline.addLast("logger2",new OutLogger("2"));
        pipeline.addLast("outputStream", new OutputStreamEncoder());
        pipeline.addLast("callDecoder", new PullInputStreamConsumer(new HessianRPCCallDecoder(_serializerFactory), _executor ));
        pipeline.addLast("replyEncoder", new HessianRPCReplyEncoder(_serializerFactory));
		pipeline.addLast("outputProducer", new OutputProducer(_executor));
        //pipeline.addLast("logger3",new OutLogger("3"));
        HessianRPCServiceHandler factory =  new HessianRPCServiceHandler(_executor);
		ArrayList servers = MBeanServerFactory.findMBeanServer(null);
		MBeanServer server = null;
			if (servers != null && servers.size() > 0)
				server = (MBeanServer) servers.get(0);
			if (server == null)
				server = MBeanServerFactory.createMBeanServer();
		

        //factory.addService("default", new ContinuationService(new ContinuationHalloWorldService(), HelloWorldServiceInterface.class, factory, _executor));
        factory.addService("default", new ImmediateInvokeService(server, MBeanServerConnection.class, factory));
        pipeline.addLast("hessianRPCServer", factory);
        
        //bootstrap.getPipeline().addLast("logger4",new OutLogger("4"));
        return pipeline;
	}

}
