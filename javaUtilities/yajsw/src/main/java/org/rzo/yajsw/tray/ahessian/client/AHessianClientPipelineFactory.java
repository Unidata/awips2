package org.rzo.yajsw.tray.ahessian.client;

import java.util.concurrent.Executor;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.util.Timer;
import org.rzo.netty.ahessian.application.jmx.remote.service.JmxSerializerFactory;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.io.PullInputStreamConsumer;
import org.rzo.netty.ahessian.rpc.client.BootstrapProvider;
import org.rzo.netty.ahessian.rpc.client.HessianProxyFactory;
import org.rzo.netty.ahessian.rpc.client.ReconnectHandler;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallEncoder;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyDecoder;
import org.rzo.netty.ahessian.rpc.message.OutputProducer;
import org.rzo.netty.ahessian.stopable.StopHandler;
import org.rzo.netty.ahessian.stopable.StopablePipeline;

public class AHessianClientPipelineFactory implements ChannelPipelineFactory
{

	Executor			_executor;
	HessianProxyFactory	_factory;
	BootstrapProvider	_bootstrapProvider;
	Timer _timer;

	AHessianClientPipelineFactory(Executor executor, HessianProxyFactory factory, BootstrapProvider bootstrapProvider, Timer timer)
	{
		_executor = executor;
		_factory = factory;
		_bootstrapProvider = bootstrapProvider;
		_timer = timer;
	}

	AHessianClientPipelineFactory(Executor executor, HessianProxyFactory factory, Timer timer)
	{
		this(executor, factory, null, timer);
	}

	public ChannelPipeline getPipeline() throws Exception
	{
		ChannelPipeline pipeline = StopablePipeline.pipeline(); // Note the
																// static
																// import.

		// no auto reconnect: port may change in case of mcast discovery
		if (_bootstrapProvider != null)
			pipeline.addLast("reconnect", new ReconnectHandler(_bootstrapProvider, 1000, _timer));
		// pipeline.addLast("logger1",new OutLogger("1"));

		// InputStreamDecoder returns an input stream and calls the next handler
		// in a separate thread
		pipeline.addLast("inputStream", new InputStreamDecoder());

		// pipeline.addLast("logger2",new OutLogger("2"));
		pipeline.addLast("outputStream", new OutputStreamEncoder());

		pipeline.addLast("hessianReplyDecoder", new PullInputStreamConsumer(new HessianRPCReplyDecoder(_factory, new JmxSerializerFactory()), _executor));
		pipeline.addLast("hessianCallEncoder", new HessianRPCCallEncoder(new JmxSerializerFactory()));
		pipeline.addLast("outputProducer", new OutputProducer(_executor));
		// pipeline.addLast("logger3",new OutLogger("3"));
		pipeline.addLast("hessianHandler", _factory);
		pipeline.addLast("stopHandler", new StopHandler());

		return pipeline;
	}

}
