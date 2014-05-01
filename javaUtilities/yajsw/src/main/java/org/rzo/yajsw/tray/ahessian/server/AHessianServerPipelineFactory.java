package org.rzo.yajsw.tray.ahessian.server;

import java.util.concurrent.Executor;
import java.util.logging.Logger;

import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.handler.ipfilter.IpFilteringHandler;
import org.rzo.netty.ahessian.application.jmx.remote.service.JmxSerializerFactory;
import org.rzo.netty.ahessian.io.InputStreamDecoder;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.io.PullInputStreamConsumer;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallDecoder;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyEncoder;
import org.rzo.netty.ahessian.rpc.message.OutputProducer;
import org.rzo.netty.ahessian.rpc.server.ExecutorInvokeService;
import org.rzo.netty.ahessian.rpc.server.HessianRPCServiceHandler;
import org.rzo.netty.ahessian.stopable.StopHandler;
import org.rzo.netty.ahessian.stopable.StopablePipeline;

public class AHessianServerPipelineFactory implements ChannelPipelineFactory
{

	Executor					_executor;
	IpFilteringHandler			_ipFilter;
	MBeanServer					_mbeanServer;
	HessianRPCServiceHandler	_factory;
	Logger _log;

	AHessianServerPipelineFactory(Executor executor, IpFilteringHandler ipFilter, MBeanServer mbeanServer, Logger log)
	{
		AhessianLogging.setAhessianLogger(log);
		_executor = executor;
		_ipFilter = ipFilter;
		_mbeanServer = mbeanServer;
		_log = log;
		_factory = new HessianRPCServiceHandler(_executor);

		// factory.addService("default", new ContinuationService(new
		// ContinuationHalloWorldService(), HelloWorldServiceInterface.class,
		// factory, _executor));
		_factory.addService("default", new ExecutorInvokeService(_mbeanServer, MBeanServerConnection.class, _factory, _executor));

	}

	public ChannelPipeline getPipeline() throws Exception
	{
		ChannelPipeline pipeline = StopablePipeline.pipeline();
		// pipeline.addLast("logger1",new OutLogger("1"));
		pipeline.addLast("ipfilter", _ipFilter);
		pipeline.addLast("inputStream", new InputStreamDecoder());
		// pipeline.addLast("logger2",new OutLogger("2"));
		pipeline.addLast("outputStream", new OutputStreamEncoder());
		pipeline.addLast("callDecoder", new PullInputStreamConsumer(new HessianRPCCallDecoder(new JmxSerializerFactory()), _executor));
		pipeline.addLast("replyEncoder", new HessianRPCReplyEncoder(new JmxSerializerFactory()));
		// pipeline.addLast("logger3",new OutLogger("3"));
		pipeline.addLast("outputProducer", new OutputProducer(_executor));
		pipeline.addLast("hessianRPCServer", _factory);
		pipeline.addLast("stopHandler", new StopHandler());

		return pipeline;
	}

}
