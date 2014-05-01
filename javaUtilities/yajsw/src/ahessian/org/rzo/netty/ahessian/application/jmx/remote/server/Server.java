package org.rzo.netty.ahessian.application.jmx.remote.server;

import java.net.InetSocketAddress;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;

public class Server
{
    public static void main(String[] args)
    {
        Executor executor = Executors.newFixedThreadPool(200);

        // Configure the server.
        ServerBootstrap bootstrap = new ServerBootstrap(
                new NioServerSocketChannelFactory(
                		executor,
                		executor));

        bootstrap.setPipelineFactory(
               new RPCServerSessionPipelineFactory( new RPCServerMixinPipelineFactory(executor)));

        // Bind and start to accept incoming connections.
        bootstrap.bind(new InetSocketAddress(8080));

    }

}
