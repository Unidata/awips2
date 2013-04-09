package org.rzo.netty.ahessian.rpc.client;

import org.jboss.netty.bootstrap.ClientBootstrap;

public interface BootstrapProvider
{
	public ClientBootstrap getBootstrap();
}
