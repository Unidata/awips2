package org.rzo.netty.ahessian.rpc.server;

import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;

/**
 * A service wraps an object, so that it can be used as a service on the hessian RPC server. <br>
 * Currently 2 types of services are implemented:
 * 
 * <ul>
 * <li>{@link ImmediateInvokeService}</li>
 * <li>{@link ContinuationService}</li>
 * </ul>
 * 
 */
public interface Service
{

	/**
	 * Handle a hessian RPC call request
	 * 
	 * @param message
	 *            the call request message
	 */
	public void messageReceived(HessianRPCCallMessage message);
}
