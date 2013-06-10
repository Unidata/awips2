package org.rzo.netty.ahessian.rpc.server;

import org.rzo.netty.ahessian.session.ServiceSession;
import org.rzo.netty.ahessian.session.Session;

public class ServiceSessionProvider
{
	private static ThreadLocal<ServiceSession> threadLocalSession = new ThreadLocal<ServiceSession>();

	public static ServiceSession getSession()
	{
		return threadLocalSession.get();
	}
	
	protected static void set(ServiceSession session)
	{
		threadLocalSession.set(session);
	}
	protected static void remove()
	{
		if (threadLocalSession.get() == null)
			return;
		if (threadLocalSession.get().isNew())
			((Session)threadLocalSession.get()).setNew(false);
		threadLocalSession.remove();
	}

}
