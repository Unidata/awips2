package org.rzo.netty.ahessian.session;

import org.jboss.netty.util.Timeout;

/**
 * A session object.
 * TODO for now session objects just hold the id. Handling of session timeout is missing
 */
public interface Session extends ServiceSession
{
	
	/**
	 * TODO Destroy the current session and associated pipeline.
	 */
	public void invalidate();
	
	public void setNew(boolean newValue);
	
	public void setTimeOut(Timeout timeOut);
	
	public Timeout removeTimeout();

	public void close();
	
	public void onMessage();

	public void setClosed(boolean b);

	

}
