package org.rzo.netty.ahessian.session;

import java.util.Collection;

public interface ServiceSession
{	
	public String getId();
	public void addClosedListener(Runnable listener);
	public void addInvalidatedListener(Runnable listener);
	
	public Object getAttribute(String name);
	public Collection<String> getAttributeNames();
	public void removeAttribute(String name);
	public void setAttribute(String name, Object value);
	
	public long getCreationTime();
	public long getLastConnectedTime();
	
	public boolean isValid();
	public boolean isClosed();
	public boolean isNew();
	
	public long getMessageCount();


}
