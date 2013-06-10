package org.rzo.netty.mcast.discovery;

public interface DiscoveryListener
{
	public void newHost(String name, String host);
}
