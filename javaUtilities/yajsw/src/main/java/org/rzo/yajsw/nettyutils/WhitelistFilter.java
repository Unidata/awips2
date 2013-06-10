/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.nettyutils;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

/**
 * The Class WhitelistFilter.
 */
@ChannelPipelineCoverage("all")
public class WhitelistFilter extends SimpleChannelUpstreamHandler
{

	/** The whitelist. */
	private final List<InetAddress>	whitelist	= new CopyOnWriteArrayList<InetAddress>();

	/**
	 * Sets the addresses to be whitelisted.
	 * 
	 * NOTE: this call will remove any previously blacklisted addresses.
	 * 
	 * @param addresses
	 *            an array of addresses to be blacklisted.
	 */
	public void allowAll(InetAddress[] addresses)
	{
		if (addresses == null)
		{
			throw new NullPointerException("addresses");
		}
		for (int i = 0; i < addresses.length; i++)
		{
			InetAddress addr = addresses[i];
			allow(addr);
		}
	}

	/**
	 * Sets the addresses to be blacklisted.
	 * 
	 * NOTE: this call will remove any previously blacklisted addresses.
	 * 
	 * @param addresses
	 *            a collection of InetAddress objects representing the addresses
	 *            to be blacklisted.
	 * 
	 * @throws IllegalArgumentException
	 *             if the specified collections contains non-{@link InetAddress}
	 *             objects.
	 */
	public void allowAll(Iterable<InetAddress> addresses)
	{
		if (addresses == null)
		{
			throw new NullPointerException("addresses");
		}

		for (InetAddress address : addresses)
		{
			allow(address);
		}
	}

	/**
	 * Blocks the specified endpoint.
	 * 
	 * @param address
	 *            the address
	 */
	public void allow(InetAddress address)
	{
		whitelist.add(address);
	}

	/**
	 * Unblocks the specified endpoint.
	 * 
	 * @param address
	 *            the address
	 */
	public void remove(InetAddress address)
	{
		if (address == null)
		{
			throw new NullPointerException("address");
		}
		whitelist.remove(address);
	}

	@Override
	public void channelOpen(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception
	{
		if (!isBlocked(ctx.getChannel()))
		{
			// forward if not blocked
			ctx.sendUpstream(e);
		}
		else
		{
			System.out.println("connection refused : " + ctx.getChannel().getRemoteAddress());
			blockSession(ctx.getChannel());
		}
	}

	/**
	 * Block session.
	 * 
	 * @param session
	 *            the session
	 */
	private void blockSession(Channel session)
	{
		SocketAddress remoteAddress = session.getRemoteAddress();
		// logger.warn("Remote address " + remoteAddress +
		// " not in the whitelist; closing.");
		session.close();
	}

	/**
	 * Checks if is blocked.
	 * 
	 * @param session
	 *            the session
	 * 
	 * @return true, if is blocked
	 */
	private boolean isBlocked(Channel session)
	{
		SocketAddress remoteAddress = session.getRemoteAddress();
		if (remoteAddress instanceof InetSocketAddress)
		{
			if (whitelist.contains(((InetSocketAddress) remoteAddress).getAddress()))
			{
				return false;
			}
		}

		return true;
	}
}
