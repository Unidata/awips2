package org.rzo.netty.mcast.discovery;

import static org.jboss.netty.channel.Channels.pipeline;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;
import java.util.Iterator;

import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
import org.jboss.netty.handler.ipfilter.IpFilterRule;
import org.jboss.netty.handler.ipfilter.IpFilterRuleList;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.mcast.MulticastEndpoint;

public class DiscoveryServer extends MulticastEndpoint
{
	private String	name;
	private String	host;
	private int		port;
	private IpFilterRuleList   firewall;

	public void init() throws Exception
	{
		if (host == null)
			host = whatIsMyIp();

		ChannelPipelineFactory factory = new ChannelPipelineFactory()
		{
			public ChannelPipeline getPipeline() throws Exception
			{
				ChannelPipeline pipeline = pipeline();
				pipeline.addLast("discoveryServer", new SimpleChannelUpstreamHandler()
				{

					@Override
					public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception
					{
						String request = getStringMessage(e);
						if (request == null)
							return;
						if (name != null && name.equals(request) && host != null && port > 0)
						{
							if (validate(e))
								send(ChannelBuffers.wrappedBuffer((name + "&" + host + "&" + port).getBytes()));
						}
					}
				});
				return pipeline;
			}

		};
		super.init(factory);
	}

	public String getName()
	{
		return name;
	}

	public String getHost()
	{
		return host;
	}

	public int getPort()
	{
		return port;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public void setHost(String host)
	{
		this.host = host;
	}

	public void setPort(int port)
	{
		this.port = port;
	}
	
	public void setIpSet(IpFilterRuleList ipSet)
	{
		this.firewall = ipSet;
	}
	
	private String whatIsMyIp()
	{
		String result = null;
        try
        {
          Enumeration<NetworkInterface> e = NetworkInterface.getNetworkInterfaces();

          while (e.hasMoreElements())
          {
            NetworkInterface ne = (NetworkInterface) e.nextElement();
            Enumeration<InetAddress> e2 = ne.getInetAddresses();

            while (e2.hasMoreElements())
            {
              InetAddress ia = (InetAddress) e2.nextElement();

              if (!ia.isAnyLocalAddress() && !ia.isLinkLocalAddress()
                  && !ia.isLoopbackAddress() && !ia.isMulticastAddress())
              if (result == null || !ia.isSiteLocalAddress())
              {
                result = ia.getHostAddress();
              }
            }
          }
        } catch (Exception ex)
        {
			Constants.ahessianLogger.warn("", ex);
        }
        return result;

	}
	
	private boolean validate(MessageEvent e)
	{
		if (firewall == null)
			return true;
		else
		{
			InetAddress inetAddress = ((InetSocketAddress)e.getRemoteAddress()).getAddress();
		      Iterator<IpFilterRule> iterator = firewall.iterator();
		      IpFilterRule ipFilterRule = null;
		      while (iterator.hasNext())
		      {
		         ipFilterRule = iterator.next();
		         if (ipFilterRule.contains(inetAddress))
		         {
		            // Match founds, is it a ALLOW or DENY rule
		            return ipFilterRule.isAllowRule();
		         }
		      }
		      // No limitation founds and no allow either, but as it is like Firewall rules, it is therefore accepted
		      return true;
		}
	}


}
