package org.rzo.netty.ahessian.log;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.DownstreamMessageEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.UpstreamMessageEvent;
import org.jboss.netty.handler.logging.LoggingHandler;
import org.rzo.netty.ahessian.Constants;

public class OutLogger extends LoggingHandler
{
	String _name;
	public OutLogger(String name)
	{
		super(name);
		_name = name;
	}
    @Override
	public void log(ChannelEvent e)
	{
    	StringBuilder sb = new StringBuilder();

		//System.out.println(e);
		if (e instanceof DownstreamMessageEvent)
		{
			DownstreamMessageEvent devm = (DownstreamMessageEvent) e;
			Object mes = devm.getMessage();
			sb.append('[');
			sb.append(e.getChannel().getId());
			sb.append(" >out> ");
			if (mes instanceof ChannelBuffer)
				encodeBuffer((ChannelBuffer)((DownstreamMessageEvent)e).getMessage(), sb);
			else
				sb.append(mes.toString());
		} else
		if (e instanceof UpstreamMessageEvent)
		{
			sb.append(e.getChannel().getId());
			sb.append(" <in< ");
			Object message = ((UpstreamMessageEvent)e).getMessage();
			if (message instanceof ChannelBuffer)
			encodeBuffer((ChannelBuffer)((UpstreamMessageEvent)e).getMessage(), sb);
		}
		else if (e instanceof ExceptionEvent)
		{
			sb.append(e.toString());
		}
		else
			sb.append(e.toString());
		
		Constants.ahessianLogger.info(sb.toString());
	}
    
    static private void encodeBuffer(ChannelBuffer buffer, StringBuilder sb)
    {
    	if (buffer == null)
    		return;
    	sb.append("("+buffer.readableBytes()+") ");
    	int size = Math.min(50, buffer.readableBytes());
    	byte[] b = new byte[size];
    	buffer.getBytes(0, b);
    	for (int i=0; i<b.length && i < 50; i++)
    	{
    		toDebugChar(sb, b[i]);
    	}
    }
    
    static private void toDebugChar(StringBuilder sb, int ch)
    {    
      if (ch >= 0x20 && ch < 0x7f) {
        sb.append((char) ch);
      }
      else
        sb.append(String.format("\\x%02x", ch & 0xff));    
    }
    
    public static String asString(byte[] buffer)
    {
    	StringBuilder sb = new StringBuilder();
    	if (buffer == null)
    		return "null";
    	sb.append("("+buffer.length+") ");
    	for (int i=0; i<buffer.length; i++)
    	{
    		toDebugChar(sb, buffer[i]);
    	}
    	return sb.toString();
    	
    }


}
