package org.rzo.netty.ahessian.auth;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.handler.codec.base64.Base64;


public class Base64AuthToken extends SimpleAuthToken
{
	
	private String _user;
	
	public Base64AuthToken(String user, String password) throws Exception
	{
		_user = user;
		String data = user+":"+password;
		ChannelBuffer digest = Base64.encode(ChannelBuffers.wrappedBuffer(data.getBytes("UTF-8")));
		byte[] digestBytes = new byte[digest.readableBytes()];
		digest.readBytes(digestBytes);
		super.setPassword(new String(digestBytes));
	}
	
	public String getUser()
	{
		return _user;
	}

}
