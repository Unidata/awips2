package org.rzo.netty.ahessian.auth;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;

public class AuthTokenList implements AuthToken
{
	
	private Map<ByteArrayWrapper, AuthToken> _tokens;
	int _receivedLength = 0;
	byte [] _receivedBytes;
	AuthToken _currentToken;
	boolean _uniqueLogon;
	private static final InternalLogger logger =
        InternalLoggerFactory.getInstance(SimpleAuthToken.class);
	
	public AuthTokenList(Map<ByteArrayWrapper, AuthToken> tokens, int bytesLength, boolean uniqueLogon)
	{
		_tokens = tokens;
		_receivedBytes = new byte[bytesLength];
		_uniqueLogon = uniqueLogon;
	}
	
	public static AuthTokenList fromList(List<AuthToken> tokens, boolean uniqueLogon)
	{
		Map<ByteArrayWrapper, AuthToken> tks = new HashMap<ByteArrayWrapper, AuthToken>();
		int bytesLength = 0;
		for (AuthToken token : tokens)
		{
			byte[] pwd = ((SimpleAuthToken)token).getPassword();
			tks.put(new ByteArrayWrapper(pwd), token);
			if (bytesLength < pwd.length)
				bytesLength = pwd.length;
		}
		return new AuthTokenList(tks, bytesLength, uniqueLogon);
	}

	public static AuthTokenList fromList(List<AuthToken> tokens)
	{
		return fromList(tokens, false);
	}

	
	public int authenticate(ChannelHandlerContext ctx, MessageEvent e)
	{
				ChannelBuffer b = (ChannelBuffer) e.getMessage();
				int toCopy = Math.min(_receivedBytes.length-_receivedLength, b.readableBytes());
				byte[] bytes = new byte[toCopy];
				b.readBytes(bytes);
				System.arraycopy(bytes, 0, _receivedBytes, _receivedLength, bytes.length);
				_receivedLength += toCopy;
				if (_receivedLength == _receivedBytes.length)
				{
					_currentToken = _tokens.get(new ByteArrayWrapper(_receivedBytes));
					if (_currentToken != null && (_uniqueLogon || _currentToken.isLoggedOn()))
					{
						logger.info("authenticated");
						((SimpleAuthToken)_currentToken).setLoggedOn(true);
						if (b.readableBytes() != 0)
							ctx.sendUpstream(e);
						return PASSED;
					}
					else
					{
						_currentToken = null;
						return FAILED;
					}
				}
				else
					return NOT_COMPLETE;
		
	}
	
	public AuthToken authenticate(String password) throws Exception
	{
		ByteArrayWrapper input = new ByteArrayWrapper(password.getBytes("UTF-8"));
		AuthToken result = _tokens.get(input);
		if (result == null)
			return null;
		if (_uniqueLogon && result.isLoggedOn())
				return null;
		((SimpleAuthToken)result).setLoggedOn(true);
		return result;
	}

	public void sendPassword(ChannelHandlerContext ctx)
	{
		;
	}

	public boolean isLoggedOn()
	{
		return _currentToken != null;
	}

	public void setLoggedOn(boolean value)
	{
		if (!value && _currentToken != null)
		{
			((SimpleAuthToken)_currentToken).setLoggedOn(false);
			_currentToken = null;
		}
	}

	public void disconnected()
	{
		setLoggedOn(false);
	}
	
	

}
