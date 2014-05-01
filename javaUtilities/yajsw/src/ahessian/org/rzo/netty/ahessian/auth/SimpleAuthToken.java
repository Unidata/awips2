package org.rzo.netty.ahessian.auth;

import java.util.Arrays;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;

/**
 * A Simple Authentication Token.
 * The password is sent unencrypted.
 */
public class SimpleAuthToken implements AuthToken
{
	
	/** The _password. */
	byte[] _password;
	
	/** The _received bytes. */
	byte[] _receivedBytes;
	
	/** The _received length. */
	int _receivedLength = 0;
	
	boolean _loggedOn = false;
	
	int _length = -1;
	
	private static final InternalLogger logger =
        InternalLoggerFactory.getInstance(SimpleAuthToken.class);

	
	/**
	 * Sets the password.
	 * 
	 * @param password the new password
	 */
	public void setPassword(String password)
	{
		_password = ensureLength(password.getBytes());
		_receivedBytes = new byte[_password.length];
	}
	
	public void setLength(int length)
	{
		_length = length;
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.auth.AuthToken#authenticate(org.jboss.netty.channel.ChannelHandlerContext, org.jboss.netty.channel.MessageEvent)
	 */
	public int authenticate(ChannelHandlerContext ctx, MessageEvent e)
	{
			ChannelBuffer b = (ChannelBuffer) e.getMessage();
			int toCopy = Math.min(_receivedBytes.length-_receivedLength, b.readableBytes());
			byte[] bytes = new byte[toCopy];
			b.readBytes(bytes);
			System.arraycopy(bytes, 0, _receivedBytes, _receivedLength, bytes.length);
			_receivedLength += toCopy;
			if (_receivedLength == _password.length)
			{
				if (Arrays.equals(_receivedBytes, _password))
				{
					logger.info("authenticated");
					if (b.readableBytes() != 0)
						ctx.sendUpstream(e);
					return PASSED;
				}
				else
					return FAILED;
			}
			else
				return NOT_COMPLETE;
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.auth.AuthToken#sendPassword(org.jboss.netty.channel.ChannelHandlerContext)
	 */
	public void sendPassword(ChannelHandlerContext ctx)
	{
		Channels.write(ctx, Channels.future(ctx.getChannel()), ChannelBuffers.wrappedBuffer(_password));
	}

	public boolean isLoggedOn()
	{
		return _loggedOn;
	}

	void setLoggedOn(boolean loggedOn)
	{
		_loggedOn = loggedOn;
	}

	public void disconnected()
	{
		setLoggedOn(false);
	}
	
	byte[] ensureLength(byte[] bytes)
	{
		if (bytes.length == _length || _length <= 0)
			return bytes;
		else
		{
			return Arrays.copyOf(bytes, _length);
		}
	}
	
	byte[] getPassword()
	{
		return _password;
	}

	
}
