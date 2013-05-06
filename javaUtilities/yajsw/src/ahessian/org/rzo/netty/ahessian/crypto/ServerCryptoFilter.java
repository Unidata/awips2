package org.rzo.netty.ahessian.crypto;

import java.security.Key;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.crypto.Cipher;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.rzo.netty.ahessian.log.OutLogger;

public class ServerCryptoFilter extends SimpleChannelHandler implements CryptoConstants
{
	KeyPair _serverKeyPair;
	Key		_clientKey;
	ChannelStateEvent _connectedEvent;
	private StreamCipher _encodeCipher;
	private StreamCipher _decodeCipher;
	private byte[] _cryptedIvKeyMessage;
	private int _bytesRead;
	private List<byte[]> _passwords = new ArrayList<byte[]>();


	@Override
    public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception {
		// send public key
		sendByteArray(ctx, getPublicKeyEncoded());
		// remember this event, so that we can propagate it to the rest of the pipeline once we have 
		// the client's secret key
		_connectedEvent = e;
    }

	private void sendByteArray(ChannelHandlerContext ctx, byte[] buffer)
	{
		try
		{
        Channel channel = ctx.getChannel();
		ChannelFuture future = Channels.future(ctx.getChannel());
		ChannelBuffer b = ChannelBuffers.dynamicBuffer();
		// first send encoded key bytes size
		b.writeInt(buffer.length);
		// then the public key
		b.writeBytes(buffer);
		Channels.write(ctx, future, b);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	byte[] getPublicKeyEncoded()
	{
		try
		{
		// generate a key pair
		SecureRandom     random = new SecureRandom();
		KeyPairGenerator generator = KeyPairGenerator.getInstance(ASYM_KEY_TYPE);
		generator.initialize(ASYM_KEY_SIZE, random);

        _serverKeyPair = generator.generateKeyPair();
        Key pubKey = _serverKeyPair.getPublic();
        return pubKey.getEncoded();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return null;
		
	}
	
	@Override
	public void messageReceived(
            ChannelHandlerContext ctx, MessageEvent e) throws Exception
            {
				// have we sent our secret key ?
				if (_decodeCipher != null)
				{
					MessageEvent m = Util.code(_decodeCipher, e, true);
					ctx.sendUpstream(m);
				}
				else
				{
					ChannelBuffer b = (ChannelBuffer) e.getMessage();
					// is this our first message ?
					if (_cryptedIvKeyMessage == null)
					{
						int size = b.readInt();
						// consistency check, so we do not get an out of memory exception
						if (size > 1024)
						{
							ctx.getChannel().close();
							return;
						}
						_cryptedIvKeyMessage = new byte[size];
					}
					// readin the client's secret key and iv
					int available = b.readableBytes();
					int toRead = Math.min(_cryptedIvKeyMessage.length - _bytesRead, available);
					b.readBytes(_cryptedIvKeyMessage, _bytesRead, toRead);
					_bytesRead += toRead;
					// we have completed receiption  ?
					if (_bytesRead == _cryptedIvKeyMessage.length)
					{
						boolean ok = false;
						try
						{
						createCiphers();
						ok = true;
						}
						catch (Exception ex)
						{
							ex.printStackTrace();
							ctx.getChannel().close();
						}
						// inform pipline that we are ready for encrypted communication
						if (ok)
							ctx.sendUpstream(_connectedEvent);
					}
				}
            }
	
	private void createCiphers() throws Exception
	{
		// first decode the received data
		String type = "".equals(ASYM_CIPHER_TYPE) ? ASYM_KEY_TYPE : ASYM_KEY_TYPE+"/"+ASYM_CIPHER_TYPE;
        Cipher asymCipher = Cipher.getInstance(type);
        asymCipher.init(Cipher.DECRYPT_MODE, _serverKeyPair.getPrivate());
        byte[] data = asymCipher.doFinal(_cryptedIvKeyMessage);
        
        System.out.println("received iv+key: "+OutLogger.asString(data));

		byte[] iv = new byte[SYM_IV_SIZE];
		System.arraycopy(data, data.length-(SYM_IV_SIZE+SYM_KEY_SIZE+PASSWORD_SIZE+PASSWORD_SIZE), iv, 0, iv.length);
        System.out.println("received iv: "+OutLogger.asString(iv));
		
		byte[] key = new byte[SYM_KEY_SIZE];
		System.arraycopy(data, data.length-(SYM_KEY_SIZE+PASSWORD_SIZE), key, 0, key.length);
        System.out.println("received key: "+OutLogger.asString(key));
        
        byte[] password = new byte[PASSWORD_SIZE];
		System.arraycopy(data, data.length-PASSWORD_SIZE, password, 0, password.length);
        if (!checkPassword(password))
        	throw new RuntimeException("password mismatch");
        
		_encodeCipher = StreamCipherFactory.createCipher(SYM_KEY_TYPE);
		_encodeCipher.engineInitEncrypt(key, iv);
		
		_decodeCipher = StreamCipherFactory.createCipher(SYM_KEY_TYPE);
		_decodeCipher.engineInitDecrypt(key, iv);
	}

	private boolean checkPassword(byte[] password)
	{
		if (password == null || password.length != PASSWORD_SIZE)
			return false;
		else for (byte[] pwd : _passwords)
			if (Arrays.equals(password, pwd))
				return true;
		return false;
	}
	
	public void addPassword(byte[] password)
	{
		if (password == null || password.length == 0 || PASSWORD_SIZE == 0)
			return;
		byte[]mPassword = new byte[PASSWORD_SIZE];
		Arrays.fill(mPassword, (byte)0);
		int length = Math.min(PASSWORD_SIZE, password.length);
		System.arraycopy(password, 0, mPassword, 0, length);
		_passwords.add(mPassword);
	}


	@Override
	public void writeRequested(
            ChannelHandlerContext ctx, MessageEvent e) throws Exception
            {
		if (_encodeCipher != null)
		{
			MessageEvent m = Util.code(_encodeCipher, e, false);
			ctx.sendDownstream(m);
		}
		
            }


	
	public static void main(String[] args)
	{
		ServerCryptoFilter h = new ServerCryptoFilter();
		h.getPublicKeyEncoded();
	}

}
