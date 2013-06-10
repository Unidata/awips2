package org.rzo.netty.ahessian.auth;

import java.security.NoSuchAlgorithmException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class AuthTokenListFactory
{
	private Map<ByteArrayWrapper, AuthToken> _tokens = new HashMap<ByteArrayWrapper, AuthToken>();
	private int _length;
	private boolean _unique = true;
	
	public void addList(Collection<String> list, int length)
	{
		_length = length;
		for (String pwd : list)
		{
			SimpleAuthToken token = new SimpleAuthToken();
			token.setLength(length);
			token.setPassword(pwd);
			_tokens.put(new ByteArrayWrapper(token._password), token);
		}
	}
	
	public void addList(Collection<String> list, int length, String encryptionAlgorithm)
	{
		_length = length;
		for (String pwd : list)
		{
			EncryptedAuthToken token = new EncryptedAuthToken();
			token.setLength(length);
			try
			{
				token.setAlgorithm(encryptionAlgorithm);
			}
			catch (NoSuchAlgorithmException e)
			{
				e.printStackTrace();
			}
			token.setPassword(pwd);
			_tokens.put(new ByteArrayWrapper(token._password), token);
		}
	}
	
	public void setUnique(boolean value)
	{
		_unique = value;
	}
	
	public AuthTokenList getAuthTokenList()
	{
		return new AuthTokenList(_tokens, _length, _unique);
	}


	
}
