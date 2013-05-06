package org.rzo.netty.ahessian.auth;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * An  Encrypted Authentication Token.
 * The password is transmitted encrypted.
 */
public class EncryptedAuthToken extends SimpleAuthToken
{
	
	/** The _algorithm. */
	MessageDigest _algorithm = null;
	
	/**
	 * Sets the algorithm.
	 * 
	 * @param algorithm the encryption algorithm. 
	 * @see java.security.MessageDigest
	 * 
	 * @throws NoSuchAlgorithmException 
	 */
	public void setAlgorithm(String algorithm) throws NoSuchAlgorithmException
	{
		_algorithm = MessageDigest.getInstance(algorithm);

	}
	
	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.auth.SimpleAuthToken#setPassword(java.lang.String)
	 */
	public void setPassword(String password)
	{
		_algorithm.reset();
		_algorithm.update(password.getBytes());
		_password = ensureLength(_algorithm.digest());
		_receivedBytes = new byte[_password.length];

	}

}
