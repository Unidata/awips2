package org.rzo.netty.ahessian.crypto;

public class StreamCipherFactory
{
	
	public static StreamCipher createCipher(String algorithm)
	{
		if ("RC4".equals(algorithm))
			return new RC4Cipher();
		else if ("Salsa20".equals(algorithm))
			return new Salsa20();
		else
			return null;
	}

}
