package org.rzo.netty.ahessian.crypto;

public interface StreamCipher
{
    public void engineInitEncrypt(byte[] key, byte[] iv) throws CryptoException;

    public void engineInitDecrypt(byte[] key, byte[] iv) throws CryptoException;
    
    public void crypt(byte[] in, int inOffset, int length,  byte[] out, int outOffset)  throws CryptoException;
    
    public byte[] crypt(byte[] data, int position, int length)   throws CryptoException;
   


}
