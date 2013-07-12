package com.raytheon.uf.common.datadelivery.registry;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.codec.binary.Base64;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Encryption
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2013  2180      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement(name = "encryption")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Encryption {

    // entropy
    private static final int IV_LENGTH = 16;

    private IvParameterSpec ivSpec;

    private SecretKeySpec key;

    private Cipher deCipher;

    private Cipher enCipher;

    @XmlElement(name = "algorithim")
    @DynamicSerializeElement
    public Algorithim algorithim;

    @XmlElement(name = "padding")
    @DynamicSerializeElement
    public Padding padding;

    public Encryption() {

    }

    @XmlEnum
    public enum Algorithim {
        // CLEAR, No encryption
        // AES, AES encryption
        // DES, DES encryption
        @XmlEnumValue(Algorithim.aes)
        AES("AES"), @XmlEnumValue(Algorithim.des)
        DES("DES");

        private static final String aes = "AES";

        private static final String des = "DES";

        private final String algo;

        private Algorithim(String name) {
            algo = name;
        }

        @Override
        public String toString() {
            return algo;
        }
    }

    @XmlEnum
    public enum Padding {
        // CLEAR, No encryption
        // AES, AES encryption
        // DES, DES encryption
        @XmlEnumValue(Padding.aes_pad)
        AES("AES/CFB8/NoPadding"), @XmlEnumValue(Padding.des_pad)
        DES("DES/CBC/PKCS5Padding");

        private static final String aes_pad = "AES/CFB8/NoPadding";

        private static final String des_pad = "DES/CBC/PKCS5Padding";

        private final String padd;

        private Padding(String name) {
            padd = name;
        }

        @Override
        public String toString() {
            return padd;
        }
    }

    public Algorithim getAlgorithim() {
        return algorithim;
    }

    public void setAlgorithim(Algorithim algorithim) {
        this.algorithim = algorithim;
    }

    public Padding getPadding() {
        return padding;
    }

    public void setPadding(Padding padding) {
        this.padding = padding;
    }

    /**
     * Sets up the cipher using the sharedKey
     * 
     * @param sharedKey
     * @throws NoSuchAlgorithmException
     */
    private void setupCipher(String sharedKey) throws Exception {

        byte[] keyBytes = null;
        MessageDigest sha = MessageDigest.getInstance("SHA-1");
        keyBytes = sha.digest(Base64.decodeBase64(sharedKey));
        keyBytes = Arrays.copyOf(keyBytes, 16); // use only first 128 bit

        byte[] ivBytes = new byte[IV_LENGTH];
        ivSpec = new IvParameterSpec(ivBytes);
        // create the cipher with the algorithm you choose
        // see javadoc for Cipher class for more info, e.g.

        key = new SecretKeySpec(keyBytes, getAlgorithim().algo);
        deCipher = Cipher.getInstance(getPadding().padd);
        enCipher = Cipher.getInstance(getPadding().padd);
    }

    /**
     * Encrypts a string using AES/DES encoding
     * 
     * @param sharedKey
     * @param password
     * @return
     * @throws Exception
     */
    public String encrypt(String sharedKey, String password)
            throws Exception {

        setupCipher(sharedKey);
        enCipher.init(Cipher.ENCRYPT_MODE, key, ivSpec);

        return Base64.encodeBase64String(enCipher.doFinal(password.getBytes()));
    }

    /**
     * Decrypts an AES/DES encoded string
     * 
     * @param sharedKey
     * @param encryptedPass
     * @return
     * @throws Exception
     */
    public String decrypt(String sharedKey, String encryptedPass)
            throws Exception {

        setupCipher(sharedKey);
        deCipher.init(Cipher.DECRYPT_MODE, key, ivSpec);

        return new String(deCipher.doFinal(Base64.decodeBase64(encryptedPass)));
    }

}
