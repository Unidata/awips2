/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.security.encryption;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Class that supports encrypting/decrypting AES encrypted objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/16/2014    3350        bphillip    Initial coding
 * 2/26/2016    5388        dhladky     Cipher is not thread safe.
 *  * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class AESEncryptor {

    private static final String ALGORITHM = "AES";

    private static final String PADDING = "AES/CFB8/NoPadding";

    // entropy input vector length
    private static final int IV_LENGTH = 16;

    private IvParameterSpec ivSpec;

    private SecretKeySpec key;

    private Cipher deCipher;

    private Cipher enCipher;
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AESEncryptor.class);

    public static void main(String[] args) {
        String action = args[0];
        String key = args[1];
        String input = args[2];

        try {
            AESEncryptor enc = new AESEncryptor(key);
            if (action.equalsIgnoreCase("encrypt")) {
                System.out.println(enc.encrypt(input));
            } else if (action.equalsIgnoreCase("decrypt")) {
                System.out.println(enc.decrypt(input));
            } else {
                System.out.println("Unrecognized action");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    // Try to use this constructor
    public AESEncryptor(String sharedKey) {
        try {
            setupCipher(sharedKey);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "Unable to create Cipher.", e);
        }
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
        keyBytes = Arrays.copyOf(keyBytes, IV_LENGTH); // use only first 128 bit

        byte[] ivBytes = new byte[IV_LENGTH];
        ivSpec = new IvParameterSpec(ivBytes);

        /*
         * create the cipher with the algorithm you choose see javadoc for
         * Cipher class for more info, e.g.
         */
        key = new SecretKeySpec(keyBytes, ALGORITHM);
        deCipher = Cipher.getInstance(PADDING);
        enCipher = Cipher.getInstance(PADDING);
        
        enCipher.init(Cipher.ENCRYPT_MODE, key, ivSpec);
        deCipher.init(Cipher.DECRYPT_MODE, key, ivSpec);
    }

    /**
     * Encrypts a string using AES/DES encoding
     * 
     * @param password
     *            The string to encrypt
     * @return The encrypted string
     * @throws Exception
     *             on error
     */
    public synchronized String encrypt(String password)
            throws Exception {

        return Base64.encodeBase64String(enCipher.doFinal(password.getBytes()));
    }

    /**
     * Decrypts an AES/DES encoded string
     * 
     * @param encryptedPass
     *            The encrypted string to decrypt
     * @return The encrypted string
     * @throws Exception
     *             on error
     */
    public synchronized String decrypt(String encryptedPass)
            throws Exception {

        return new String(deCipher.doFinal(Base64.decodeBase64(encryptedPass)));
    }

}
