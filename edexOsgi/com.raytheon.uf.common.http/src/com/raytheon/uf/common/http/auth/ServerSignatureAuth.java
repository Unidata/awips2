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
package com.raytheon.uf.common.http.auth;

import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;
import java.util.HashMap;
import java.util.Map;

/**
 * Implements signature authentication for HTTP servers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ServerSignatureAuth extends SignatureAuthScheme {

    private final Map<String, KeyFactory> factoryMap = new HashMap<String, KeyFactory>();

    /**
     * Get the KeyFactory associated with the provided algorithm, a new factory
     * is created and cached if not found
     * 
     * @param keyAlg
     * @return
     * @throws NoSuchAlgorithmException
     */
    public KeyFactory getFactory(String keyAlg) throws NoSuchAlgorithmException {
        KeyFactory rval;
        synchronized (factoryMap) {
            rval = factoryMap.get(keyAlg);
            if (rval == null) {
                rval = KeyFactory.getInstance(keyAlg);
                factoryMap.put(keyAlg, rval);
            }
        }
        return rval;
    }

    /**
     * Decode a base64 X509 encoded public key using the provided algorithm.
     * 
     * @param encodedKey
     * @param algorithm
     * @return
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeySpecException
     */
    public PublicKey decodePublicKey(String encodedKey, String algorithm)
            throws NoSuchAlgorithmException, InvalidKeySpecException {
        byte[] bytes = base64Decode(encodedKey);
        X509EncodedKeySpec pubKeySpec = new X509EncodedKeySpec(bytes);
        KeyFactory factory = getFactory(algorithm);
        return factory.generatePublic(pubKeySpec);
    }

    /**
     * @param sig
     * @param hostHeader
     *            host used in request including port
     * @param path
     *            path used in request
     * @return true if the signature matches the URI parts provided
     * @throws SignatureException
     */
    public boolean verify(Signature sig, String hostHeader, String path)
            throws SignatureException {
        return verify(sig, hostHeader, path, null);
    }

    /**
     * @param sig
     * @param hostHeader
     *            host used in request including port
     * @param path
     *            path used in request
     * @param bytes
     *            body of request
     * @return true if the signature matches the URI parts and body provided
     * @throws SignatureException
     */
    public boolean verify(Signature sig, String hostHeader, String path,
            byte[] bytes) throws SignatureException {
        synchronized (sig) {
            sig.update(hostHeader.getBytes());
            sig.update(path.getBytes());
            if (bytes != null) {
                sig.update(bytes);
            }
            return sig.verify(bytes);
        }
    }

}
