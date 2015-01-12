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

import java.net.URI;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.SignatureException;

import org.apache.http.HttpHost;
import org.apache.http.client.utils.URIUtils;

/**
 * Implements signature authentication for HTTP clients
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
public class ClientSignatureAuth extends SignatureAuthScheme {

    private final KeyPair keypair;

    private final Signature sig;

    /**
     * Default constructor, creates a new keypair
     * 
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     */
    public ClientSignatureAuth() throws NoSuchAlgorithmException,
            InvalidKeyException {
        this(generateKeys());
    }

    /**
     * @param keypair
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     */
    public ClientSignatureAuth(KeyPair keypair)
            throws NoSuchAlgorithmException, InvalidKeyException {
        this.keypair = keypair;
        this.sig = Signature.getInstance(SIG_ALG);
        this.sig.initSign(this.keypair.getPrivate());
    }

    /**
     * Create a base64 encoded signature from URI
     * 
     * @param uri
     * @return
     * @throws SignatureException
     */
    public String sign(URI uri) throws SignatureException {
        return sign(uri, null);
    }

    /**
     * Create a base64 encoded signature from URI and body
     * 
     * @param uri
     * @param bytes
     * @return
     * @throws SignatureException
     */
    public String sign(URI uri, byte[] bytes) throws SignatureException {
        HttpHost host = URIUtils.extractHost(uri);
        synchronized (sig) {
            sig.update(host.toHostString().getBytes());
            sig.update(uri.getPath().getBytes());
            if (bytes != null) {
                sig.update(bytes);
            }
            return base64Encode(sig.sign());
        }
    }

    /**
     * Generate a new public/private key pair using the default algorithms
     * 
     * @return
     * @throws NoSuchAlgorithmException
     */
    public static final KeyPair generateKeys() throws NoSuchAlgorithmException {
        KeyPairGenerator keyGen = KeyPairGenerator.getInstance(KEY_ALG);
        SecureRandom random = SecureRandom.getInstance(RANDOM_ALG);
        keyGen.initialize(KEY_SIZE, random);
        return keyGen.generateKeyPair();
    }

    /**
     * @return the keypair
     */
    public KeyPair getKeypair() {
        return keypair;
    }

    /**
     * Get the default public key algorithm
     * 
     * @return
     */
    public String getKeyAlgorithm() {
        return KEY_ALG;
    }

    /**
     * Get the base64 X509 encoded public key
     * 
     * @return
     */
    public String getEncodedPublicKey() {
        PublicKey pub = keypair.getPublic();
        return base64Encode(pub.getEncoded());
    }
}
