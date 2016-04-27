package com.raytheon.uf.common.security.certificate;
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

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyFactory;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Collection;

/**
*
* <p>This class imports a key and a certificate into a keystore
* (<code>$home/keystore.ImportKey</code>). If the keystore is
* already present, it is simply deleted. Both the key and the
* certificate file must be in <code>DER</code>-format. The key must be
* encoded with <code>PKCS#8</code>-format. The certificate must be
* encoded in <code>X.509</code>-format.</p>
* 
* This file is adapted from the original ImportKey by Joachim Karrer, Jens Carlberg
* 
* SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/16/2016     5486        dhladky     initial creation
 *  * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
public class CertLoader  {

    private static final String JKSKeystore = "JKS";

    private static final String SUNKeystore = "SUN";

    private static final String KeyAlg = "RSA";

    private static final String certificateType = "X.509";
    
    private static final String keyStoreName = "keystore.jks";

    /**
     * Open up streams for the certificate and key files
     * 
     * @param fname
     * @return
     * @throws IOException
     */
    private static InputStream fullStream(String fname) throws IOException {
        FileInputStream fis = new FileInputStream(fname);
        DataInputStream dis = new DataInputStream(fis);
        byte[] bytes = new byte[dis.available()];
        dis.readFully(bytes);
        dis.close();
        ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
        return bais;
    }

    /**
     * Takes 5 input parameters
     * 
     * @param keyFile
     * @param certFile
     * @param keystorename
     * @param keyPass
     * @param alias
     */
    public static void main(String args[]) {

        String keypass = null;
        String alias = null;
        String keystorename = null;
        String keyfile = null;
        String certfile = null;

        // parsing command line input
        if (args.length < 5 || args.length > 5) {
            System.out
                    .println("Usage: java com.raytheon.uf.edex.common.security.certificate.KeyLoader [keyfile (DER format)] [certfile (DER format)] [keystorePath] [keypass] [alias]");
            System.exit(0);
        } else {
            keyfile = args[0];
            certfile = args[1];
            keystorename = args[2]+"/"+keyStoreName;
            keypass = args[3];
            alias = args[4];
        }

        try {
            // initializing and clearing keystore
            KeyStore ks = KeyStore.getInstance(JKSKeystore, SUNKeystore);
            ks.load(null, keypass.toCharArray());
            System.out.println("Using keystore-file : " + keystorename);
            ks.store(new FileOutputStream(keystorename), keypass.toCharArray());
            ks.load(new FileInputStream(keystorename), keypass.toCharArray());

            // loading Key
            InputStream fl = fullStream(keyfile);
            byte[] key = new byte[fl.available()];
            KeyFactory kf = KeyFactory.getInstance(KeyAlg);
            fl.read(key, 0, fl.available());
            fl.close();
            PKCS8EncodedKeySpec keysp = new PKCS8EncodedKeySpec(key);
            PrivateKey ff = kf.generatePrivate(keysp);

            // loading CertificateChain
            CertificateFactory cf = CertificateFactory
                    .getInstance(certificateType);
            InputStream certstream = fullStream(certfile);

            Collection<?> c = cf.generateCertificates(certstream);
            Certificate[] certs = new Certificate[c.toArray().length];

            if (c.size() == 1) {
                certstream = fullStream(certfile);
                System.out.println("One certificate, no chain.");
                Certificate cert = cf.generateCertificate(certstream);
                certs[0] = cert;
            } else {
                System.out.println("Certificate chain length: " + c.size());
                certs = (Certificate[]) c.toArray();
            }

            // storing keystore
            ks.setKeyEntry(alias, ff, keypass.toCharArray(), certs);
            System.out.println("Key and Certificate stored.");
            System.out.println("Alias:" + alias);
            ks.store(new FileOutputStream(keystorename), keypass.toCharArray());
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

}