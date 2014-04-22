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

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.codec.binary.Base64;

/**
 * Base class for signature authentication scheme
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
public abstract class SignatureAuthScheme {

    protected static final String RANDOM_ALG = "SHA1PRNG";

    protected static final int KEY_SIZE = 256;

    protected static final String KEY_ALG = "EC";

    protected static final String SIG_ALG = "SHA256WITHECDSA";

    public static final String HTTP_AUTH_HEADER = "Authorization";

    public static final String AUTH_PARAMETER_DELIM = ",";

    public static final String CRED_FIELD_NAME = "Credential";

    public static final String SIG_FIELD_NAME = "Signature";

    /**
     * matches '[alorithm-name] [credentials-and-signature]'
     */
    private static final Pattern AUTH_HEADER_PATTERN = Pattern
            .compile("^(\\S+)\\s+(\\S.+)$");

    /**
     * matches header parameter key/value pairs '[key1]=[val1],[key2]=[val2]'
     */
    private static final Pattern AUTH_PARAMETER_PATTERN = Pattern
            .compile("([^,=]+)=([^,=]+)");

    /**
     * Create an Authorization header in the form
     * 
     * <pre>
     * '[algorithm-name] Credential=[userid],Signature=[signature]'
     * </pre>
     * 
     * Using the default signature algorithm.
     * 
     * @param userid
     * @param signature
     * @return
     */
    public static String formatAuthHeader(String userid, String signature) {
        return formatAuthHeader(new SignedCredential(userid, signature, SIG_ALG));
    }
    
    /**
     * Create an Authorization header in the form
     * 
     * <pre>
     * '[algorithm-name] Credential=[userid],Signature=[signature]'
     * </pre>
     * 
     * @param sc
     * @return
     */
    public static String formatAuthHeader(SignedCredential sc) {
        StringBuilder builder = new StringBuilder();
        builder.append(sc.getAlgorithm()).append(" ");
        builder.append(CRED_FIELD_NAME).append("=");
        builder.append(sc.getUserid()).append(AUTH_PARAMETER_DELIM);
        builder.append(SIG_FIELD_NAME).append("=");
        builder.append(sc.getSignature());
        return builder.toString();
    }
    
    /**
     * Parse an Authorization header in the form
     * 
     * <pre>
     * '[algorithm-name] Credential=[userid],Signature=[signature]'
     * </pre>
     * 
     * @param authHeader
     * @return
     */
    public static SignedCredential parseAuthHeader(String authHeader) {
        Matcher m = AUTH_HEADER_PATTERN.matcher(authHeader.trim());
        if (!m.matches()) {
            return null;
        }
        String algorithm = m.group(1).trim();
        String paramStr = m.group(2).trim();
        Map<String, String> parameters = new HashMap<String, String>(2);
        m = AUTH_PARAMETER_PATTERN.matcher(paramStr);
        while (m.find()) {
            parameters.put(m.group(1).trim().toLowerCase(), m.group(2).trim());
        }
        String userid = parameters.get(CRED_FIELD_NAME.toLowerCase());
        String signature = parameters.get(SIG_FIELD_NAME.toLowerCase());
        return new SignedCredential(userid, signature, algorithm);
    }

    /**
     * Create a non-chuncked (no newlines) base64 string from bytes
     * 
     * @param bytes
     * @return
     */
    public static final String base64Encode(byte[] bytes) {
        bytes = Base64.encodeBase64(bytes, false);
        return org.apache.commons.codec.binary.StringUtils.newStringUtf8(bytes);
    }

    /**
     * Decode a base64 encoded string
     * 
     * @param encString
     * @return
     */
    public static final byte[] base64Decode(String encString) {
        return Base64.decodeBase64(encString);
    }

}
