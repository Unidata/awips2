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
package com.raytheon.uf.common.xmpp.iq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jivesoftware.smack.packet.IQ;

import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.XmlBuilder;
import com.raytheon.uf.common.xmpp.XmlBuilder.Pair;

/**
 * Custom IQ packet for public key authentication
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AuthInfo extends IQ {

    public static final String AUTH_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:auth";

    public static final String INFO_ELEMENT_NAME = "authinfo";

    public static final String PUBKEY_ELEMENT_NAME = "publickey";

    public static final String ALG_ATTRIBUTE = "algorithm";

    public static final String ID_ATTRIBUTE = "jid";

    public static final String SESSION_ATTRIBUTE = "sessionid";

    private String encodedKey;

    private String algorithm;

    private String userid;

    private String sessionid;

    /**
     * 
     */
    public AuthInfo() {
    }

    /**
     * @param iq
     */
    public AuthInfo(IQ iq) {
        super(iq);
    }

    /**
     * @param encodedKey
     * @param algorithm
     * @param userid
     * @param sessionid
     */
    public AuthInfo(String encodedKey, String algorithm, String userid,
            String sessionid) {
        this.encodedKey = encodedKey;
        this.algorithm = algorithm;
        this.userid = userid;
        this.sessionid = sessionid;
    }

    /**
     * Construct a setter packet used to store a new public key on server
     * 
     * @param encodedKey
     * @param algorithm
     * @return
     */
    public static AuthInfo createSetPacket(String encodedKey, String algorithm) {
        AuthInfo rval = new AuthInfo();
        rval.setEncodedKey(encodedKey);
        rval.setAlgorithm(algorithm);
        rval.setType(Type.SET);
        return rval;
    }

    /**
     * Create a getter packet used to query the server for the current public
     * key
     * 
     * @param userid
     * @return
     */
    public static AuthInfo createGetPacket(String userid) {
        return createGetPacket(userid, null);
    }

    /**
     * Create a getter packet used to query the server for the current public
     * key and check authorization for session ownership.
     * 
     * @param userid
     * @param sessionid
     * @return
     */
    public static AuthInfo createGetPacket(String userid, String sessionid) {
        AuthInfo rval = new AuthInfo();
        rval.setUserid(userid);
        rval.setSessionid(sessionid);
        rval.setType(Type.GET);
        return rval;
    }


    /* (non-Javadoc)
     * @see org.jivesoftware.smack.packet.IQ#getChildElementXML()
     */
    @Override
    public String getChildElementXML() {
        XmlBuilder builder = new XmlBuilder();
        List<Pair> queryAttributes = new ArrayList<Pair>(2);
        if (userid != null) {
            queryAttributes.add(new Pair(ID_ATTRIBUTE, userid));
        }
        if (sessionid != null) {
            queryAttributes.add(new Pair(SESSION_ATTRIBUTE, sessionid));
        }
        builder.startTag(PacketConstants.QUERY_ELEMENT_NAME, AUTH_QUERY_XMLNS,
                queryAttributes);
        builder.startTag(INFO_ELEMENT_NAME, PacketConstants.COLLAB_XMLNS);
        builder.startTag(PUBKEY_ELEMENT_NAME,
                Arrays.asList(new Pair(ALG_ATTRIBUTE, algorithm)));
        builder.appendText(encodedKey);
        builder.endTag(PUBKEY_ELEMENT_NAME);
        builder.endTag(INFO_ELEMENT_NAME);
        builder.endTag(PacketConstants.QUERY_ELEMENT_NAME);
        return builder.toXml();
    }

    /**
     * @return the encodedKey
     */
    public String getEncodedKey() {
        return encodedKey;
    }

    /**
     * @return the algorithm
     */
    public String getAlgorithm() {
        return algorithm;
    }

    /**
     * @return the userid
     */
    public String getUserid() {
        return userid;
    }

    /**
     * @return the sessionid
     */
    public String getSessionid() {
        return sessionid;
    }

    /**
     * @param encodedKey
     *            the encodedKey to set
     */
    public void setEncodedKey(String encodedKey) {
        this.encodedKey = encodedKey;
    }

    /**
     * @param algorithm
     *            the algorithm to set
     */
    public void setAlgorithm(String algorithm) {
        this.algorithm = algorithm;
    }

    /**
     * @param userid
     *            the userid to set
     */
    public void setUserid(String userid) {
        this.userid = userid;
    }

    /**
     * @param sessionid
     *            the sessionid to set
     */
    public void setSessionid(String sessionid) {
        this.sessionid = sessionid;
    }

}
