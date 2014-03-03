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
package com.raytheon.collaboration.dataserver.auth;

import java.security.GeneralSecurityException;
import java.security.PublicKey;
import java.security.Signature;
import java.util.Collections;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.map.LRUMap;
import org.eclipse.jetty.util.log.Log;
import org.eclipse.jetty.util.log.Logger;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.XMPPError;
import org.jivesoftware.smack.packet.XMPPError.Condition;
import org.jivesoftware.smack.util.SyncPacketSend;

import com.raytheon.collaboration.dataserver.Config;
import com.raytheon.collaboration.dataserver.RestException;
import com.raytheon.collaboration.dataserver.XmppServerConnection;
import com.raytheon.uf.common.http.auth.ServerSignatureAuth;
import com.raytheon.uf.common.http.auth.SignedCredential;
import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.common.xmpp.iq.AuthInfo;

/**
 * Manages authentication credentials for HTTP data server which are stored on
 * the XMPP server
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
public class ServerAuthManager {

    private final Logger log = Log.getLogger(this.getClass());

    private final XmppServerConnection xmppServer;

    private final ServerSignatureAuth sigAuth = new ServerSignatureAuth();

    private final boolean isEnabled;

    private final Map<String, Signature> sigCache;

    /**
     * @param xmppServer
     * @throws XMPPException
     */
    @SuppressWarnings("unchecked")
    public ServerAuthManager(XmppServerConnection xmppServer)
            throws XMPPException {
        isEnabled = BaseProvider.serverSupportsFeature(
                xmppServer.getConnection(), AuthInfo.AUTH_QUERY_XMLNS);
        this.xmppServer = xmppServer;
        int cacheSize = Config.getInt(Config.AUTH_CACHE_SIZE_KEY,
                Config.AUTH_CACHE_SIZE_DEFAULT);
        this.sigCache = Collections.synchronizedMap(new LRUMap(cacheSize));
    }
    
    /**
     * Returns cached signature verification for credential.
     * 
     * @param credential
     * @return
     * @throws RestException
     *             if there was an internal error or credentials are not
     *             authorized for request
     */
    public Signature getAuthorizedSignature(SignedCredential credential)
            throws RestException {
        Signature rval;
        rval = sigCache.get(credential.getUserid());
        if (rval == null) {
            rval = getAuthorizedSignatureDirect(credential);
        }
        return rval;
    }

    /**
     * Returns signature verification for credential from server. Caches result.
     * 
     * @param credential
     * @return
     * @throws RestException
     *             if there was an internal error or credentials are not
     *             authorized for request
     */
    public Signature getAuthorizedSignatureDirect(SignedCredential credential)
            throws RestException {
        return getAuthorizedSignatureDirect(credential, null);
    }

    /**
     * Returns signature verification for credential from server. Caches result.
     * 
     * @param credential
     * @param sessionid
     * @return
     * @throws RestException
     *             if there was an internal error or credentials are not
     *             authorized for request
     */
    public Signature getAuthorizedSignatureDirect(SignedCredential credential,
            String sessionid) throws RestException {
        AuthInfo query;
        if (sessionid == null) {
            query = AuthInfo.createGetPacket(credential.getUserid());
        } else {
            query = AuthInfo.createGetPacket(credential.getUserid(),
                sessionid);
        }
        AuthInfo info = queryServerForAuth(query);
        Signature rval = getSignature(credential, info);
        sigCache.put(credential.getUserid(), rval);
        return rval;
    }

    /**
     * Query XMPP server for auth information
     * 
     * @param query
     * @return
     * @throws RestException
     *             if there was an internal error or credentials are not
     *             authorized for request
     */
    private AuthInfo queryServerForAuth(AuthInfo query) throws RestException {
        AuthInfo rval;
        try {
            XMPPConnection conn = xmppServer.getConnection();
            Packet response = SyncPacketSend.getReply(conn, query);
            if (response instanceof AuthInfo) {
                rval = (AuthInfo) response;
            } else {
                log.warn("Unexpected return type from XMPP server: "
                        + response.toXML());
                throw new RestException(
                        HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                        "Problem communicating with authentication server");
            }
        } catch (XMPPException e) {
            XMPPError xmppError = e.getXMPPError();
            if ( xmppError != null &&xmppError.getCondition().equals(Condition.not_allowed)){
                log.debug("Credential not authorized for sessionid: "
                        + xmppError.getMessage());
                throw new RestException(HttpServletResponse.SC_FORBIDDEN,
                        "Credential not authorized for session");
            } else {
                log.warn("Problem getting signature from server", e);
                throw new RestException(
                        HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                        "Problem communicating with authentication server");
            }
        }
        return rval;
    }

    /**
     * Construct signature validation object using credentials and private key
     * from XMPP server.
     * 
     * @param credential
     * @param info
     * @return
     * @throws RestException
     *             if XMPP server contains malformed auth info
     */
    private Signature getSignature(SignedCredential credential, AuthInfo info)
            throws RestException {
        if (info.getAlgorithm() == null || info.getEncodedKey() == null) {
            log.warn("Missing elements from auth info packet: " + info.toXML());
            throw new RestException(
                    HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Authentication server contains malformed authorization information");
        }
        Signature rval;
        try {
            PublicKey key = sigAuth.decodePublicKey(info.getEncodedKey(),
                    info.getAlgorithm());
            rval = Signature.getInstance(credential.getAlgorithm());
            rval.initVerify(key);
        } catch (GeneralSecurityException e) {
            log.warn("Unable to create signature verification", e);
            throw new RestException(
                    HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Authentication server contains malformed authorization information");
        }
        return rval;
    }

    /**
     * @return true if XMPP server supports public key auth scheme
     */
    public boolean isEnabled() {
        return isEnabled;
    }

}
