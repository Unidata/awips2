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
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketTypeFilter;
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
import com.raytheon.uf.common.xmpp.iq.SecurityToggle;
import com.raytheon.uf.common.xmpp.iq.SecurityToggle.Mode;

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
 * Mar 04, 2014 2756       bclement     added security toggle
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

    private boolean isEnabled;

    private final Map<String, Signature> sigCache;

    /**
     * @param xmppServer
     * @throws XMPPException
     */
    @SuppressWarnings("unchecked")
    public ServerAuthManager(XmppServerConnection xmppServer)
            throws XMPPException {
        this.xmppServer = xmppServer;
        XMPPConnection conn = xmppServer.getConnection();
        registerListeners(conn);
        isEnabled = isEnabledOnServer(conn);
        int cacheSize = Config.getInt(Config.AUTH_CACHE_SIZE_KEY,
                Config.AUTH_CACHE_SIZE_DEFAULT);
        this.sigCache = Collections.synchronizedMap(new LRUMap(cacheSize));
    }
    
    /**
     * Register XMPP packet and event listeners with connection
     * 
     * @param conn
     */
    private void registerListeners(final XMPPConnection conn) {
        conn.addPacketListener(new PacketListener() {
            @Override
            public void processPacket(Packet packet) {
                /*
                 * XMPP server doesn't send disable/enable message, only update
                 * messages that signal the client to query for the new state
                 */
                if (packet instanceof SecurityToggle) {
                    if (((SecurityToggle) packet).getMode()
                            .equals(Mode.UPDATED)) {
                        try {
                            isEnabled = querySecurityToggle(conn);
                        } catch (XMPPException e) {
                            log.warn(
                                    "Problem getting updated security settings",
                                    e);
                        }
                    }
                }
            }
        }, new PacketTypeFilter(SecurityToggle.class));
    }

    /**
     * @param conn
     * @return true if security is enabled on the server
     * @throws XMPPException
     */
    public boolean isEnabledOnServer(XMPPConnection conn) throws XMPPException {
        boolean rval = BaseProvider.serverSupportsFeature(conn,
                AuthInfo.AUTH_QUERY_XMLNS);
        if (rval) {
            // assume enabled if server doesn't support toggle
            if (BaseProvider.serverSupportsFeature(conn,
                    SecurityToggle.TOGGLE_QUERY_XMLNS)) {
                rval = querySecurityToggle(conn);
            }
        }
        return rval;
    }

    /**
     * Check for security enabled toggle on server. This allows for the xmpp
     * server to run the public key auth service with new clients without
     * enforcing security; allowing compatibility with legacy clients
     * 
     * @param conn
     * @return true if security toggle is set to enabled
     * @throws XMPPException
     */
    private boolean querySecurityToggle(XMPPConnection conn)
            throws XMPPException {
        boolean rval = false;
        SecurityToggle query = SecurityToggle.createGet();
        Packet response = SyncPacketSend.getReply(conn, query);
        if (response instanceof SecurityToggle) {
            rval = ((SecurityToggle) response).getMode().equals(Mode.ENABLED);
        } else {
            log.warn("Unexpected response from security toggle query: "
                    + response.toXML());
        }
        return rval;
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
