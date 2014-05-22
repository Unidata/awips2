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
package com.raytheon.uf.viz.collaboration.comm.provider.account;

import java.net.URI;
import java.security.GeneralSecurityException;
import java.security.SignatureException;

import org.apache.http.message.AbstractHttpMessage;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.util.SyncPacketSend;

import com.raytheon.uf.common.http.auth.ClientSignatureAuth;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.common.xmpp.iq.AuthInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;

/**
 * Manages collaboration client's authentication credentials for HTTP data
 * server which are stored on the XMPP server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2014 2756       bclement     Initial creation
 * Apr 14, 2014 2903       bclement    moved from session subpackage to account
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ClientAuthManager {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(ClientAuthManager.class);

    private final XMPPConnection conn;

    public static enum State {
        PENDING, ENABLED, DISABLED, ERROR
    }

    private State state = State.PENDING;

    private final Object stateMonitor = new Object();

    private ClientSignatureAuth sigAuth;

    /**
     * @param conn
     * @throws CollaborationException
     */
    public ClientAuthManager(XMPPConnection conn) throws CollaborationException {
        this.conn = conn;
        if (isEnabledOnServer(conn)) {
            createAndRegisterAuth();
        } else {
            this.sigAuth = null;
            this.state = State.DISABLED;
        }
    }

    /**
     * @param conn
     * @return true if server supports collaboration auth feature
     * @throws CollaborationException
     */
    public static boolean isEnabledOnServer(XMPPConnection conn)
            throws CollaborationException {
        try {
            return BaseProvider.serverSupportsFeature(conn,
                    AuthInfo.AUTH_QUERY_XMLNS);
        } catch (XMPPException e) {
            throw new CollaborationException(
                    "Unable to query server for supported features", e);
        }
    }

    /**
     * Create a new key pair and register key with server
     * 
     * @return
     * @throws CollaborationException
     */
    private void createAndRegisterAuth() {
        Job job = new Job("Creating and registering collaboration credentials") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                State authState = State.ERROR;
                try {
                    ClientSignatureAuth auth = new ClientSignatureAuth();
                    String encodedKey = auth.getEncodedPublicKey();
                    AuthInfo packet = AuthInfo.createSetPacket(encodedKey,
                            auth.getKeyAlgorithm());
                    SyncPacketSend.getReply(conn, packet);
                    ClientAuthManager.this.sigAuth = auth;
                    authState = State.ENABLED;
                } catch (GeneralSecurityException e) {
                    log.error("Problem creating public key auth", e);
                } catch (XMPPException e) {
                    log.error("Problem registering public key", e);
                } finally {
                    synchronized (stateMonitor) {
                        ClientAuthManager.this.state = authState;
                        stateMonitor.notifyAll();
                    }
                }
                return Status.OK_STATUS;
            }
        };
        job.setPriority(Job.SHORT);
        job.setSystem(true);
        job.schedule();
    }

    /**
     * NOTE: this method will block if auth registration has not yet completed
     * 
     * @return the sigAuth, null if {@link ClientAuthManager#getState()} is
     *         {@link State#DISABLED}
     */
    public ClientSignatureAuth getSigAuth() {
        if (state.equals(State.PENDING)) {
            waitForInit();
        }
        return sigAuth;
    }

    /**
     * Add an Auth header to the message using the credential and a signature
     * generated from the URI.
     * 
     * @param msg
     * @param credential
     * @param uri
     * @throws CollaborationException
     */
    public void signRequest(AbstractHttpMessage msg, String credential, URI uri)
            throws CollaborationException {
        signRequest(msg, credential, uri, null);
    }

    /**
     * Add an Auth header to the message using the credential and a signature
     * generated from the URI and body.
     * 
     * @param msg
     * @param credential
     * @param uri
     * @param body
     * @throws CollaborationException
     */
    public void signRequest(AbstractHttpMessage msg, String credential,
            URI uri, byte[] body) throws CollaborationException {
        ClientSignatureAuth sigAuth = getSigAuth();
        if (state.equals(State.ENABLED)) {
            String sig;
            try {
                if (body != null) {
                    sig = sigAuth.sign(uri, body);
                } else {
                    sig = sigAuth.sign(uri);
                }
            } catch (SignatureException e) {
                throw new CollaborationException(
                        "Problem signing HTTP message", e);
            }
            String headerValue = ClientSignatureAuth.formatAuthHeader(
                    credential, sig);
            msg.addHeader(ClientSignatureAuth.HTTP_AUTH_HEADER, headerValue);
        }
    }

    /**
     * waits thread until state is no longer pending auth registration
     */
    private void waitForInit() {
        synchronized (stateMonitor) {
            if (state.equals(State.PENDING)) {
                try {
                    stateMonitor.wait();
                } catch (InterruptedException e) {
                    log.error("Interrupted waiting for auth", e);
                }
            }
        }
    }

    /**
     * @return the state of the auth registration
     */
    public State getState() {
        return state;
    }

}
