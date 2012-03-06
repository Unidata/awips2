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
package com.raytheon.uf.viz.collaboration.ui.login;

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

import java.util.List;

import org.eclipse.ecf.core.ContainerCreateException;
import org.eclipse.ecf.core.ContainerFactory;
import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.security.ConnectContextFactory;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationData.class);

    private static CollaborationData data;

    private static IContainer client;

    // private static IPresenceContainerAdapter presence;

    // private Map<IChat, List<ID>> openChats;

    private CollaborationData() {
        // openChats = new HashMap<IChat, List<ID>>();
    }

    public static CollaborationData getInstance() {
        if (data == null) {
            data = new CollaborationData();
        }
        return data;
    }

    public void connect(String user, String pass) {
        // XMPPConnection.DEBUG_ENABLED = true;
        try {
            if (client == null) {
                client = ContainerFactory.getDefault().createContainer(
                        "ecf.xmpp.smack");
            }

            if (client.getConnectedID() != null) {
                return;
            }

            // if (presence == null) {
            // presence = (IPresenceContainerAdapter) client
            // .getAdapter(IPresenceContainerAdapter.class);
            //
            // presence.getChatManager().addMessageListener(
            // new IIMMessageListener() {
            // @Override
            // public void handleMessageEvent(
            // IIMMessageEvent messageEvent) {
            // if (messageEvent instanceof ChatMessageEvent) {
            // ChatMessageEvent event = (ChatMessageEvent) messageEvent;
            // System.out.println(event.getChatMessage()
            // .getBody());
            // }
            // }
            // });
            // }

            // TODO XXX this obviously should be dynamic
            ID id = IDFactory.getDefault().createID(
                    client.getConnectNamespace(),
                    user + "@awipscm.omaha.us.ray.com");
            client.connect(id,
                    ConnectContextFactory.createPasswordConnectContext(pass));
            statusHandler.handle(Priority.INFO, id.getName()
                    + " connected to collaboration");
        } catch (ContainerCreateException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (ECFException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        PlatformUI.getWorkbench().addWorkbenchListener(
                new IWorkbenchListener() {

                    @Override
                    public boolean preShutdown(IWorkbench workbench,
                            boolean forced) {
                        return true;
                    }

                    @Override
                    public void postShutdown(IWorkbench workbench) {
                        CollaborationData.getInstance().disconnect();
                    }
                });
    }

    protected void disconnect() {
        statusHandler.handle(Priority.INFO,
                "Disconnecting from collaboration...");
        System.out.println("disconnecting");
        client.disconnect();
    }

    public void addChat(List<ID> toUsers) {
        // openChats.add(toUsers);
    }

    // needs work TODO
    public boolean hasChat(List<ID> users) {
        System.err.println("implement hasChat");
        // for (IChat chat : openChats.keySet()) {
        // for (ID id : openChats.get(chat)) {
        // }
        // }
        return false;
    }

    // public IChat getOpenChat(List<ID> ids) {
    // return null;
    // }

    /**
     * @return the client
     */
    public IContainer getClient() {
        return client;
    }

    /**
     * @return the presence
     */
    // public IPresenceContainerAdapter getPresence() {
    // return presence;
    // }
}
