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

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.Arrays;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.google.common.net.HostAndPort;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;

/**
 * Listens for add server option in a Combo box, gets new input from user and
 * appends to the combo box
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 18, 2013 2563       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class ServerInput implements Listener {

    private final Combo serverText;

    private final int addItemIndex;

    private static final int DEFAULT_XMPP_PORT = 5432;

    private static final int TIMEOUT = 5000; // 5 seconds


    /**
     * @param serverText
     */
    public ServerInput(Combo serverText, int addItemIndex) {
        this.serverText = serverText;
        this.addItemIndex = addItemIndex;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.
     * Event)
     */
    @Override
    public void handleEvent(Event event) {
        int index = serverText.getSelectionIndex();
        if (index == addItemIndex) {
            InputDialog dlg = new InputDialog(Display.getCurrent()
                    .getActiveShell(), "", "Enter server address", "", null);
            int status = dlg.open();
            if (status == Window.OK) {
                String value = dlg.getValue();
                addToOptions(value);
            }
        }
    }

    /**
     * Append new server to options combo and select it
     * 
     * @param server
     */
    private void addToOptions(String server) {
        String[] items = serverText.getItems();
        items = Arrays.copyOf(items, items.length + 1);
        items[items.length - 1] = "Custom Server : " + server;
        serverText.setItems(items);
        serverText.select(items.length - 1);
        // this doesn't persist the new server. I can
        // see that being a good thing or an annoyance.
    }

    /**
     * Validate server by parsing the string and attempting to ping the server.
     * Returns an error string if server string could not be parsed or server
     * could not be contacted.
     * 
     * @param server
     * @return null if there were no issues
     */
    public static String validate(String server) {
        Socket s = null;
        try {
            HostAndPort hnp = HostAndPort.fromString(server).withDefaultPort(
                    DEFAULT_XMPP_PORT);
            s = new Socket();
            s.setReuseAddress(true);
            SocketAddress sa = new InetSocketAddress(hnp.getHostText(),
                    hnp.getPort());
            s.connect(sa, TIMEOUT);
            return null;
        } catch (Exception e) {
            return "Unable to connect to server: " + e.getLocalizedMessage();
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (IOException e) {
                    // no action
                }
            }
        }
    }

    /**
     * Attempt to get canonical host name for server
     * 
     * @param server
     * @return
     * @throws CollaborationException
     */
    public static String getFullName(String server)
            throws CollaborationException {
        try {
            String hostText = HostAndPort.fromString(server).getHostText();
            return InetAddress.getByName(hostText).getCanonicalHostName();
        } catch (Exception e) {
            throw new CollaborationException("Unable to get full host name", e);
        }
    }

}
