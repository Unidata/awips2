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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation.HostConfig;
import com.raytheon.uf.viz.collaboration.ui.SiteConfigurationManager;

/**
 * Listens for add/remove events for servers in a Combo box, gets input from
 * user and edits user server list
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 18, 2013 2563       bclement     Initial creation
 * Jan 06, 2014 2563       bclement     added removeDescription
 * Jan 07, 2014 2563       bclement     updated default xmpp port
 * Jan 08, 2014 2563       bclement     renamed from ServerInput to ServerListListener
 *                                      moved input responsibility to ServerInputDialog
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class ServerListListener implements Listener {

    private final Combo serverText;

    public static final String addButtonText = "Add";

    public static final String removeButtonText = "Remove";


    /**
     * @param serverText
     */
    public ServerListListener(Combo serverText) {
        this.serverText = serverText;
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
        if (!(event.widget instanceof Button)){
            return;
        }
        Button b = (Button) event.widget;
        if (b.getText().equals(addButtonText)) {
            add();
        } else if (b.getText().equalsIgnoreCase(removeButtonText)) {
            remove();
        }
    }

    /**
     * Get server to add from user and modify user config.
     */
    private void add() {
        Shell shell = Display.getCurrent().getActiveShell();
        ServerInputDialog dlg = new ServerInputDialog(shell);
        int status = dlg.open();
        if (status == Window.OK) {
            addToOptions(dlg.getDescription(), dlg.getServerAddress());
            if (dlg.isPersist()) {
                HostConfig hc = new HostConfig();
                hc.setPrettyName(dlg.getDescription());
                hc.setHostname(dlg.getServerAddress());
                SiteConfigurationManager.addUserHostConfig(hc);
            }
        }
    }

    /**
     * Get server to remove from user and modify user config.
     */
    private void remove() {
        Shell shell = Display.getCurrent().getActiveShell();
        List<HostConfig> userHosts = SiteConfigurationManager
                .getUserHostConfig();
        if (!userHosts.isEmpty()) {
            String[] options = new String[userHosts.size()];
            Iterator<HostConfig> iter = userHosts.iterator();
            for (int i = 0; iter.hasNext() && i < options.length; ++i) {
                options[i] = iter.next().toString();
            }
            ServerRemoveDialog dlg = new ServerRemoveDialog(shell, options);
            int status = dlg.open();
            if (status == Window.OK) {
                String selection = dlg.getSelection();
                removeFromOptions(selection);
                SiteConfigurationManager.removeUserHostConfig(HostConfig
                        .removeDescription(selection));
            }
        }
    }

    /**
     * Append new server to options combo and select it
     * 
     * @param server
     */
    private void addToOptions(String description, String server) {
        String[] items = serverText.getItems();
        items = Arrays.copyOf(items, items.length + 1);
        items[items.length - 1] = description + " : " + server;
        serverText.setItems(items);
        serverText.select(items.length - 1);
    }

    /**
     * Remove server from options combo
     * 
     * @param displayText
     */
    private void removeFromOptions(String displayText) {
        String[] items = serverText.getItems();
        String[] newItems = new String[items.length - 1];
        for (int i = 0, j = 0; i < items.length; ++i) {
            String old = items[i];
            if (old.equals(displayText)) {
                continue;
            }
            newItems[j] = items[i];
            j += 1;
        }
        serverText.setItems(newItems);
        int oldSelection = serverText.getSelectionIndex();
        if (oldSelection >= newItems.length) {
            serverText.select(newItems.length - 1);
        }
    }

}
