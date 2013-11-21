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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation.SiteConfig;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.SiteConfigurationManager;

/**
 * Change the role for the logged in user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ChangeRoleAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ChangeRoleAction.class);

    private final String role;

    public ChangeRoleAction() {
        super("Change Role");
        this.role = null;
        setMenuCreator(new MenuCreator());
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    public ChangeRoleAction(String role) {
        super(role, Action.AS_RADIO_BUTTON);
        this.role = role;
        Presence presence = CollaborationConnection.getConnection()
                .getPresence();
        String currentRole = (String) presence
                .getProperty(SiteConfigInformation.ROLE_NAME);
        if (role.equals(currentRole)) {
            setChecked(true);
        }
    }

    @Override
    public void run() {
        if (!this.isChecked()) {
            return;
        }
        CollaborationConnection connection = CollaborationConnection
                .getConnection();

        Presence presence = connection.getPresence();
        presence.setProperty(SiteConfigInformation.ROLE_NAME, role);

        try {
            connection.getAccountManager().sendPresence(presence);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error sending presence", e);
        }

    }

    private static class MenuCreator implements IMenuCreator {

        private Menu menu;

        @Override
        public void dispose() {
            menu.dispose();
        }

        @Override
        public Menu getMenu(Control parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        private void fill() {
            SiteConfigInformation siteInfo = SiteConfigurationManager
                    .getSiteConfigInformation();
            Presence presence = CollaborationConnection.getConnection()
                    .getPresence();
            String currentSite = (String) presence
                    .getProperty(SiteConfigInformation.SITE_NAME);
            for (SiteConfig config : siteInfo.getConfig()) {
                if (config.getSite().equals(currentSite)) {
                    for (String role : config.getRoles()) {
                        Action action = new ChangeRoleAction(role);
                        IContributionItem contrib = new ActionContributionItem(
                                action);
                        contrib.fill(menu, -1);
                    }
                }
            }
        }
    };
}
