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
import com.raytheon.uf.viz.collaboration.ui.session.SubscribeList;

/**
 * Change the site for the logged in user
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

public class ChangeSiteAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ChangeSiteAction.class);

    private final String site;

    public ChangeSiteAction() {
        super("Change Site");
        this.site = null;
        setMenuCreator(new MenuCreator());
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    public ChangeSiteAction(String site) {
        super(site, Action.AS_RADIO_BUTTON);
        this.site = site;
        Presence presence = CollaborationConnection.getConnection()
                .getPresence();
        String currentSite = (String) presence
                .getProperty(
                SiteConfigInformation.SITE_NAME);
        if (site.equals(currentSite)) {
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
        presence.setProperty(SiteConfigInformation.SITE_NAME, site);
        // now need to send the new subscribe list out to those who are
        // listening for it
        SubscribeList list = new SubscribeList();
        list.setEnabledSites(SiteConfigurationManager.getSubscribeList(site));
        connection.postEvent(list);

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
            for (SiteConfig config : siteInfo.getConfig()) {
                Action action = new ChangeSiteAction(config.getSite());
                IContributionItem contrib = new ActionContributionItem(action);
                contrib.fill(menu, -1);
            }
        }
    };
}
