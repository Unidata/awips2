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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.jivesoftware.smack.packet.Presence;
import org.osgi.framework.Bundle;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.SiteColorInformation;
import com.raytheon.uf.viz.collaboration.ui.SiteColorInformation.SiteColor;
import com.raytheon.uf.viz.collaboration.ui.SiteConfigurationManager;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Built for the session in which everyone joins
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2012            mnash     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 19, 2013 2563       bclement    moved participant filter logic to one method
 * Jan 08, 2014 2563       bclement    changes to match SiteConfigurationManager user sites config
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * Feb 13, 2014 2751       bclement    VenueParticipant refactor
 * Feb 18, 2014 2631       mpduff      Add processJoinAlert()
 * Feb 19, 2014 2751       bclement    add change color icon, fix NPE when user cancels change color
 * Mar 05, 2014 2798       mpduff      Changed how messages are processed for the feed view.
 * Mar 06, 2014 2751       bclement    moved users table refresh logic to refreshParticipantList()
 * Mar 18, 2014 2798       mpduff      Fixed issue with user changing site and participant list not 
 *                                         having the color update to reflect the change.
 * Mar 24, 2014 2936       mpduff      Remove join alerts from feed view.
 * Mar 25, 2014 2938       mpduff      Show status message for site and role changes.
 * Apr 01, 2014 2938       mpduff      Update logic for site and role changes.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SessionFeedView extends SessionView {

    public static final String ID = "com.raytheon.uf.viz.collaboration.SessionFeedView";

    private Action colorChangeAction;

    private Action autoJoinAction;

    private Action userAddSiteAction;

    private Action userRemoveSiteAction;

    private List<String> enabledSites;

    private final List<String> userEnabledSites;

    private List<SiteColor> colors;

    /**
     * Set of users logged in.
     */
    private final ConcurrentHashMap<String, Presence> enabledUsers = new ConcurrentHashMap<String, Presence>();

    /**
     * 
     */
    public SessionFeedView() {
        super();
        String actingSite = CollaborationConnection.getConnection()
                .getPresence().getProperty(SiteConfigInformation.SITE_NAME)
                .toString();
        enabledSites = SiteConfigurationManager.getSubscribeList(actingSite);
        userEnabledSites = SiteConfigurationManager.getUserSubscribeList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#initComponents
     * (org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void initComponents(Composite parent) {
        super.initComponents(parent);
        colors = SiteConfigurationManager.getSiteColors();
        if (colors != null) {
            for (VenueParticipant user : session.getVenue().getParticipants()) {
                setColorForSite(user);
            }
        } else {
            colors = new ArrayList<SiteColor>();
        }
        usersTable.refresh();
    }

    @Subscribe
    public void refreshBlockList(SubscribeList list) {
        enabledSites = list.getEnabledSites();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#createActions()
     */
    @Override
    protected void createActions() {
        super.createActions();
        Bundle bundle = Activator.getDefault().getBundle();
        colorChangeAction = new Action("Change Site Color...",
                IconUtil.getImageDescriptor(bundle, "change_color.gif")) {
            @Override
            public void run() {
                ColorDialog dlg = new ColorDialog(Display.getCurrent()
                        .getActiveShell());
                RGB rgb = dlg.open();
                if (rgb != null) {
                    /*
                     * get the selected entry so we know what site to change the
                     * color for
                     */
                    String site = getSelectedSite();

                    replaceSiteColor(site.toString(), rgb);
                    /*
                     * loop through all the entries in the list so we can set
                     * the color for all sites corresponding to "selectedSite"
                     */
                    if (site != null) {
                        for (VenueParticipant user : session.getVenue()
                                .getParticipants()) {
                            setColorForSite(user);
                        }
                    }

                    usersTable.refresh();
                }
            }
        };

        autoJoinAction = new Action(CollabPrefConstants.AUTO_JOIN, SWT.TOGGLE) {
            @Override
            public void run() {
                Activator
                        .getDefault()
                        .getPreferenceStore()
                        .setValue(CollabPrefConstants.AUTO_JOIN,
                                autoJoinAction.isChecked());
            };
        };

        autoJoinAction.setChecked(Activator.getDefault().getPreferenceStore()
                .getBoolean(CollabPrefConstants.AUTO_JOIN));
        Activator.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new IPropertyChangeListener() {
                    @Override
                    public void propertyChange(PropertyChangeEvent event) {
                        autoJoinAction.setChecked(Activator.getDefault()
                                .getPreferenceStore()
                                .getBoolean(CollabPrefConstants.AUTO_JOIN));
                    }
                });

        userAddSiteAction = new Action("Subscribe") {
            @Override
            public void run() {
                userEnabledSites.add(getSelectedSite());
            };
        };

        userRemoveSiteAction = new Action("Unsubscribe") {
            @Override
            public void run() {
                userEnabledSites.remove(getSelectedSite());
            }
        };

        MenuManager manager = (MenuManager) getViewSite().getActionBars()
                .getMenuManager();
        manager.add(autoJoinAction);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#fillContextMenu
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    protected void fillContextMenu(IMenuManager manager) {
        super.fillContextMenu(manager);
        manager.add(colorChangeAction);
        String site = getSelectedSite();
        if (userEnabledSites.contains(site) == false
                && enabledSites.contains(site) == false) {
            userAddSiteAction.setText("Subscribe to " + getSelectedSite());
            manager.add(userAddSiteAction);
        } else if (enabledSites.contains(site) == false
                && userEnabledSites.contains(site)) {
            userRemoveSiteAction.setText("Unsubscribe from "
                    + getSelectedSite());
            manager.add(userRemoveSiteAction);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#setParticipantValues
     * (com.raytheon.uf.viz.collaboration.ui.session.ParticipantsLabelProvider)
     */
    @Override
    protected void setParticipantValues(ParticipantsLabelProvider labelProvider) {
        super.setParticipantValues(labelProvider);
        labelProvider.setEnabledSites(enabledSites);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#handleMessage
     * (com.raytheon.uf.viz.collaboration.comm.identity.IMessage)
     */
    @Override
    public void handleMessage(IMessage message) {
        final IMessage msg = message;
        boolean isHistory = isHistory(msg);

        // so not to have delay, going to handle messages from yourself
        // separately
        if (isSelf(msg, isHistory)) {
            return;
        }

        Object site = null;
        if (isHistory) {
            site = msg.getSubject();
        } else if (msg.getFrom() instanceof VenueParticipant) {
            Presence presence = session.getVenue().getPresence(
                    (VenueParticipant) msg.getFrom());
            site = presence.getProperty(SiteConfigInformation.SITE_NAME);
        }

        // should we append?
        if (site == null || enabledSites.contains(site)
                || userEnabledSites.contains(site)) {
            appendMessage(msg);
        }
    }

    @Override
    protected void styleAndAppendText(StringBuilder sb, int offset,
            String name, VenueParticipant userId, String subject,
            List<StyleRange> ranges) {
        if (subject != null) {
            setColorForSite(userId, subject);
        }
        super.styleAndAppendText(sb, offset, name, userId, subject, ranges);
    }

    /**
     * Get the acting site based on the first user that is selected out of the
     * list
     * 
     * @return
     */
    private String getSelectedSite() {
        IStructuredSelection selection = (IStructuredSelection) usersTable
                .getSelection();
        VenueParticipant selectedEntry = (VenueParticipant) selection
                .getFirstElement();
        Presence pres = session.getVenue().getPresence(selectedEntry);
        Object selectedSite = pres.getProperty(SiteConfigInformation.SITE_NAME);
        return selectedSite == null ? "" : selectedSite.toString();
    }

    /**
     * Takes an IRosterEntry and sets their color in the SessionColorManager for
     * the site that they belong to, calls into
     * setColorForSite(UserId,IPresence)
     * 
     * @param user
     */
    private void setColorForSite(VenueParticipant user) {
        Presence presence = session.getVenue().getPresence(user);
        setColorForSite(user, presence);
    }

    /**
     * Does the work for setting the color for each user that belongs to a site
     * 
     * @param id
     * @param presence
     */
    private void setColorForSite(VenueParticipant id, Presence presence) {
        if (presence == null) {
            return;
        }
        Object site = presence.getProperty(SiteConfigInformation.SITE_NAME);
        if (site != null) {
            setColorForSite(id, site.toString());
        }
    }

    private void setColorForSite(VenueParticipant id, String site) {
        SiteColor siteColor = new SiteColor();
        siteColor.setSite(site.toString());
        int index = colors.indexOf(siteColor);
        if (index >= 0) {
            SiteColor actualColor = colors.get(index);
            colorManager.setColorForUser(id, actualColor.getColor());
        }
    }

    /**
     * Removes the color from the map if the site exists in the list
     * 
     * @param site
     * @param rgb
     */
    private void replaceSiteColor(String site, RGB rgb) {
        // now that the users have their color set, we need to add
        // to the list that has the site color information
        SiteColor color = new SiteColor();
        color.setSite(site);
        color.setColor(rgb);
        boolean exists = false;
        for (SiteColor col : SessionFeedView.this.colors) {
            if (col.getSite().equals(site)) {
                exists = true;
            }
        }
        if (exists) {
            SessionFeedView.this.colors.remove(color);
        }
        SessionFeedView.this.colors.add(color);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.SessionView#
     * sendParticipantSystemMessage
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId,
     * java.lang.String)
     */
    @Override
    protected void sendParticipantSystemMessage(VenueParticipant participant,
            String message) {
        super.sendParticipantSystemMessage(participant, message);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.SessionView#
     * participantPresenceUpdated
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId,
     * org.jivesoftware.smack.packet.Presence)
     */
    @Override
    protected void participantPresenceUpdated(VenueParticipant participant,
            Presence presence) {
        // Verify we have properties
        if (!presence.getPropertyNames().contains(
                SiteConfigInformation.SITE_NAME)) {
            return;
        }

        String siteName = getSiteName(presence);

        // only show sites you care about
        if (enabledSites.contains(siteName)
                || userEnabledSites.contains(siteName)) {
            String user = participant.getName();
            Presence prev = enabledUsers.get(user);

            String roleName = getRoleName(presence);
            if (presence.isAvailable()) {
                if (prev == null || hasPresenceChanged(prev, presence)) {
                    StringBuilder message = getMessage(roleName, siteName, user);
                    sendSystemMessage(message);
                }

                enabledUsers.put(user, presence);
            }
        }

        /*
         * Presence changed is triggered for participant's site being changed.
         * Need to set the color to handle this situation.
         */
        setColorForSite(participant, presence);
        refreshParticipantList();
    }

    /**
     * Determine if the user's presence has changed.
     * 
     * @param prev
     *            The previous Presence object
     * @param current
     *            The current Presence object
     * @return true if the presence has changed
     */
    private boolean hasPresenceChanged(Presence prev, Presence current) {
        if (!getRoleName(prev).equals(getRoleName(current))) {
            return true;
        }

        if (!getSiteName(prev).equals(getSiteName(current))) {
            return true;
        }

        return false;
    }

    /**
     * Get the role name from the presence.
     * 
     * @param presence
     *            The Presence
     * @return the role name for this presence
     */
    private String getRoleName(Presence presence) {
        Object roleObj = presence.getProperty(SiteConfigInformation.ROLE_NAME);
        return roleObj == null ? "" : roleObj.toString();
    }

    /**
     * Get the site name from the presence.
     * 
     * @param presence
     *            The Presence
     * @return the site name for this presence
     */
    private String getSiteName(Presence presence) {
        Object siteObj = presence.getProperty(SiteConfigInformation.SITE_NAME);
        return siteObj == null ? "" : siteObj.toString();
    }

    /**
     * Get the status message.
     * 
     * @param roleName
     * @param siteName
     * @param user
     * 
     * @return The StringBuilder instance holding the message
     */
    private StringBuilder getMessage(String roleName, String siteName,
            String user) {
        StringBuilder message = new StringBuilder();
        message.append(user);
        message.append(" ").append(roleName).append(" ").append(siteName);
        return message;
    }

    /**
     * No operation for Session Feed View
     */
    @Override
    protected void participantArrived(VenueParticipant participant,
            String description) {
        refreshParticipantList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void participantDeparted(VenueParticipant participant,
            String description) {
        if (enabledUsers.remove(participant.getName()) != null) {
            super.participantDeparted(participant, description);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.SessionView#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        SiteColorInformation information = new SiteColorInformation();
        information.setColors(this.colors);
        SiteConfigurationManager.writeSiteColorInformation(information);

        // write out the user enabled sites information to a file
        String[] sites = userEnabledSites.toArray(new String[userEnabledSites
                .size()]);
        SiteConfigurationManager.writeUserEnabledSites(sites);
    }
}
