package com.raytheon.uf.viz.collaboration.ui;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.SharedGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.data.SessionGroupContainer;

/**
 * Provides contacts list UI elements with icons, text, tooltips, etc
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 20, 2013 2563       bclement    fixed support for ungrouped roster items
 * Jan 24, 2014 2701       bclement    removed local groups, added shared groups
 * Jan 27, 2014 2700       bclement    pass roster entries directly to userLabelProvider
 * Jan 28, 2014 2698       bclement    removed venue info
 * Feb 13, 2014 2751       bclement    made AbstractUsersLabelProvider generic
 * Feb 17, 2014 2751       bclement    added block image logic to userLabelProvider
 * Mar 06, 2014 2848       bclement    get venueName directly from session
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class UsersTreeLabelProvider extends ColumnLabelProvider {

    private AbstractUserLabelProvider<UserId> userLabelProvider = new AbstractUserLabelProvider<UserId>() {

        @Override
        protected Presence getPresence(UserId user) {
            CollaborationConnection connection = CollaborationConnection
                    .getConnection();
            if (connection == null) {
                return null;
            }
            return connection.getContactsManager().getPresence(user);
        }

        protected String getDisplayName(UserId user) {
            return getLocalAlias(user);
        }

        @Override
        protected UserId convertObject(Object element) {
            if (element instanceof RosterEntry) {
                return IDConverter.convertFrom((RosterEntry) element);
            } else if (element instanceof UserId) {
                return (UserId) element;
            } else {
                return null;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.collaboration.ui.AbstractUserLabelProvider#
         * getImageName
         * (com.raytheon.uf.viz.collaboration.comm.identity.user.IUser)
         */
        @Override
        protected String getImageName(UserId user) {
            return isBlocked(user) ? "blocked" : super.getImageName(user);
        }

        /**
         * @param user
         * @return true if we are blocked from seeing updates from user
         */
        private boolean isBlocked(UserId user) {
            boolean rval = false;
            CollaborationConnection conn = CollaborationConnection
                    .getConnection();
            UserId account = conn.getUser();
            if (!account.isSameUser(user)) {
                ContactsManager cm = conn.getContactsManager();
                RosterEntry entry = cm.getRosterEntry(user);
                if (ContactsManager.isBlocked(entry)) {
                    rval = true;
                }
            }
            return rval;
        }

    };

    private List<ILabelProviderListener> listeners;

    private Map<String, Image> imageMap;

    private Font boldFont = null;

    public UsersTreeLabelProvider() {
        listeners = new ArrayList<ILabelProviderListener>();
        imageMap = new HashMap<String, Image>();
    }

    @Override
    public Image getImage(Object element) {
        if (Activator.getDefault() == null) {
            return null;
        }
        String key = "";
        if (element instanceof UserId) {
            return userLabelProvider.getImage(element);
        } else if (element instanceof RosterEntry) {
            return userLabelProvider.getImage((RosterEntry) element);
        } else if (element instanceof RosterGroup) {
            key = "roster_group";
        } else if (element instanceof SharedGroup) {
            key = "shared_group";
        } else if (element instanceof IVenueSession) {
            // key = "session_group";
        } else if (element instanceof SessionGroupContainer) {
            key = "session_group";
        }

        if (imageMap.get(key) == null && !key.equals("")) {
            imageMap.put(key, CollaborationUtils.getNodeImage(key));
        }
        return imageMap.get(key);
    }

    @Override
    public String getText(Object element) {
        if (element instanceof RosterGroup) {
            return ((RosterGroup) element).getName();
        } else if (element instanceof SharedGroup) {
            return ((SharedGroup) element).getName();
        } else if (element instanceof RosterEntry) {
            return userLabelProvider.getText((RosterEntry) element);
        } else if (element instanceof SessionGroupContainer) {
            return "Active Sessions";
        } else if (element instanceof UserId) {
            UserId user = (UserId) element;
            String fullname = userLabelProvider.getText(element);
            CollaborationConnection conn = CollaborationConnection
                    .getConnection();
            UserId me = conn.getUser();
            if (me.isSameUser(user)) {
                // hostname for self
                fullname += " - " + user.getHost();
            }
            return fullname;
        } else if (element instanceof IVenueSession) {
            IVenueSession venue = (IVenueSession) element;
            if (venue.getVenue() == null) {
                return null;
            }
            return venue.getVenueName();
        }
        return null;
    }

    @Override
    public Font getFont(Object element) {
        if (element instanceof RosterGroup || element instanceof SharedGroup
                || element instanceof SessionGroupContainer) {
            // for this case do nothing, as it is not the top level of
            // session groups
            if (boldFont == null) {
                Font currFont = Display.getCurrent().getSystemFont();
                boldFont = new Font(Display.getCurrent(), currFont.toString(),
                        currFont.getFontData()[0].getHeight(), SWT.BOLD);
            }
            return boldFont;
        }
        return null;
    }

    /**
     * Gets the tooltip text on the tree that this is a label provider for
     */
    @Override
    public String getToolTipText(Object element) {
        StringBuilder builder = new StringBuilder();
        if (element instanceof UserId) {
            return userLabelProvider.getToolTipText(element);
        } else if (element instanceof RosterEntry) {
            return userLabelProvider.getToolTipText((RosterEntry) element);
        }
        // builds the tooltip text for the session group
        // portion of the view
        else if (element instanceof IVenueSession) {
            IVenueSession sessGroup = (IVenueSession) element;
            IVenue venue = sessGroup.getVenue();
            builder.append("ID: ").append(venue.getId());
            builder.append("\nName: ").append(venue.getName())
                    .append("\n");
            builder.append("Subject: ").append(venue.getSubject())
                    .append("\n");
            builder.append("Participants: ")
                    .append(venue.getParticipantCount());
            return builder.toString();
        } else {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.CellLabelProvider#getToolTipStyle(java.lang
     * .Object)
     */
    @Override
    public int getToolTipStyle(Object object) {
        return SWT.SHADOW_OUT;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.CellLabelProvider#getToolTipShift(java.lang
     * .Object)
     */
    @Override
    public Point getToolTipShift(Object object) {
        return new Point(5, 5);
    }

    @Override
    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    @Override
    public void dispose() {
        userLabelProvider.dispose();
        for (String key : imageMap.keySet()) {
            imageMap.get(key).dispose();
        }
        if (boldFont != null && !boldFont.isDisposed()) {
            boldFont.dispose();
        }
    }
}
