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
package com.raytheon.uf.viz.collaboration.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;
import org.jivesoftware.smack.packet.Presence.Type;
import org.jivesoftware.smack.packet.RosterPacket.ItemStatus;
import org.jivesoftware.smack.packet.RosterPacket.ItemType;

import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Common code that is used whenever providing labels for users in a tree.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012            bsteffen     Initial creation
 * Jan 27, 2014 2700       bclement     added roster entry support
 * Feb 13, 2014 2751       bclement     made generic for IUsers
 * Feb 13, 2014 2751       njensen      Extracted getImageName() to allow overrides
 * Feb 17, 2014 2751       bclement     moved block image logic to roster specific code
 * Apr 24, 2014 3070       bclement     added pending contact icon
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractUserLabelProvider<T extends IUser> extends
        ColumnLabelProvider {

    protected Map<String, Image> imageMap = new HashMap<String, Image>();

    @Override
    public String getText(Object element) {
        T user = convertObject(element);
        if (user == null) {
            return null;
        }
        StringBuilder name = new StringBuilder();
        name.append(getDisplayName(user));
        Presence presence = getPresence(user);
        if (presence != null) {
            Object site = presence.getProperty(SiteConfigInformation.SITE_NAME);
            if (site != null) {
                name.append(" - ");
                name.append(site);
            }
            Object role = presence.getProperty(SiteConfigInformation.ROLE_NAME);
            if (role != null) {
                name.append(" - ");
                name.append(role);
            }
        }
        return name.toString();
    }

    /**
     * Cast object to appropriate type
     * 
     * @param element
     * @return null if object cannot be cast
     */
    abstract protected T convertObject(Object element);

    @Override
    public Image getImage(Object element) {
        T user = convertObject(element);
        if (user == null) {
            return null;
        }
        String key = getImageName(user);
        if (element instanceof RosterEntry) {
            RosterEntry entry = (RosterEntry) element;
            ItemStatus status = entry.getStatus();
            if (status != null) {
                /* status always indicates pending */
                key = "pending";
            }
        }

        if (imageMap.get(key) == null && !key.equals("")) {
            imageMap.put(key, CollaborationUtils.getNodeImage(key));
        }
        return imageMap.get(key);
    }

    @Override
    public String getToolTipText(Object element) {
        T user = convertObject(element);
        if (user == null) {
            return null;
        }
        Presence presence = getPresence(user);
        StringBuilder text = new StringBuilder();
        text.append("Name: ").append(getDisplayName(user)).append("\n");
        text.append("Status: ");
        if (presence == null || presence.getType() != Type.available) {
            text.append("Offline \n");
        } else {
            text.append(CollaborationUtils.formatMode(presence.getMode()))
                    .append("\n");
            if (presence.getStatus() != null && !presence.getStatus().isEmpty()) {
                text.append("Message : \"").append(presence.getStatus())
                        .append("\"\n");
            }
            for (String key : presence.getPropertyNames()) {
                Object value = presence.getProperty(key);
                if (value != null && key != null) {
                    text.append(key).append(" : ").append(value).append("\n");
                }
            }
        }
        if (element instanceof RosterEntry) {
            RosterEntry entry = (RosterEntry) element;
            ItemType type = entry.getType();
            if (type != null) {
                text.append("Subscription: ").append(type).append("\n");
            }
            ItemStatus status = entry.getStatus();
            if (status != null) {
                /* status always indicates pending */
                text.append("Contact request pending\n");
            }
        }
        // delete trailing newline
        text.deleteCharAt(text.length() - 1);
        return text.toString();
    }

    @Override
    public int getToolTipStyle(Object object) {
        return SWT.SHADOW_OUT;
    }

    @Override
    public Point getToolTipShift(Object object) {
        return new Point(5, 5);
    }

    @Override
    public void dispose() {
        for (String key : imageMap.keySet()) {
            imageMap.get(key).dispose();
        }
        imageMap.clear();
    }

    protected static String getLocalAlias(UserId user) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            String name = user.getAlias();
            if (name == null) {
                name = user.getName();
            }
            return name;
        } else {
            return connection.getContactsManager().getDisplayName(user);
        }
    }

    /**
     * @param user
     * @return display text for user name
     */
    abstract protected String getDisplayName(T user);

    /**
     * @param user
     * @return last known presence for user
     */
    abstract protected Presence getPresence(T user);

    /**
     * Gets the image name for an icon associated with an element
     * 
     * @param element
     * @return
     */
    protected String getImageName(T user) {
        Presence presence = getPresence(user);
        String key = "";
        if (presence != null && presence.getType() == Type.available) {
            Mode mode = presence.getMode();
            if (mode == null) {
                mode = Mode.available;
            }
            key = mode.toString().replaceAll("\\s+", "_");
        } else {
            key = "contact_disabled";
        }
        return key;
    }

}
