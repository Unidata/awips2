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

import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.data.SessionGroupContainer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class UsersTreeLabelProvider extends ColumnLabelProvider {
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
            IPresence presence = CollaborationConnection.getConnection()
                    .getPresence();
            if (presence != null) {
                String mode = presence.getMode().toString();
                mode = mode.replaceAll("\\s+", "_");
                key = mode;
            }
        } else if (element instanceof IRosterEntry) {
            IRosterEntry entry = (IRosterEntry) element;
            if (entry.getPresence() != null
                    && entry.getPresence().getType() == Type.AVAILABLE) {
                key = entry.getPresence().getMode().toString()
                        .replaceAll("\\s+", "_");
            } else {
                key = "contact_disabled";
            }
        } else if (element instanceof IRosterGroup) {
            key = "group";
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
        if (element instanceof IRosterEntry) {
            IRosterEntry entry = (IRosterEntry) element;
            UserId id = IDConverter.convertFrom(entry.getUser());
            if (id.getAlias() != null && !id.getAlias().isEmpty()) {
                return id.getAlias();
            } else if (entry.getUser().getName() != null
                    && !entry.getUser().getName().isEmpty()) {
                return entry.getUser().getName();
            } else {
                return id.getName();
            }
        } else if (element instanceof IRosterGroup) {
            return ((IRosterGroup) element).getName();
        } else if (element instanceof SessionGroupContainer) {
            return "Active Sessions";
        } else if (element instanceof UserId) {
            return ((UserId) element).getName() + " - "
                    + ((UserId) element).getHost();
        } else if (element instanceof IVenueSession) {
            if (((IVenueSession) element).getVenue() == null) {
                return null;
            }
            return ((IVenueSession) element).getVenue().getInfo()
                    .getVenueDescription();
        }
        return null;
    }

    @Override
    public Font getFont(Object element) {
        if (element instanceof IRosterGroup
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
        if (element instanceof IRosterEntry) {
            IRosterEntry user = (IRosterEntry) element;
            builder.append("ID: ").append(
                    IDConverter.convertFrom(user.getUser()).getFQName());
            builder.append("\nStatus: ");
            if (user.getPresence() == null
                    || user.getPresence().getType() == Type.UNAVAILABLE) {
                builder.append("Offline");
            } else {
                builder.append(CollaborationUtils.formatMode(user.getPresence()
                        .getMode()));

                // builder.append("Type: ").append(user.getType())
                // .append("\n");
                String message = user.getPresence().getStatus();
                if (message != null && message.length() > 0) {
                    builder.append("\n");
                    builder.append("Message: \"").append(
                            user.getPresence().getStatus() + "\"");
                }
            }
            return builder.toString();
        }
        // builds the tooltip text for the session group
        // portion of the view
        else if (element instanceof IVenueSession) {
            IVenueSession sessGroup = (IVenueSession) element;
            IVenueInfo info = sessGroup.getVenue().getInfo();
            builder.append("ID: ").append(info.getVenueID());
            builder.append("\nName: ").append(info.getVenueDescription())
                    .append("\n");
            builder.append("Subject: ").append(info.getVenueSubject())
                    .append("\n");
            builder.append("Participants: ").append(info.getParticipantCount());
            return builder.toString();
        } else {
            return null;
        }
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
        for (String key : imageMap.keySet()) {
            imageMap.get(key).dispose();
        }
        if (boldFont != null && !boldFont.isDisposed()) {
            boldFont.dispose();
        }
    }
}
