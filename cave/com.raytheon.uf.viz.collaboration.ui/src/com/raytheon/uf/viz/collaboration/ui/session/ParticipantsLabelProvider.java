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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.SharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.display.data.SessionColorManager;
import com.raytheon.uf.viz.collaboration.ui.AbstractUserLabelProvider;

/**
 * Generate the Participant's label and icon image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            mnash     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ParticipantsLabelProvider extends AbstractUserLabelProvider {

    protected String sessionId = null;

    private List<String> enabledSites;

    protected Map<RGB, Color> colors = new HashMap<RGB, Color>();

    private SessionColorManager manager;

    private Font boldFont;

    private Font underlinedFont;

    private Font combinedFont;

    public ParticipantsLabelProvider() {

    }

    @Override
    public void dispose() {
        super.dispose();
        if (colors != null) {
            for (Color col : colors.values()) {
                col.dispose();
            }
            colors.clear();
        }

        if (boldFont != null && !boldFont.isDisposed()) {
            boldFont.dispose();
            boldFont = null;
        }
        if (underlinedFont != null && !underlinedFont.isDisposed()) {
            underlinedFont.dispose();
            underlinedFont = null;
        }
        if (combinedFont != null && !combinedFont.isDisposed()) {
            combinedFont.dispose();
            combinedFont = null;
        }
    }

    @Override
    public Font getFont(Object element) {
        if (!(element instanceof UserId)) {
            return null;
        }
        UserId user = (UserId) element;
        boolean leader = isSessionLeader(user);
        boolean provider = isDataProvider(user);
        if (leader && provider) {
            if (combinedFont == null) {
                Font currFont = Display.getCurrent().getSystemFont();
                combinedFont = new Font(Display.getCurrent(),
                        currFont.toString(),
                        currFont.getFontData()[0].getHeight(), SWT.BOLD
                                | SWT.ITALIC);
            }
            return combinedFont;
        } else if (leader) {
            if (boldFont == null) {
                Font currFont = Display.getCurrent().getSystemFont();
                boldFont = new Font(Display.getCurrent(), currFont.toString(),
                        currFont.getFontData()[0].getHeight(), SWT.BOLD);
            }
            return boldFont;
        } else if (provider) {
            if (underlinedFont == null) {
                Font currFont = Display.getCurrent().getSystemFont();
                underlinedFont = new Font(Display.getCurrent(),
                        currFont.toString(),
                        currFont.getFontData()[0].getHeight(), SWT.ITALIC);
            }
            return underlinedFont;
        }
        return null;
    }

    @Override
    public Color getForeground(Object element) {
        if (!(element instanceof UserId)) {
            return null;
        }
        UserId user = ((UserId) element);
        RGB rgb = manager.getColorFromUser(user);
        if (rgb == null) {
            rgb = new RGB(0, 0, 0);
        }
        Color color = colors.get(rgb);
        if (color == null) {
            color = new Color(Display.getCurrent(), rgb);
            colors.put(rgb, color);
        }
        return color;
    }

    /**
     * @param sessionId
     *            the sessionId to set
     */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    protected ISession getSession() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return null;
        }
        return connection.getSession(sessionId);
    }

    protected boolean isSessionLeader(UserId user) {
        ISession session = getSession();
        if (session instanceof SharedDisplaySession) {
            UserId leader = ((SharedDisplaySession) session)
                    .getCurrentSessionLeader();
            return user.equals(leader);
        }
        return false;
    }

    protected boolean isDataProvider(UserId user) {
        ISession session = getSession();
        if (session instanceof SharedDisplaySession) {
            UserId provider = ((SharedDisplaySession) session)
                    .getCurrentDataProvider();
            return user.equals(provider);
        }
        return false;
    }

    @Override
    public String getToolTipText(Object element) {
        String toolTip = super.getToolTipText(element);
        if (toolTip == null) {
            return null;
        }
        StringBuilder builder = new StringBuilder(toolTip);
        UserId user = (UserId) element;
        Presence presence = getPresence(user);
        if (presence != null) {
            String site = String.valueOf(presence
                    .getProperty(SiteConfigInformation.SITE_NAME));
            if (enabledSites != null && enabledSites.contains(site)) {
                builder.append("\n").append("Subscribed");
            }
        }
        ISession session = getSession();
        if (session instanceof SharedDisplaySession) {
            boolean isSessionLeader = user
                    .equals(((SharedDisplaySession) session)
                            .getCurrentSessionLeader());
            boolean isDataProvider = user
                    .equals(((SharedDisplaySession) session)
                            .getCurrentDataProvider());
            if (isSessionLeader || isDataProvider) {
                // TODO if transferring control is ever desired and implemented
                // we need to distinguish these. Until then, Leader works fine.
                builder.append("\nLeader");
                // if (isSessionLeader) {
                // builder.append("\nSession Leader");
                // }
                // if (isDataProvider) {
                // builder.append("\nData Provider");
                // }
            }
        }
        return builder.toString();
    }

    protected void setEnabledSites(List<String> enabledSites) {
        this.enabledSites = enabledSites;
    }

    /**
     * @param manager
     *            the manager to set
     */
    public void setManager(SessionColorManager manager) {
        this.manager = manager;
    }

    @Override
    protected Presence getPresence(UserId user) {
        IVenueSession session = (IVenueSession) getSession();
        return session.getVenue().getPresence(user);
    }
}
