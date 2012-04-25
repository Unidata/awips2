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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.provider.session.SharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.SessionColorManager;

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
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ParticipantsLabelProvider extends ColumnLabelProvider {

    private List<ILabelProviderListener> listeners;

    protected String sessionId = null;

    protected Map<String, Image> imageMap;

    protected Map<UserId, Color> colors;

    private SessionColorManager manager;

    private Font boldFont;

    public ParticipantsLabelProvider() {
        listeners = new ArrayList<ILabelProviderListener>();
        imageMap = new HashMap<String, Image>();
    }

    @Override
    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    @Override
    public void dispose() {
        for (String key : imageMap.keySet()) {
            imageMap.get(key).dispose();
        }

        if (colors != null) {
            for (Color col : colors.values()) {
                col.dispose();
            }
        }
    }

    @Override
    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    @Override
    public Image getImage(Object element) {
        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);

        if (session == null) {
            return null;
        }

        IRosterEntry user = (IRosterEntry) element;
        Image image = null;
        String key = user.getPresence().getMode().toString();
        if (key != null) {
            image = imageMap.get(key);
            if (image == null) {
                image = CollaborationUtils.getNodeImage(key);
                if (image != null) {
                    imageMap.put(key, image);
                }
            }
        }
        return getImageModifier(session, element, image);
    }

    private Image getImageModifier(IVenueSession session, Object element,
            Image image) {
        if (session instanceof SharedDisplaySession) {
            boolean isSessionLeader = ((IRosterEntry) element).getUser()
                    .equals(((SharedDisplaySession) session)
                            .getCurrentSessionLeader());
            boolean isDataProvider = ((IRosterEntry) element).getUser().equals(
                    ((SharedDisplaySession) session).getCurrentDataProvider());
            if (isSessionLeader) {
                drawSessionLeader(image);
            }
            if (isDataProvider) {
                drawDataProvider(image);
            }
        }
        return image;
    }

    private void drawSessionLeader(Image image) {
        // TODO draw session leader here
        // image = new Image(Display.getCurrent(), image.getBounds());
        ImageData data = image.getImageData();
        int whitePixel = data.palette.getPixel(new RGB(0, 255, 255));
        int redPixel = data.palette.getPixel(new RGB(255, 0, 0));
        data.transparentPixel = whitePixel;
        for (int i = 0; i < data.data.length; i++) {
            if (data.data[i] == -1) {
                data.data[i] = (byte) redPixel;
            }
        }
        image = new Image(Display.getCurrent(), data);
        // for (int i = 0; i < data.data.length; i++) {
        // if (data.data[i] == -1) {
        // data.data[i] = 0;
        // }
        // }
        // image = new Image(Display.getCurrent(), data);
        // GC gc = new GC(image);
        // Rectangle rect = image.getBounds();
        // image.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_RED));
        // // gc.drawImage(image, 0, 0);
        // gc.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
        // gc.fillRectangle(5, 5, rect.width - 3, rect.height - 3);
        // gc.dispose();
    }

    private void drawDataProvider(Image image) {
        // TODO draw data provider here
    }

    @Override
    public String getText(Object element) {
        IRosterEntry user = (IRosterEntry) element;
        if (user.getUser().getAlias() != null
                && !user.getUser().getAlias().isEmpty()) {
            return user.getUser().getAlias();
        }
        String name = user.getUser().getName();
        return name;
    }

    @Override
    public Font getFont(Object element) {
        if (boldFont == null) {
            Font currFont = Display.getCurrent().getSystemFont();
            boldFont = new Font(Display.getCurrent(), currFont.toString(),
                    currFont.getFontData()[0].getHeight(), SWT.BOLD);
        }
        return boldFont;
    }

    @Override
    public Color getForeground(Object element) {
        if (colors == null) {
            colors = new HashMap<UserId, Color>();
        }
        UserId userId = ((IRosterEntry) element).getUser();
        RGB color = manager.getColorFromUser(userId);

        // add to map so we can dispose
        if (color == null) {
            colors.put(userId, new Color(Display.getCurrent(), 0, 0, 0));
        } else {
            colors.put(userId, new Color(Display.getCurrent(), color));
        }
        return colors.get(userId);
    }

    @Override
    public Color getBackground(Object element) {
        return null;
    }

    /**
     * @param sessionId
     *            the sessionId to set
     */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /**
     * @return the sessionId
     */
    public String getSessionId() {
        return sessionId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.CellLabelProvider#getToolTipText(java.lang.
     * Object)
     */
    @Override
    public String getToolTipText(Object element) {
        IRosterEntry entry = (IRosterEntry) element;
        return buildParticipantTooltip(entry);
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

    protected String buildParticipantTooltip(IRosterEntry user) {
        StringBuilder builder = new StringBuilder();
        builder.append("Status : ")
                .append(user.getPresence().getMode().getMode()).append("\n");
        builder.append("Message : \"")
                .append(user.getPresence().getStatusMessage()).append("\"");
        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);
        if (session instanceof SharedDisplaySession) {
            boolean isSessionLeader = user.getUser().equals(
                    ((SharedDisplaySession) session).getCurrentSessionLeader());
            boolean isDataProvider = user.getUser().equals(
                    ((SharedDisplaySession) session).getCurrentDataProvider());
            if (isSessionLeader || isDataProvider) {
                if (isSessionLeader) {
                    builder.append("\nSession Leader");
                }
                if (isDataProvider) {
                    builder.append("\nData Provider");
                }
            }
        }
        return builder.toString();
    }

    /**
     * @param manager
     *            the manager to set
     */
    public void setManager(SessionColorManager manager) {
        this.manager = manager;
    }
}
