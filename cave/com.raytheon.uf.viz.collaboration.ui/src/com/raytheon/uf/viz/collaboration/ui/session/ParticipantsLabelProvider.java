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

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;

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

public class ParticipantsLabelProvider implements ITableColorProvider,
        ITableFontProvider, ITableLabelProvider {

    private List<ILabelProviderListener> listeners;

    protected String sessionId = null;

    protected Map<String, Image> imageMap;

    protected Map<UserId, Color> colors;

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
    public Image getColumnImage(Object element, int columnIndex) {
        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);

        if (session == null) {
            return null;
        }

        CollaborationUser user = (CollaborationUser) element;
        Image image = null;
        String key = user.getImageKey();
        if (key != null) {
            image = imageMap.get(key);
            if (image == null) {
                image = CollaborationUtils.getNodeImage(user);
                if (image != null) {
                    imageMap.put(key, image);
                }
            }
        }

        return image;
    }

    @Override
    public String getColumnText(Object element, int columnIndex) {
        CollaborationUser user = (CollaborationUser) element;
        return user.getText();
    }

    @Override
    public Font getFont(Object element, int columnIndex) {
        return null;
    }

    @Override
    public Color getForeground(Object element, int columnIndex) {
        if (colors == null) {
            colors = new HashMap<UserId, Color>();
        }
        // String host = ((CollaborationUser) element).
        String id = ((CollaborationUser) element).getId();
        String[] uid = null;
        if (id != null) {
            uid = ((CollaborationUser) element).getId().split("@");
        }
        UserId userId = new UserId(uid[0], uid[1]);
        RGB color = SharedDisplaySessionMgr.getSessionContainer(sessionId)
                .getColorManager().getColors().get(userId);
        if (color == null) {
            userId.setHost("conference." + uid[1]);
            color = SharedDisplaySessionMgr.getSessionContainer(sessionId)
                    .getColorManager().getColors().get(userId);
        }

        // add to map so we can dispose
        if (color == null) {
            colors.put(userId, new Color(Display.getCurrent(), 0, 0, 0));
        } else {
            colors.put(userId, new Color(Display.getCurrent(), color));
        }
        return colors.get(userId);
    }

    @Override
    public Color getBackground(Object element, int columnIndex) {
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
}
