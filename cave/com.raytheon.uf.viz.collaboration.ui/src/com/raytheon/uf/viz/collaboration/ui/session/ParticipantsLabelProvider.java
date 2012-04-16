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
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
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

    private static int COLOR_SESSION_LEADER = SWT.COLOR_GREEN;

    private static int COLOR_DATA_PROVIDER = SWT.COLOR_RED;

    private List<ILabelProviderListener> listeners;

    private String sessionId = null;

    private Map<String, Image> imageMap;

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

        if (image != null) {
            ISharedDisplaySession sdSession = session
                    .spawnSharedDisplaySession();
            String userId = user.getId();
            String sessionLeaderId = CollaborationUtils.makeUserId(sdSession
                    .getCurrentSessionLeader());
            String dataProviderId = CollaborationUtils.makeUserId(sdSession
                    .getCurrentDataProvider());
            List<ParticipantRole> roleList = new ArrayList<ParticipantRole>();
            if (userId.equals(sessionLeaderId)) {
                roleList.add(ParticipantRole.SESSION_LEADER);
            }
            if (userId.equals(dataProviderId)) {
                roleList.add(ParticipantRole.DATA_PROVIDER);
            }
            if (roleList.size() > 0) {
                image = getModifier(roleList, user);
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
        return null;
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

    /**
     * Modify image image to indicate Session Leader and/or DataProvider.
     * 
     * @param roles
     *            - non-empty list indicate the role(s)
     * @param user
     *            -
     * @return image - modified with indicator(s)
     */
    private Image getModifier(List<ParticipantRole> roles,
            CollaborationUser user) {
        String key = user.getImageKey();
        StringBuilder modKey = new StringBuilder(key);
        int roleCnt = 0;
        if (roles.contains(ParticipantRole.SESSION_LEADER)) {
            ++roleCnt;
            modKey.append(":")
                    .append(ParticipantRole.SESSION_LEADER.toString());
        }
        if (roles.contains(ParticipantRole.DATA_PROVIDER)) {
            ++roleCnt;
            modKey.append(":").append(ParticipantRole.DATA_PROVIDER.toString());
        }
        Image image = imageMap.get(modKey.toString());

        if (image == null) {
            image = CollaborationUtils.getNodeImage(user);
            // original image is 16x16
            GC gc = new GC(image, SWT.LEFT_TO_RIGHT);
            int topColor = -1;
            int bottomColor = -1;
            if (roleCnt == 1) {
                if (roles.contains(ParticipantRole.SESSION_LEADER)) {
                    topColor = COLOR_SESSION_LEADER;
                    bottomColor = COLOR_SESSION_LEADER;
                } else {
                    topColor = COLOR_DATA_PROVIDER;
                    bottomColor = COLOR_DATA_PROVIDER;
                }
            } else {
                topColor = COLOR_SESSION_LEADER;
                bottomColor = COLOR_DATA_PROVIDER;
            }
            gc.setBackground(Display.getCurrent().getSystemColor(topColor));
            gc.fillRectangle(0, 0, 16, 8);
            gc.setBackground(Display.getCurrent().getSystemColor(bottomColor));
            gc.fillRectangle(0, 8, 8, 8);
            gc.dispose();
            image.getImageData();
            imageMap.put(modKey.toString(), image);
        }
        return image;
    }
}
