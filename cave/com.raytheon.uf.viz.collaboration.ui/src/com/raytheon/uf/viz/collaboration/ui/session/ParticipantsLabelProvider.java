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
import java.util.Arrays;
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
import org.osgi.framework.Bundle;

import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * TODO Add Description
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
        // TODO Auto-generated method stub
        System.out.println("isLabelProperty");
        return false;
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
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
        // TODO Determine user's role and then test getModifier.
        if (image != null) {
            ParticipantRole[] types = new ParticipantRole[] {
                    ParticipantRole.SESSION_LEADER,
                    ParticipantRole.DATA_PROVIDER };
            image = getModifier(types, user);
        }
        return image;
    }

    @Override
    public String getColumnText(Object element, int columnIndex) {
        CollaborationUser user = (CollaborationUser) element;
        // RoleType[] roles = user.getRoles(sessionId);
        return user.getText();
    }

    @Override
    public Font getFont(Object element, int columnIndex) {
        // System.out.println("getFont");
        return null;
    }

    @Override
    public Color getForeground(Object element, int columnIndex) {
        // CollaborationUser user = (CollaborationUser) element;
        // System.out.println("getForeground");
        return null;
    }

    @Override
    public Color getBackground(Object element, int columnIndex) {
        // System.out.println("getBackground");
        // TODO Auto-generated method stub
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

    private Image getModifier(ParticipantRole[] types, CollaborationUser user) {
        String key = user.getImageKey();
        StringBuilder modKey = new StringBuilder(key);
        List<ParticipantRole> t = Arrays.asList(types);
        if (t.contains(ParticipantRole.SESSION_LEADER)) {
            modKey.append(":")
                    .append(ParticipantRole.SESSION_LEADER.toString());
        }
        if (t.contains(ParticipantRole.DATA_PROVIDER)) {
            modKey.append(":").append(ParticipantRole.DATA_PROVIDER.toString());
        }
        Image image = imageMap.get(modKey.toString());

        if (image == null) {
            Bundle bundle = Activator.getDefault().getBundle();
            image = CollaborationUtils.getNodeImage(user);
            // original image is 16x16
            GC gc = new GC(image, SWT.LEFT_TO_RIGHT);

            if (t.contains(ParticipantRole.SESSION_LEADER)) {
                Image im = IconUtil.getImageDescriptor(bundle,
                        "session_leader.png").createImage();
                gc.drawImage(im, 7, 7);
                im.dispose();
            }
            if (t.contains(ParticipantRole.DATA_PROVIDER)) {
                Image im = IconUtil.getImageDescriptor(bundle,
                        "data_provider.png").createImage();
                gc.drawImage(im, 0, 16);
                im.dispose();
            }
            image.getImageData();
            imageMap.put(modKey.toString(), image);
            gc.dispose();
        }
        return image;
    }
}
