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

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;

import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.DataUser.RoleType;

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

    private String sessionId = null;

    @Override
    public void addListener(ILabelProviderListener listener) {
        // TODO Auto-generated method stub

    }

    @Override
    public void dispose() {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean isLabelProperty(Object element, String property) {
        // TODO Auto-generated method stub
        System.out.println("isLabelProperty");
        return false;
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        // TODO Auto-generated method stub

    }

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
        System.out.println("getColumnImage");
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getColumnText(Object element, int columnIndex) {
        CollaborationUser user = (CollaborationUser) element;
        RoleType[] roles = user.getRoles(sessionId);
        return user.getText();
    }

    @Override
    public Font getFont(Object element, int columnIndex) {
        System.out.println("getFont");
        return null;
    }

    @Override
    public Color getForeground(Object element, int columnIndex) {
        CollaborationDataManager.getInstance().getUser(element.toString());
        System.out.println("getForeground");
        return null;
    }

    @Override
    public Color getBackground(Object element, int columnIndex) {
        System.out.println("getBackground");
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
    // usersList.setLabelProvider(new LabelProvider() {
    // public String getText(Object element) {
    // CollaborationUser user = (CollaborationUser) element;
    // DataUser.RoleType[] roles = user.getRoles(sessionId);
    // StringBuilder sb = new StringBuilder();
    // if (roles.length > 0
    // && roles[0] != DataUser.RoleType.PARTICIPANT) {
    // sb.append("[");
    // for (DataUser.RoleType r : roles) {
    // switch (r) {
    // case DATA_PROVIDER:
    // sb.append("D");
    // break;
    // case LEADER:
    // sb.append("L");
    // break;
    // default:
    // sb.append("?");
    // break;
    // }
    // }
    // sb.append("] - ");
    // }
    // sb.append(user.getId());
    // return sb.toString();
    // }
    //
    // public Image getImage(Object element) {
    // Image image = null;
    // if (element instanceof CollaborationNode) {
    // CollaborationNode node = (CollaborationNode) element;
    // String key = node.getImageKey();
    // if (key != null) {
    // image = imageMap.get(key);
    // if (image == null) {
    // image = CollaborationUtils.getNodeImage(node);
    // imageMap.put(key, image);
    // }
    // }
    // }
    // return image;
    // }
    // });

}
