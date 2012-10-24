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
package com.raytheon.uf.viz.collaboration.core.ui.tree;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;

import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.core.ui.data.ActiveSessionsTreeData;
import com.raytheon.uf.viz.collaboration.core.ui.data.MyUserTreeData;
import com.raytheon.uf.viz.collaboration.core.ui.data.UserGroupTreeData;
import com.raytheon.uf.viz.collaboration.core.ui.data.UserTreeData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class UsersTreeLabelProvider extends ColumnLabelProvider {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#getImage(java.lang.Object)
     */
    @Override
    public Image getImage(Object element) {
        Image image = null;
        if (element instanceof UserTreeData) {

        } else if (element instanceof MyUserTreeData) {

        } else if (element instanceof UserGroupTreeData) {

        } else if (element instanceof ActiveSessionsTreeData) {

        }
        return image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText(Object element) {
        String text = null;
        if (element instanceof UserTreeData) {
            text = formatUserId(((UserTreeData) element).getUser());
        } else if (element instanceof MyUserTreeData) {
            text = formatUserId(((UserTreeData) element).getUser())
                    + " - "
                    + CollaborationConnection.getConnection()
                            .getConnectionData().getServer();
        } else if (element instanceof UserGroupTreeData) {
            text = ((UserGroupTreeData) element).getGroupName();
        } else if (element instanceof ActiveSessionsTreeData) {
            text = "Active Sessions";
        }
        return text;
    }

    public static String formatUserId(UserId user) {
        Object site = user.getProperties().get(SiteConfigInformation.SITE_NAME);
        Object role = user.getProperties().get(SiteConfigInformation.ROLE_NAME);
        String name = (user.getAlias() != null ? user.getAlias() : user
                .getName());
        if (site != null) {
            name += " - " + site;
        }
        if (role != null) {
            name += " - " + role;
        }
        return name;
    }
}
