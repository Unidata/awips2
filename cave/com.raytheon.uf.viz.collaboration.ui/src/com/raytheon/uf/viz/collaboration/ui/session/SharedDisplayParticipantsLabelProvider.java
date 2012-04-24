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
import java.util.List;

import org.eclipse.swt.graphics.Image;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;

/**
 * Generate the labels and images for a shared display session
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SharedDisplayParticipantsLabelProvider extends
        ParticipantsLabelProvider {

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
        Image image = super.getColumnImage(element, columnIndex);
        if (image != null) {
            ISharedDisplaySession sdSession = SharedDisplaySessionMgr
                    .getSessionContainer(sessionId).getSession();
            IRosterEntry user = (IRosterEntry) element;
            UserId userId = user.getUser();
            UserId sessionLeaderId = sdSession.getCurrentSessionLeader();
            UserId dataProviderId = sdSession.getCurrentDataProvider();
            List<SharedDisplayRole> roleList = new ArrayList<SharedDisplayRole>();
            if (userId.equals(sessionLeaderId)) {
                roleList.add(SharedDisplayRole.SESSION_LEADER);
            }
            if (userId.equals(dataProviderId)) {
                roleList.add(SharedDisplayRole.DATA_PROVIDER);
            }
            if (roleList.size() > 0) {
                image = getModifier(roleList, user);
            }
        }
        return image;

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
    private Image getModifier(List<SharedDisplayRole> roles, IRosterEntry user) {
        // String key = user.getImageKey();
        String key = "";
        StringBuilder modKey = new StringBuilder(key);
        int roleCnt = 0;
        if (roles.contains(SharedDisplayRole.SESSION_LEADER)) {
            ++roleCnt;
            modKey.append(":").append(
                    SharedDisplayRole.SESSION_LEADER.toString());
        }
        if (roles.contains(SharedDisplayRole.DATA_PROVIDER)) {
            ++roleCnt;
            modKey.append(":").append(
                    SharedDisplayRole.DATA_PROVIDER.toString());
        }
        Image image = imageMap.get(modKey.toString());

        if (image == null) {
            image = CollaborationUtils.getNodeImage(key);
            // original image is 16x16
            image.getImageData();
            imageMap.put(modKey.toString(), image);
        }
        return image;
    }

}
