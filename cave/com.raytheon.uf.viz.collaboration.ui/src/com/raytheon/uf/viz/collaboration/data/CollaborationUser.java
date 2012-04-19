package com.raytheon.uf.viz.collaboration.data;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

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
public class CollaborationUser extends CollaborationNode {

    String session;

    UserId iChatID;

    public CollaborationUser(String id) {
        super(id);
        this.session = null;
        // this.roles = new ArrayList<DataUser.RoleType>();
        // this.status = DataUser.StatusType.NOT_ON_LINE;
        // this.statusMessage = "";
    }

    public CollaborationUser(String id, String session) {
        super(id);
        this.session = session;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.data.CollaborationNode#getImageKey()
     */
    @Override
    public String getImageKey() {
        if (getType() == Type.AVAILABLE) {
            return getMode().toString();
        }
        return "contact_disabled";
    }

    public IPresence.Mode getMode() {
        return CollaborationDataManager.getInstance().getUser(id).mode;
    }

    public IPresence.Type getType() {
        return CollaborationDataManager.getInstance().getUser(id).type;
    }

    public void setPresence(IPresence presence) {
        CollaborationDataManager.getInstance().getUser(id)
                .setPresence(presence);
    }

    // public void setMode(IPresence.Mode mode) {
    // CollaborationDataManager.getInstance().getUser(id).mode = mode;
    // }
    //
    // public void setType(IPresence.Type type) {
    // CollaborationDataManager.getInstance().getUser(id).type = type;
    // }

    // public void setStatus(IPresence.Mode mode) {
    // if (mode.getMode().equals(Mode.AWAY)) {
    // CollaborationDataManager.getInstance().getUser(id).mode =
    // StatusType.AWAY;
    // } else if (mode.getMode().equals(Mode.DND)) {
    // CollaborationDataManager.getInstance().getUser(id).mode =
    // StatusType.DO_NOT_DISTURB;
    // } else if (mode.getMode().equals(Mode.AVAILABLE)) {
    // CollaborationDataManager.getInstance().getUser(id).mode =
    // StatusType.AVAILABLE;
    // }
    // }

    public String getStatusMessage() {
        return CollaborationDataManager.getInstance().getUser(id).statusMessage;
    }

    // public void setStatusMessage(String statusMessage) {
    // CollaborationDataManager.getInstance().getUser(id).statusMessage =
    // statusMessage;
    // }
}
