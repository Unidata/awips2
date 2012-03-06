package com.raytheon.uf.viz.collaboration.data;

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
public class CollaborationUser extends CollaborationNode implements
        Comparable<CollaborationUser> {

    String session;

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
        return getStatus().toString();
    }

    public DataUser.RoleType[] getRoles(String session) {
        return CollaborationDataManager.getInstance().getUser(id)
                .getSessionRoles(session);
    }

    public void addRole(DataUser.RoleType role) {
        CollaborationDataManager.getInstance().getUser(id)
                .addSessionRole(session, role);
    }

    public void removeRole(DataUser.RoleType role) {
        CollaborationDataManager.getInstance().getUser(id)
                .removeSessionRole(session, role);
    }

    public void removeSession(String session) {
        CollaborationDataManager.getInstance().getUser(id)
                .removeSession(session);
    }

    public DataUser.StatusType getStatus() {
        return CollaborationDataManager.getInstance().getUser(id).status;
    }

    public void setStatus(DataUser.StatusType status) {
        CollaborationDataManager.getInstance().getUser(id).status = status;
    }

    public String getStatusMessage() {
        return CollaborationDataManager.getInstance().getUser(id).statusMessage;
    }

    public void setStatusMessage(String statusMessage) {
        CollaborationDataManager.getInstance().getUser(id).statusMessage = statusMessage;
    }

    @Override
    public int compareTo(CollaborationUser o) {
        return getId().compareToIgnoreCase(o.getId());
    }

}
