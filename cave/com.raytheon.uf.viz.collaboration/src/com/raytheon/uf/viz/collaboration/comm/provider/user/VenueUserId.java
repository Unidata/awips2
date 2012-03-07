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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class VenueUserId extends UserId implements IChatID {

    private String nickName;
    
    /**
     * 
     * @param userName
     * @param hostName
     */
    public VenueUserId(String userName, String hostName) {
        super(userName, hostName);
        nickName = null;
    }

    /**
     * 
     * @param userName
     * @param hostName
     */
    public VenueUserId(String userName, String nickName, String hostName) {
        super(userName, hostName);
        this.nickName = nickName;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID#setNickname(java.lang.String)
     */
    @Override
    public void setNickname(String nickname) {
        this.nickName = nickname;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID#getNickname()
     */
    @Override
    public String getNickname() {
        return nickName;
    }

}
