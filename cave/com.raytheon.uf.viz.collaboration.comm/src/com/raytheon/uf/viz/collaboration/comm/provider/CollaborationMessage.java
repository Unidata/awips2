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
package com.raytheon.uf.viz.collaboration.comm.provider;

import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class CollaborationMessage extends BaseMessage {

    private static final long serialVersionUID = 1L;

    /**
     * 
     * @param to
     * @param body
     */
    public CollaborationMessage(IQualifiedID to, String body) {
        super(to, body);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getMessageType()
     */
    @Override
    public MessageType getMessageType() {
        return MessageType.COLLABORATION;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IMessage#getBodyAsBinary(byte[])
     */
    @Override
    public void getBodyAsBinary(byte[] body) {
    }

}
