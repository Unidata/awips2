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
package com.raytheon.uf.viz.collaboration.comm.identity.invite;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;

/**
 * An invite message for inviting another user to a ISharedDisplaySession.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class SharedDisplayInvite {

    @DynamicSerializeElement
    private String subject;

    @DynamicSerializeElement
    private String message;

    @DynamicSerializeElement
    private IVenueParticipant sessionLeader;

    @DynamicSerializeElement
    private IVenueParticipant dataProvider;

    @DynamicSerializeElement
    private String sessionId;

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public IVenueParticipant getSessionLeader() {
        return sessionLeader;
    }

    public void setSessionLeader(IVenueParticipant sessionLeader) {
        this.sessionLeader = sessionLeader;
    }

    public IVenueParticipant getDataProvider() {
        return dataProvider;
    }

    public void setDataProvider(IVenueParticipant dataProvider) {
        this.dataProvider = dataProvider;
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

}
