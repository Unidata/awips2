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
package com.raytheon.uf.viz.collaboration.display.rsc.telestrator;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Event posted when a venue participant draws on shared display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * May 05, 2014 3076       bclement    added DISPOSE_ALL
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class CollaborationDrawingEvent {

    public static enum CollaborationEventType {
        DRAW, ERASE, REDO, UNDO, CLEAR, CLEAR_ALL, LOCK_USERS, UNLOCK_USERS, DISPOSE_ALL, NEW_USER_ARRIVED;
    }

    @DynamicSerializeElement
    private int displayId;

    @DynamicSerializeElement
    private List<Coordinate> coordinates;

    @DynamicSerializeElement
    private VenueParticipant userName;

    @DynamicSerializeElement
    private CollaborationEventType type;

    public CollaborationDrawingEvent() {

    }

    public CollaborationDrawingEvent(int displayId) {
        this.displayId = displayId;
    }

    /**
     * @return the displayId
     */
    public int getDisplayId() {
        return displayId;
    }

    /**
     * @param displayId
     *            the displayId to set
     */
    public void setDisplayId(int displayId) {
        this.displayId = displayId;
    }

    /**
     * @return the coordinates
     */
    public List<Coordinate> getCoordinates() {
        return coordinates;
    }

    /**
     * @param coordinates
     *            the coordinates to set
     */
    public void setCoordinates(List<Coordinate> coordinates) {
        this.coordinates = coordinates;
    }

    /**
     * @return the userName
     */
    public VenueParticipant getUserName() {
        return userName;
    }

    /**
     * @param userName
     *            the userName to set
     */
    public void setUserName(VenueParticipant userName) {
        this.userName = userName;
    }

    /**
     * @return the type
     */
    public CollaborationEventType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(CollaborationEventType type) {
        this.type = type;
    }
}