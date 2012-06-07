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
package com.raytheon.uf.viz.collaboration.ui.telestrator.event;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class CollaborationDrawingEvent {

    public static enum CollaborationEventType {
        DRAW, ERASE, REDO, UNDO, CLEAR, TOGGLE_LOCK, CLEAR_ALL;
    }

    @DynamicSerializeElement
    private List<Coordinate> coordinates;

    @DynamicSerializeElement
    private UserId userName;

    @DynamicSerializeElement
    private CollaborationEventType type;

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
    public UserId getUserName() {
        return userName;
    }

    /**
     * @param userName
     *            the userName to set
     */
    public void setUserName(UserId userName) {
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