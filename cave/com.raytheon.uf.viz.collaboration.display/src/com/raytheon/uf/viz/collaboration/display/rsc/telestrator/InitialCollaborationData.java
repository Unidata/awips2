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

import java.util.Collection;
import java.util.Stack;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Holds all drawing data for a user, which is sent to new user upon joining a
 * telestrator session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2012            dgilling     Initial creation
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class InitialCollaborationData {

    @DynamicSerializeElement
    private int displayId;

    @DynamicSerializeElement
    private VenueParticipant userName;

    @DynamicSerializeElement
    private boolean sessionLeader;

    @DynamicSerializeElement
    private boolean drawingLocked;

    @DynamicSerializeElement
    private Collection<Geometry> currrentData;

    @DynamicSerializeElement
    private Stack<Collection<Geometry>> redoData;

    @DynamicSerializeElement
    private Stack<Collection<Geometry>> undoData;

    /**
     * It is recommended that this constructor only be used by dynamic
     * serialize.
     */
    public InitialCollaborationData() {

    }

    public InitialCollaborationData(VenueParticipant userName, int displayId,
            boolean sessionLeader, boolean drawingLocked,
            CollaborationDrawingToolLayer layer) {
        this.userName = userName;
        this.displayId = displayId;
        this.sessionLeader = sessionLeader;
        this.drawingLocked = drawingLocked;
        this.currrentData = layer.getCurrentData();
        this.undoData = layer.getUndoStack();
        this.redoData = layer.getRedoStack();
    }

    public VenueParticipant getUserName() {
        return userName;
    }

    public void setUserName(VenueParticipant userName) {
        this.userName = userName;
    }

    public Collection<Geometry> getCurrrentData() {
        return currrentData;
    }

    public void setCurrrentData(Collection<Geometry> currrentData) {
        this.currrentData = currrentData;
    }

    public Stack<Collection<Geometry>> getRedoData() {
        return redoData;
    }

    public void setRedoData(Stack<Collection<Geometry>> redoData) {
        this.redoData = redoData;
    }

    public Stack<Collection<Geometry>> getUndoData() {
        return undoData;
    }

    public void setUndoData(Stack<Collection<Geometry>> undoData) {
        this.undoData = undoData;
    }

    public int getDisplayId() {
        return displayId;
    }

    public void setDisplayId(int displayId) {
        this.displayId = displayId;
    }

    public boolean isSessionLeader() {
        return sessionLeader;
    }

    public void setSessionLeader(boolean sessionLeader) {
        this.sessionLeader = sessionLeader;
    }

    public boolean isDrawingLocked() {
        return drawingLocked;
    }

    public void setDrawingLocked(boolean drawingLocked) {
        this.drawingLocked = drawingLocked;
    }
}
