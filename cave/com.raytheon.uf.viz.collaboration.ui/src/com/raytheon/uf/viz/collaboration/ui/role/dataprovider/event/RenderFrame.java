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
package com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;

/**
 * RenderFrame object, tracks IRenderEvents and dispatches changes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RenderFrame extends DispatchingObject<Object> {

    private List<IRenderEvent> renderEvents = new LinkedList<IRenderEvent>();

    /**
     * @param targetObject
     * @param dispatcher
     */
    public RenderFrame(Dispatcher dispatcher) {
        super(null, dispatcher);
    }

    public void addEvent(IRenderEvent event) {
        renderEvents.add(event);
    }

    public boolean merge(RenderFrame existingFrame) {
        if (existingFrame.renderEvents.size() != renderEvents.size()) {
            return false;
        }

        UpdateRenderFrameEvent updateEvent = RemoteGraphicsEventFactory
                .createEvent(UpdateRenderFrameEvent.class, this);
        Iterator<IRenderEvent> myIter = renderEvents.iterator();
        Iterator<IRenderEvent> newIter = existingFrame.renderEvents.iterator();
        boolean changes = false;
        while (myIter.hasNext() && newIter.hasNext()) {
            IRenderEvent myEvent = myIter.next();
            IRenderEvent newEvent = newIter.next();
            IRenderEvent update = null;
            if (myEvent.getClass().equals(newEvent.getClass())) {
                // Same type, check equality
                if (myEvent.equals(newEvent) == false) {
                    // Not equal, create diff object
                    update = myEvent.createDiffObject(newEvent);
                    changes = true;
                }
            } else {
                // Not compatible, full replace
                update = newEvent;
                changes = true;
            }
            updateEvent.addEvent(update);
        }

        renderEvents.clear();
        renderEvents.addAll(existingFrame.renderEvents);

        if (changes == false) {
            updateEvent.getRenderEvents().clear();
        }
        // Send update event
        dispatch(updateEvent);
        return true;
    }

    public void sendFrame() {
        RenderFrameEvent event = RemoteGraphicsEventFactory.createEvent(
                RenderFrameEvent.class, this);
        event.setRenderEvents(new ArrayList<IRenderEvent>(renderEvents));
        dispatch(event);
    }

    public void dispose() {
        dispatch(RemoteGraphicsEventFactory.createEvent(FrameDisposed.class,
                this));
    }

    public void clear() {
        renderEvents.clear();
    }

    public int size() {
        return renderEvents.size();
    }
}
