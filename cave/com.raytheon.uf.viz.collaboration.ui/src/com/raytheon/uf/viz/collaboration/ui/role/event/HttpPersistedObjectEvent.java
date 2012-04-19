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
package com.raytheon.uf.viz.collaboration.ui.role.event;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class HttpPersistedObjectEvent extends PersistedObjectEvent {

    private static long eventIdCounter = 0;

    public static synchronized HttpPersistedObjectEvent createNewObject(
            String sessionId) {
        HttpPersistedObjectEvent event = new HttpPersistedObjectEvent();
        event.sessionId = sessionId;
        event.eventId = ++eventIdCounter;
        return event;
    }

    @DynamicSerializeElement
    private String sessionId;

    @DynamicSerializeElement
    private int objectId;

    @DynamicSerializeElement
    private long eventId;

    @DynamicSerializeElement
    private AbstractRemoteGraphicsEvent event;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IPersistedObjectEvent
     * #store(com.raytheon.uf.viz.remote.graphics.AbstractRemoteGraphicsEvent)
     */
    @Override
    public void store(AbstractRemoteGraphicsEvent event) {
        this.event = event;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IPersistedObjectEvent
     * #retrieve()
     */
    @Override
    public AbstractRemoteGraphicsEvent retrieve() {
        return event;
    }

    /**
     * @return the sessionId
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * @param sessionId
     *            the sessionId to set
     */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /**
     * @return the objectId
     */
    public int getObjectId() {
        return objectId;
    }

    /**
     * @param objectId
     *            the objectId to set
     */
    public void setObjectId(int objectId) {
        this.objectId = objectId;
    }

    /**
     * @return the eventId
     */
    public long getEventId() {
        return eventId;
    }

    /**
     * @param eventId
     *            the eventId to set
     */
    public void setEventId(long eventId) {
        this.eventId = eventId;
    }

    /**
     * @return the event
     */
    public AbstractRemoteGraphicsEvent getEvent() {
        return event;
    }

    /**
     * @param event
     *            the event to set
     */
    public void setEvent(AbstractRemoteGraphicsEvent event) {
        this.event = event;
    }

}
