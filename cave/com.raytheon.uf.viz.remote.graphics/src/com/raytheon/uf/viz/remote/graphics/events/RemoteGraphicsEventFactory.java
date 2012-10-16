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
package com.raytheon.uf.viz.remote.graphics.events;

import com.raytheon.uf.viz.remote.graphics.DispatchingObject;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RemoteGraphicsEventFactory {

    public static <T extends AbstractRemoteGraphicsEvent> T createEvent(
            Class<? extends T> eventType, DispatchingObject<?> creatingObject) {
        T event = null;
        try {
            event = eventType.newInstance();
            event.setDisplayId(creatingObject.getDispatcher().getDispatcherId());
            if (AbstractDispatchingObjectEvent.class
                    .isAssignableFrom(eventType)) {
                AbstractDispatchingObjectEvent.class.cast(event).setObjectId(
                        creatingObject.getObjectId());
            }
        } catch (Throwable t) {
            // TODO: Handle gracefully
        }
        return event;
    }
}
