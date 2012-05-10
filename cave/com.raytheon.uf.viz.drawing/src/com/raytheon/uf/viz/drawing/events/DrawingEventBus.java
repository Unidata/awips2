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
package com.raytheon.uf.viz.drawing.events;

import com.google.common.eventbus.EventBus;

/**
 * Declare its own event bus so events can be sent efficiently
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class DrawingEventBus {
    private static EventBus eventBus;

    private static DrawingListener drawingListener;

    /**
     * @return the eventBus
     */
    public static EventBus getEventBus() {
        if (eventBus == null) {
            eventBus = new EventBus("DrawingEventBus");
        }
        return eventBus;
    }

    /**
     * @return the drawingListener
     */
    public static DrawingListener getDrawingListener() {
        if (drawingListener == null) {
            drawingListener = new DrawingListener();
        }
        return drawingListener;
    }

    /**
     * Send in class to register in the event bus
     * 
     * @param ob
     */
    public static void register(Object ob) {
        getEventBus().register(ob);
    }
}
