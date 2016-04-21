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
package com.raytheon.uf.viz.collaboration.ui.editor.event;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * An input event that holds necessary information to recreate the event on a
 * different machine.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class InputEvent {

    public enum EventType {
        MOUSE_DOWN, MOUSE_DOWN_MOVE, MOUSE_UP, MOUSE_HOVER, MOUSE_MOVE, DOUBLE_CLICK, MOUSE_WHEEL, KEY_UP, KEY_DOWN
    }

    @DynamicSerializeElement
    protected double x;

    @DynamicSerializeElement
    protected double y;

    /**
     * Associated metadata with an event. In the case of a button click, it is
     * the button id. For a scroll wheel, it is the count. For a key press, it
     * is the key code.
     */
    @DynamicSerializeElement
    protected int eventData;

    @DynamicSerializeElement
    protected EventType type;

    public InputEvent() {

    }

    public InputEvent(EventType type, double x, double y, int eventData) {
        this.type = type;
        this.x = x;
        this.y = y;
        this.eventData = eventData;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
    }

    public EventType getType() {
        return type;
    }

    public void setType(EventType type) {
        this.type = type;
    }

    public int getEventData() {
        return eventData;
    }

    public void setEventData(int eventData) {
        this.eventData = eventData;
    }

}
