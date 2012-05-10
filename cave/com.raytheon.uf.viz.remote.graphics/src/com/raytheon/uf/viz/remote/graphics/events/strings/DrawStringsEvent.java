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
package com.raytheon.uf.viz.remote.graphics.events.strings;

import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for drawing multiple strings in bulk
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DrawStringsEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private DrawStringEvent[] strings;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#createDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public DrawStringsEvent createDiffObject(IRenderEvent event) {
        DrawStringsEvent diff = (DrawStringsEvent) event;
        DrawStringsEvent diffEvent = new DrawStringsEvent();
        if (diff.strings != null) {
            if (strings != null && diff.strings.length == strings.length) {
                diffEvent.strings = new DrawStringEvent[diff.strings.length];
                for (int i = 0; i < strings.length; ++i) {
                    DrawStringEvent paintEvent = strings[i];
                    DrawStringEvent diffPaintEvent = diff.strings[i];
                    if (paintEvent.equals(diffPaintEvent) == false) {
                        diffEvent.strings[i] = paintEvent
                                .createDiffObject(diffPaintEvent);
                    }
                }
            } else {
                diffEvent.strings = diff.strings;
            }
        }
        return diffEvent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        DrawStringsEvent event = (DrawStringsEvent) diffEvent;
        DrawStringEvent[] diffImageEvents = event.strings;
        if (diffImageEvents == null) {
            strings = null;
        } else if (strings == null) {
            strings = event.strings;
        } else if (strings.length != diffImageEvents.length) {
            strings = event.strings;
        } else {
            for (int i = 0; i < strings.length; ++i) {
                DrawStringEvent diffPaintEvent = diffImageEvents[i];
                if (diffPaintEvent != null) {
                    strings[i].applyDiffObject(diffPaintEvent);
                }
            }
        }
    }

    /**
     * @return the strings
     */
    public DrawStringEvent[] getStrings() {
        return strings;
    }

    /**
     * @param strings
     *            the strings to set
     */
    public void setStrings(DrawStringEvent[] strings) {
        this.strings = strings;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DrawStringsEvent other = (DrawStringsEvent) obj;
        if (!Arrays.equals(strings, other.strings))
            return false;
        return true;
    }

}
