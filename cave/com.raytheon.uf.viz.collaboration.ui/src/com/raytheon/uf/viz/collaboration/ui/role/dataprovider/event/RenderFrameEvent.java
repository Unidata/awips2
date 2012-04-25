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

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class RenderFrameEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private List<IRenderEvent> renderEvents = new LinkedList<IRenderEvent>();

    /**
     * @return the renderEvents
     */
    public List<IRenderEvent> getRenderEvents() {
        return renderEvents;
    }

    /**
     * @param renderEvents
     *            the renderEvents to set
     */
    public void setRenderEvents(List<IRenderEvent> renderEvents) {
        this.renderEvents = renderEvents;
    }

    public void addEvent(IRenderEvent event) {
        renderEvents.add(event);
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
        if (!super.equals(obj))
            return false;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RenderFrameEvent other = (RenderFrameEvent) obj;
        if (renderEvents == null) {
            if (other.renderEvents != null)
                return false;
        } else if (!renderEvents.equals(other.renderEvents))
            return false;
        return true;
    }

}
