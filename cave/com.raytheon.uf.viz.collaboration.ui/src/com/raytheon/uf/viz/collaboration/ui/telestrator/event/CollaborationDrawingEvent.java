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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent;
import com.raytheon.uf.viz.drawing.events.DrawingEvent;
import com.vividsolutions.jts.geom.Geometry;

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
public class CollaborationDrawingEvent extends DrawingEvent implements
        IDisplayEvent {

    @DynamicSerializeElement
    private Geometry geom;

    // @DynamicSerializeElement
    // private RGB color;

    public CollaborationDrawingEvent() {

    }

    public CollaborationDrawingEvent(Geometry geom, RGB color) {
        this.geom = geom;
        // this.color = color;
    }

    // /**
    // * @return the color
    // */
    // public RGB getColor() {
    // return color;
    // }
    //
    // /**
    // * @param color
    // * the color to set
    // */
    // public void setColor(RGB color) {
    // this.color = color;
    // }

    /**
     * @return the geom
     */
    public Geometry getGeom() {
        return geom;
    }

    /**
     * @param geom
     *            the geom to set
     */
    public void setGeom(Geometry geom) {
        this.geom = geom;
    }
}
