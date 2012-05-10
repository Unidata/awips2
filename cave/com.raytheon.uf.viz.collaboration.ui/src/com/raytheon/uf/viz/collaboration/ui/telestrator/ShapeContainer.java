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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.vividsolutions.jts.geom.Geometry;

/**
 * This container holds the wireframe shape as well as the geometry that it
 * applies to. This will get serialized and sent over the network to other
 * users, and will just be the geometry at that time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 4, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class ShapeContainer {

    private IWireframeShape shape;

    @DynamicSerializeElement
    private Geometry geom;

    public ShapeContainer() {
    }

    /**
     * @return the shape
     */
    public IWireframeShape getShape() {
        return shape;
    }

    /**
     * @param shape
     *            the shape to set
     */
    public void setShape(IWireframeShape shape) {
        this.shape = shape;
    }

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
