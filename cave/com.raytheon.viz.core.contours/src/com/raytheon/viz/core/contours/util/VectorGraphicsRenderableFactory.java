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
package com.raytheon.viz.core.contours.util;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;

/**
 * VectorGraphicsRenderable Factory
 * 
 * Constructs the VectorGraphicsRenderable for D2D usage of GriddedVectorDisplay
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2013     #2287  randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class VectorGraphicsRenderableFactory implements
        IVectorGraphicsRenderableFactory {
    private double scale;

    public VectorGraphicsRenderableFactory() {
        this.scale = 0.6;
    }

    public VectorGraphicsRenderableFactory(double scale) {
        this.scale = scale;
    }

    /**
     * @param scale
     *            the scale to set
     */
    public void setScale(double scale) {
        this.scale = scale;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.util.IVectorGraphicsRenderableFactory#
     * createRenderable()
     */
    @Override
    public VectorGraphicsRenderable createRenderable(IDescriptor descriptor,
            IGraphicsTarget target, double size) {
        return new VectorGraphicsRenderable(descriptor, target, size, scale);
    }

}
