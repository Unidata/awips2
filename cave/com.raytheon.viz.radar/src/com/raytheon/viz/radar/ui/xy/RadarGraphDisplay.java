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
package com.raytheon.viz.radar.ui.xy;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.ui.AbstractNonMapDisplay;
import com.raytheon.viz.core.graphing.GraphProperties;
import com.raytheon.viz.core.graphing.xy.XYGraph;

/**
 * Display used for radar data that will be displayed on a graph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2009            askripsk    Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class RadarGraphDisplay extends AbstractNonMapDisplay {

    // The area that contains the displayed graphs
    protected XYGraph graphArea;

    public RadarGraphDisplay() {
        super(new PixelExtent(0, 1000, 0, 1000), new RadarGraphDescriptor());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);
        GraphProperties graphProps = new GraphProperties(paintProps);

        // Create the graphs for all of the resources
        if (graphArea == null) {
            if (descriptor.getResourceList().size() > 0) {
                this.initializeGraph((RadarGraphResource) descriptor
                        .getResourceList().get(0).getResource(), graphProps);
            }
        }

        // Paint the graphs
        paintSelectedResource(target, graphProps);
    }

    @Override
    public void calcPixelExtent(Rectangle clientArea) {
        IView view = getView();
        int[] dims = getDimensions();
        double[] center = view.getExtent().getCenter();
        double zoomLevel = view.recalcZoomLevel(dims);
        view.scaleToClientArea(clientArea, dims);
        view.zoom(zoomLevel);
        recenter(center);
    }

    private void paintSelectedResource(IGraphicsTarget target,
            GraphProperties graphProps) throws VizException {
        // Plot the resource data on the graph
        for (ResourcePair rp : getDescriptor().getResourceList()) {
            if (rp.getResource() != null) {
                graphProps = (GraphProperties) calcPaintDataTime(graphProps,
                        rp.getResource());
                rp.getResource().paint(target, graphProps);
            }
        }
    }

    private void initializeGraph(RadarGraphResource aRsc,
            GraphProperties graphProps) {
    }

}
