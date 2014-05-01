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
package com.raytheon.uf.viz.xy.timeheight.graph;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.graph.AbstractGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.axis.LinearAxisPlacer;
import com.raytheon.uf.viz.xy.graph.axis.LogarithmicAxisPlacer;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor.TimeDirection;
import com.raytheon.uf.viz.xy.timeheight.rsc.AbstractTimeHeightResource;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class TimeHeightGraph extends AbstractGraph {

    /** The x labels */
    protected List<IGraphLabel<DataTime>> xLabels;

    private int zoomLevel = 1;

    /**
     * @param descriptor
     */
    public TimeHeightGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
        xLabels = new ArrayList<IGraphLabel<DataTime>>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.AbstractGraph#canHandleResoruce(com.raytheon
     * .uf.viz.xy.map.rsc.IGraphableResource)
     */
    @Override
    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        return rsc instanceof AbstractTimeHeightResource;
    }

    @Override
    public void removeGraphResource(IGraphableResource<?, ?> rsc) {
        super.removeGraphResource(rsc);
        if (graphResource.size() == 0) {
            xLabels.clear();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.AbstractGraph#constructVirtualExtent()
     */
    @Override
    protected void constructVirtualExtent() {
        xLabels.clear();
        double[] minMaxX = new double[2];
        getRangeData(xLabels, new ArrayList<IGraphLabel<Double>>());
        minMaxX[0] = xLabels.get(0).getDiscreteValue();
        minMaxX[1] = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        HeightScale heightScale = ((TimeHeightDescriptor) descriptor)
                .getHeightScale();
        if (heightScale.getScale() == ScaleType.LOG) {
            xAxisPlacer = new LogarithmicAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        } else {
            xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        }
        if (((TimeHeightDescriptor) descriptor).getTimeDirection() == TimeDirection.LEFT_TO_RIGHT) {
            yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(),
                    minMaxX[0], minMaxX[1]);
        } else {
            yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(),
                    minMaxX[1], minMaxX[0]);
        }
        updateVirtualExtent();
        newResources = false;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.AbstractGraph#createAxes()
     */
    @Override
    protected void createAxes() {
        yAxisPlacer.setPixelWidth(graphExtent.getWidth());

        createHeightAxis(((TimeHeightDescriptor) descriptor).getHeightScale(),
                zoomLevel);
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (1 / paintProps.getZoomLevel() != zoomLevel) {
            zoomLevel = (int) (1 / paintProps.getZoomLevel());
            createAxes();
            redraw = true;
        }
        super.paint(target, paintProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.AbstractGraph#paintTitles(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (titleFont == null) {
            titleFont = target.initializeFont((String) null, 11.0f,
                    new IFont.Style[] { IFont.Style.BOLD });
        }

        RGB titleColor = descriptor.getGraphResource()
                .getCapability(ColorableCapability.class).getColor();
        String title = ((TimeHeightDescriptor) descriptor).getHeightScale()
                .getUnit();
        paintYTitle(target, paintProps, title, titleColor, 0);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.AbstractGraph#paintUnits(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintUnits(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (unitsFont == null) {
            unitsFont = target.initializeFont((String) null, 10.0f,
                    new IFont.Style[] {});
        }

        paintHeightUnits(target, paintProps);

        paintDataTimeUnits(target, paintProps, xLabels);
    }

    @Override
    public void zoom(int index, Coordinate gridCoord) {
        // Not handled by this
    }

    @Override
    public void pan(double xDist, double yDist, boolean panning) {
        // Not handled by this
    }

}
