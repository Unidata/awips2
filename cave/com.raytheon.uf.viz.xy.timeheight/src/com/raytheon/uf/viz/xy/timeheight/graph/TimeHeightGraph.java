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
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.scales.HeightScale.ScaleType;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor.TimeDirection;
import com.raytheon.uf.viz.xy.timeheight.rsc.AbstractTimeHeightResource;
import org.locationtech.jts.geom.Coordinate;

/**
 *
 * The background graph for a time height display
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 03, 2010            bsteffen    Initial creation
 * Jun 18, 2014 3242       njensen     Null safety checks
 * Nov 05, 2015 5070       randerso    Adjust font sizes for dpi scaling
 * Jul 18, 2017 6048       mapeters    Handle rare case where resources have no
 *                                     datatimes in constructVirtualExtent()
 *
 * </pre>
 *
 * @author bsteffen
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
        xLabels = new ArrayList<>();
    }

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

    @Override
    protected void constructVirtualExtent() {
        xLabels.clear();
        getRangeData(xLabels, new ArrayList<IGraphLabel<Double>>());

        double minX, maxX;
        if (!xLabels.isEmpty()) {
            minX = xLabels.get(0).getDiscreteValue();
            maxX = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        } else {
            // Arbitrary values, won't have any effect since there is no data
            minX = 0;
            maxX = 1;
        }

        HeightScale heightScale = ((TimeHeightDescriptor) descriptor)
                .getHeightScale();
        if (heightScale.getScale() == ScaleType.LOG) {
            xAxisPlacer = new LogarithmicAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        } else {
            xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        }
        if (((TimeHeightDescriptor) descriptor)
                .getTimeDirection() == TimeDirection.LEFT_TO_RIGHT) {
            yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minX,
                    maxX);
        } else {
            yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), maxX,
                    minX);
        }
        updateVirtualExtent();
        newResources = false;

    }

    @Override
    protected void createAxes() {
        if (yAxisPlacer != null && xAxisPlacer != null) {
            yAxisPlacer.setPixelWidth(graphExtent.getWidth());

            createHeightAxis(
                    ((TimeHeightDescriptor) descriptor).getHeightScale(),
                    zoomLevel);
        }
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

    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (titleFont == null) {
            titleFont = target.initializeFont((String) null, 9,
                    new IFont.Style[] { IFont.Style.BOLD });
        }

        RGB titleColor = descriptor.getGraphResource()
                .getCapability(ColorableCapability.class).getColor();
        String title = ((TimeHeightDescriptor) descriptor).getHeightScale()
                .getUnit();
        paintYTitle(target, paintProps, title, titleColor, 0);

    }

    @Override
    protected void paintUnits(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (unitsFont == null) {
            unitsFont = target.initializeFont((String) null, 8,
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
