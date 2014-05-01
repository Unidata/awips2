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
package com.raytheon.uf.viz.xy.timeseries.graph;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.graph.AbstractGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.axis.GraphAxis;
import com.raytheon.uf.viz.xy.graph.axis.IAxis;
import com.raytheon.uf.viz.xy.graph.axis.LinearAxisPlacer;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The Time Series graph, needs to be extracted into AbstractGraph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            mschenke     Initial creation
 * Feb 10, 2011 8244       bkowal       replaced deprecated method calls;
 *                                      magnitude influences axis label font.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeSeriesGraph extends AbstractGraph {

    protected static final DecimalFormat df = new DecimalFormat("#.0###");

    /** The x labels */
    protected List<IGraphLabel<DataTime>> xLabels;

    public TimeSeriesGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
        xLabels = new ArrayList<IGraphLabel<DataTime>>();
    }

    @Override
    protected void createAxes() {
        // Create the Axis if they do not exist
        if (xAxes.length == 0) {
            xAxes = new IAxis[11];
            for (int i = 0; i < xAxes.length; ++i) {
                xAxes[i] = new GraphAxis();
                xAxes[i].setLineStyle(LineStyle.DASHED);
                xAxes[i].setDrawAxis(true);
            }
        }

        // Update the values
        double inc = xAxisPlacer.getDataWidth() / 10;
        double val = Math.ceil(xAxisPlacer.getMinDataValue() / inc) * inc;

        for (int i = 0; i < xAxes.length; i++) {
            xAxes[i].setDiscreteValue(val + inc * i);
        }

        // Place them
        double minX = graphExtent.getMinX();
        double maxX = graphExtent.getMaxX();
        double maxY = graphExtent.getMaxY();

        xAxisPlacer.setPixelWidth(graphExtent.getHeight());
        yAxisPlacer.setPixelWidth(graphExtent.getWidth());

        // Place the data axes
        double[] offsets = xAxisPlacer.placeAxes(xAxes);

        for (int i = 0; i < offsets.length; ++i) {
            double offset = offsets[i];
            xAxes[i].setStartLoc(new Coordinate(minX, maxY - offset, 0));
            xAxes[i].setEndLoc(new Coordinate(maxX, maxY - offset, 0));
        }
    }

    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        // Can only handle graphing of TimeSeriesResources
        return (rsc instanceof TimeSeriesResource);
    }

    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        int i = 0;
        for (IGraphableResource<?, ?> grsc : graphResource) {
            TimeSeriesResource rsc = (TimeSeriesResource) grsc;
            if (rsc.getProperties().isVisible()) {
                RGB colorToUse = rsc.getCapability(ColorableCapability.class)
                        .getColor();
                String rscTitle = rsc.getTitles()[1];
                paintYTitle(target, paintProps, rscTitle, colorToUse, i++);
            }
        }
    }

    @Override
    protected void paintUnits(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        RGB colorToUse = null;
        List<DrawableString> strings = new ArrayList<DrawableString>();
        for (IGraphableResource<?, ?> grsc : graphResource) {
            TimeSeriesResource rsc = (TimeSeriesResource) grsc;
            if (rsc == null) {
                // System.err.println("rsc null");
                continue;
            } else if (rsc.getData() == null) {
                // System.err.println("getData() null");
                continue;
            } else if (rsc.getData().getData() == null) {
                // System.err.println("getData() second null");
                continue;
            } else if (rsc.getData().getData().size() < 1) {
                continue;
            }
            if (rsc.getProperties().isVisible()) {
                colorToUse = rsc.getCapability(ColorableCapability.class)
                        .getColor();

                if (rsc.getData() == null
                        || rsc.getData().getData().size() == 0
                        || !(rsc.getData().getData().get(0) instanceof XYImageData)) {
                    for (int i = 0; i < xAxes.length; i++) {
                        Coordinate[] coords = xAxes[i].getCoordinates();
                        if (coords[0].y < graphExtent.getMinY()) {
                            continue;
                        }

                        DrawableString parameters = new DrawableString("",
                                colorToUse);
                        parameters.font = unitsFont;
                        parameters.textStyle = TextStyle.DROP_SHADOW;
                        parameters.horizontalAlignment = HorizontalAlignment.RIGHT;
                        parameters.magnification = this.currentMagnification;

                        String value = df.format(xAxes[i].getDiscreteValue());
                        if (i == 0) {
                            parameters.verticallAlignment = VerticalAlignment.BOTTOM;
                        } else {
                            parameters.verticallAlignment = VerticalAlignment.MIDDLE;
                        }
                        parameters.setText(value, colorToUse);
                        parameters.setCoordinates(coords[0].x, coords[0].y,
                                coords[0].z);
                        strings.add(parameters);
                    }
                }
            }
        }
        target.drawStrings(strings);

        paintDataTimeUnits(target, paintProps, xLabels);
    }

    @Override
    protected void constructVirtualExtent() {

        // make sure all resources are initialized
        for (IGraphableResource<?, ?> grsc : graphResource) {
            TimeSeriesResource rsc = (TimeSeriesResource) grsc;
            if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                return;
            }
        }

        // TODO: Loop through resources and create extent then call
        // updateVirtualExtent
        double[] minMaxY = new double[2];
        xLabels.clear();
        ArrayList<IGraphLabel<Double>> yLabels = new ArrayList<IGraphLabel<Double>>();
        getRangeData(xLabels, yLabels);
        double minX = 0;
        double maxX = 0;
        minMaxY[0] = 0;
        minMaxY[1] = 0;

        if (yLabels.size() > 0) {
            minMaxY[0] = yLabels.get(0).getDiscreteValue();
            minMaxY[1] = yLabels.get(yLabels.size() - 1).getDiscreteValue();
        }
        if (xLabels.size() > 0) {
            minX = xLabels.get(0).getDiscreteValue();
            maxX = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        }
        // normalizeAxis now takes into accout data that will never be
        // negative like wind speed.
        normalizeAxis(minMaxY);

        xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(), minMaxY[0],
                minMaxY[1]);
        yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minX, maxX);

        updateVirtualExtent();

        newResources = false;

    }

    @Override
    public void zoom(int index, Coordinate gridCoord) {
        yAxisPlacer.zoom(gridCoord.x - graphExtent.getMinX(), index);
        xAxisPlacer.zoom(graphExtent.getMaxY() - gridCoord.y, index);
        double inc = xAxisPlacer.getDataWidth() / 10;
        double newMin = (int) (xAxisPlacer.getMinDataValue() / inc) * inc;
        xAxisPlacer.pan(xAxisPlacer.getPixelLoc(newMin));
        updateVirtualExtent();
    }

    @Override
    public void pan(double xDist, double yDist, boolean panning) {
        yAxisPlacer.pan(xDist);
        xAxisPlacer.pan(-yDist);
        if (!panning) {
            double inc = xAxisPlacer.getDataWidth() / 10;
            double newMin = Math.round(xAxisPlacer.getMinDataValue() / inc)
                    * inc;
            xAxisPlacer.pan(xAxisPlacer.getPixelLoc(newMin));
        }
        updateVirtualExtent();
    }

}
