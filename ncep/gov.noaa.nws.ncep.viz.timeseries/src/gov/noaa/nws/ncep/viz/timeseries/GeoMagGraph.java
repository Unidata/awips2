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
package gov.noaa.nws.ncep.viz.timeseries;

import gov.noaa.nws.ncep.viz.timeseries.rsc.GeoMagResource;
import gov.noaa.nws.ncep.viz.timeseries.rsc.GeoMagResourceData;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.graph.GraphLabelComparator;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.axis.GraphAxis;
import com.raytheon.uf.viz.xy.graph.axis.IAxis;
import com.raytheon.uf.viz.xy.graph.axis.LinearAxisPlacer;
import com.raytheon.uf.viz.xy.graph.labeling.DataTimeLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.timeseries.graph.TimeSeriesGraph;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
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

public class GeoMagGraph extends TimeSeriesGraph {

    // protected static final DecimalFormat df = new DecimalFormat("#.0###");
    private SimpleDateFormat sdf;

    public GeoMagGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
        sdf = new SimpleDateFormat("HHmm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected void createAxes() {
        // Create the Axis if they do not exist
        double halfDelta = getDeltaFromResource() / 2.0;
        xAxes = new IAxis[3];
        for (int i = 0; i < xAxes.length; ++i) {
            xAxes[i] = new GraphAxis();
            xAxes[i].setLineStyle(LineStyle.SOLID);
            xAxes[i].setDrawAxis(true);
        }
        xAxes[0].setDiscreteValue(-halfDelta);
        xAxes[1].setDiscreteValue(0.0);
        xAxes[2].setDiscreteValue(halfDelta);

        // Update the values
        // double inc = xAxisPlacer.getDataWidth() / 10;
        // double val = Math.ceil(xAxisPlacer.getMinDataValue() / inc) * inc;
        //
        // for (int i = 0; i < xAxes.length; i++) {
        // xAxes[i].setDiscreteValue(val + inc * i);
        // }

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

        createVerticalAxes();
    }

    private void createVerticalAxes() {

        double start = 0;
        double end = 0;
        if (xLabels.size() > 0) {
            start = xLabels.get(0).getDiscreteValue();
            end = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        }
        double diff = end - start;
        double numint = xLabels.size() - 1;

        yAxes = new IAxis[xLabels.size()];
        for (int i = 0; i < xLabels.size(); ++i) {
            yAxes[i] = new GraphAxis();
            yAxes[i].setLineStyle(LineStyle.SOLID);
            yAxes[i].setDrawAxis(true);
            yAxes[i].setDiscreteValue(start + (diff * i / numint));
        }

        // yAxes[0].setDiscreteValue(start);
        // yAxes[1].setDiscreteValue(start + (diff * 0.25));
        // yAxes[2].setDiscreteValue(start + (diff * 0.5));
        // yAxes[3].setDiscreteValue(start + (diff * 0.75));
        // yAxes[4].setDiscreteValue(end);

        double minY = graphExtent.getMinY();
        double maxX = graphExtent.getMaxX();
        double maxY = graphExtent.getMaxY();

        double[] offsets = yAxisPlacer.placeAxes(yAxes);

        for (int i = 0; i < offsets.length; ++i) {
            double offset = offsets[i];
            yAxes[i].setStartLoc(new Coordinate(maxX - offset, minY, 0));
            yAxes[i].setEndLoc(new Coordinate(maxX - offset, maxY, 0));
        }

    }

    @Override
    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        // Can only handle graphing of GeoMagResources
        return (rsc instanceof GeoMagResource);
    }

    // protected void paintTitles(IGraphicsTarget target,
    // PaintProperties paintProps) throws VizException {
    //
    // int i = 0;
    // for (IGraphableResource<?, ?> grsc : graphResource) {
    // TimeSeriesResource rsc = (TimeSeriesResource) grsc;
    // if (rsc.getProperties().isVisible()) {
    // RGB colorToUse = rsc.getCapability(ColorableCapability.class)
    // .getColor();
    // String rscTitle = rsc.getTitles()[1];
    // paintYTitle(target, paintProps, rscTitle, colorToUse, i++);
    // }
    // }
    // }
    //
    // @Override
    // protected void paintUnits(IGraphicsTarget target, PaintProperties
    // paintProps)
    // throws VizException {
    //
    // RGB colorToUse = null;
    // List<DrawableString> strings = new ArrayList<DrawableString>();
    // for (IGraphableResource<?, ?> grsc : graphResource) {
    // TimeSeriesResource rsc = (TimeSeriesResource) grsc;
    // if (rsc == null) {
    // // System.err.println("rsc null");
    // continue;
    // } else if (rsc.getData() == null) {
    // // System.err.println("getData() null");
    // continue;
    // } else if (rsc.getData().getData() == null) {
    // // System.err.println("getData() second null");
    // continue;
    // } else if (rsc.getData().getData().size() < 1) {
    // continue;
    // }
    // if (rsc.getProperties().isVisible()) {
    // colorToUse = rsc.getCapability(ColorableCapability.class)
    // .getColor();
    //
    // if (rsc.getData() == null
    // || rsc.getData().getData().size() == 0
    // || !(rsc.getData().getData().get(0) instanceof XYImageData)) {
    // for (int i = 0; i < xAxes.length; i++) {
    // Coordinate[] coords = xAxes[i].getCoordinates();
    // if (coords[0].y < graphExtent.getMinY()) {
    // continue;
    // }
    //
    // DrawableString parameters = new DrawableString("",
    // colorToUse);
    // parameters.font = unitsFont;
    // parameters.textStyle = TextStyle.DROP_SHADOW;
    // parameters.horizontalAlignment = HorizontalAlignment.RIGHT;
    // parameters.magnification = this.currentMagnification;
    //
    // String value = df.format(xAxes[i].getDiscreteValue());
    // if (i == 0) {
    // parameters.verticallAlignment = VerticalAlignment.BOTTOM;
    // } else {
    // parameters.verticallAlignment = VerticalAlignment.MIDDLE;
    // }
    // parameters.setText(value, colorToUse);
    // parameters.setCoordinates(coords[0].x, coords[0].y,
    // coords[0].z);
    // strings.add(parameters);
    // }
    // }
    // }
    // }
    // target.drawStrings(strings);
    //
    // paintDataTimeUnits(target, paintProps, xLabels);
    // }

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
        // ArrayList<IGraphLabel<Double>> yLabels = new
        // ArrayList<IGraphLabel<Double>>();
        getXaxisIntervals(xLabels);
        double minX = 0;
        double maxX = 0;
        minMaxY[0] = 0;
        minMaxY[1] = 0;

        double delta = getDeltaFromResource();
        minMaxY[0] = -1. * delta / 2.0;
        minMaxY[1] = delta / 2.0;

        if (xLabels.size() > 0) {
            minX = xLabels.get(0).getDiscreteValue();
            maxX = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        }
        // normalizeAxis now takes into accout data that will never be
        // negative like wind speed.
        // normalizeAxis(minMaxY);

        xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(), minMaxY[0],
                minMaxY[1]);
        yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minX, maxX);

        updateVirtualExtent();

        newResources = false;

    }

    // @Override
    // public void zoom(int index, Coordinate gridCoord) {
    // yAxisPlacer.zoom(gridCoord.x - graphExtent.getMinX(), index);
    // xAxisPlacer.zoom(graphExtent.getMaxY() - gridCoord.y, index);
    // double inc = xAxisPlacer.getDataWidth() / 10;
    // double newMin = (int) (xAxisPlacer.getMinDataValue() / inc) * inc;
    // xAxisPlacer.pan(xAxisPlacer.getPixelLoc(newMin));
    // updateVirtualExtent();
    // }
    //
    // @Override
    // public void pan(double xDist, double yDist, boolean panning) {
    // yAxisPlacer.pan(xDist);
    // xAxisPlacer.pan(-yDist);
    // if (!panning) {
    // double inc = xAxisPlacer.getDataWidth() / 10;
    // double newMin = Math.round(xAxisPlacer.getMinDataValue() / inc)
    // * inc;
    // xAxisPlacer.pan(xAxisPlacer.getPixelLoc(newMin));
    // }
    // updateVirtualExtent();
    // }

    private void getXaxisIntervals(List<IGraphLabel<DataTime>> xLabels) {
        for (IGraphableResource<?, ?> grsc : graphResource) {
            if (grsc instanceof GeoMagResource) {
                GeoMagResource rsc = (GeoMagResource) grsc;
                DataTime start = rsc.getStartTime();
                xLabels.add(new DataTimeLabel(start));
                DataTime end = rsc.getEndTime();
                xLabels.add(new DataTimeLabel(end));

                int hrs = ((GeoMagResourceData) rsc.getResourceData())
                        .getPlotLengthInHours();
                int numInterval = 3;
                if (hrs % 4 == 0)
                    numInterval = 4;
                if (hrs == 72)
                    numInterval = 3;
                long diff = end.getRefTime().getTime()
                        - start.getRefTime().getTime();

                for (int i = 1; i < numInterval; i++) {
                    long startTime = start.getRefTime().getTime();
                    long newTime = startTime + (diff * i / numInterval);
                    DataTime dtime = new DataTime(new Date(newTime));
                    xLabels.add(new DataTimeLabel(dtime));
                }
            }
            for (IGraphLabel<DataTime> label : xLabels) {
                label.setResource((AbstractVizResource<?, ?>) grsc);
            }
            // IGraphableResource<X, Y> rsc = (IGraphableResource<X, Y>) grsc;
            // if (rsc.getXRangeData().length > 0) {
            // IGraphLabel<Y>[] yVals = rsc.getYRangeData();
            // for (IGraphLabel<Y> val : yVals) {
            // val.setResource((AbstractVizResource<?, ?>) rsc);
            // yLabels.add(val);
            // }
            //
            // IGraphLabel<X>[] xVals = rsc.getXRangeData();
            // for (IGraphLabel<X> val : xVals) {
            // val.setResource((AbstractVizResource<?, ?>) rsc);
            // xLabels.add(val);
            // }
            // }
        }
        Collections.sort(xLabels, new GraphLabelComparator());
    }

    private double getDeltaFromResource() {
        double delta = 0.0;
        for (IGraphableResource<?, ?> grsc : graphResource) {
            if (grsc instanceof GeoMagResource) {
                GeoMagResource rsc = (GeoMagResource) grsc;
                delta = Math.max(delta, rsc.getDelta());

            }
        }
        return delta;
    }

    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        RGB colorToUse = RGBColors.getRGBColor("red");

        int i = 0;
        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagResource rsc = (GeoMagResource) grsc;
            if (rsc.getProperties().isVisible()) {
                if (rsc.getDelta() == 50.)
                    colorToUse = RGBColors.getRGBColor("light blue");
                String[] rscTitle = rsc.getTitles();
                paintYTitle(target, paintProps, rscTitle[0], colorToUse, i++);
                paintYTitle(target, paintProps, rscTitle[1], colorToUse, i++);
            }
        }
    }

    @Override
    protected void paintDataTimeUnits(IGraphicsTarget target,
            PaintProperties paintProps, List<IGraphLabel<DataTime>> xLabels)
            throws VizException {

        List<DrawableString> strings = new ArrayList<DrawableString>(
                xLabels.size());

        for (IGraphLabel<DataTime> xLabel : xLabels) {
            double val = xLabel.getDiscreteValue();
            Date date = xLabel.getUnderlyingObject().getRefTime();

            RGB labelColor = xLabel.getResource()
                    .getCapability(ColorableCapability.class).getColor();
            DrawableString parameters = new DrawableString(sdf.format(date),
                    labelColor);
            parameters.font = unitsFont;
            parameters.horizontalAlignment = HorizontalAlignment.CENTER;
            parameters.verticallAlignment = VerticalAlignment.TOP;
            parameters.magnification = this.currentMagnification;

            double offset = yAxisPlacer.getPixelLoc(val);
            Coordinate loc = new Coordinate(graphExtent.getMinX() + offset,
                    graphExtent.getMaxY(), 0);
            parameters.setCoordinates(loc.x, loc.y, loc.z);

            strings.add(parameters);
        }
        target.drawStrings(strings);
    }

}
