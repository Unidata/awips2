/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp;

import gov.noaa.nws.ncep.viz.rtkp.rsc.GeoMagRTKpResource;
import gov.noaa.nws.ncep.viz.rtkp.rsc.GeoMagRTKpResourceData;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;

import java.awt.Font;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.ResourceList;
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
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The RTKP Time Series graph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2014  1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagRTKpGraph extends TimeSeriesGraph {

    protected SimpleDateFormat sdf;

    protected SimpleDateFormat topLabelSdf = new SimpleDateFormat(
            "dd-MMM-yyyy HH:mm:ss 'UTC'");

    protected static final DecimalFormat df = new DecimalFormat("#");

    protected RGB colorToUse = RGBColors.getRGBColor("white");

    protected float fontSize = 14;

    protected String fontName = "Times";

    protected String fontStyle = "Bold";

    protected int fontStyleInt = Font.PLAIN;

    public GeoMagRTKpGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
        sdf = new SimpleDateFormat("HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        topLabelSdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected void createAxes() {

        graphExtent = new PixelExtent(0, 999, 150, 900);

        double minX = graphExtent.getMinX();
        double maxX = graphExtent.getMaxX();
        double minY = graphExtent.getMinY();
        double maxY = graphExtent.getMaxY();

        // Set up the bounding box axes
        minXAxis.setStartLoc(new Coordinate(minX, maxY, 0));
        minXAxis.setEndLoc(new Coordinate(maxX, maxY, 0));

        maxXAxis.setStartLoc(new Coordinate(minX, minY, 0));
        maxXAxis.setEndLoc(new Coordinate(maxX, minY, 0));

        minYAxis.setStartLoc(new Coordinate(minX, maxY, 0));
        minYAxis.setEndLoc(new Coordinate(minX, minY, 0));

        maxYAxis.setStartLoc(new Coordinate(maxX, maxY, 0));
        maxYAxis.setEndLoc(new Coordinate(maxX, minY, 0));

        // Create the Axis if they do not exist
        xAxes = new IAxis[28];
        int index = 0;
        for (int i = 0; i < 10; ++i) {
            xAxes[index] = new GraphAxis();
            xAxes[index].setLineStyle(LineStyle.SOLID);
            xAxes[index].setDrawAxis(true);
            xAxes[index].setDiscreteValue(i);
            index++;

            double tmp = i + (1 / 3.0);
            for (int j = 0; j < 2 && i < 9; j++) {
                xAxes[index] = new GraphAxis();
                xAxes[index].setLineStyle(LineStyle.DASHED);
                xAxes[index].setDrawAxis(true);
                xAxes[index].setDiscreteValue(tmp);

                tmp += (1 / 3.0);
                index++;
            }
        }

        // Place them
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

    @Override
    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        // Can only handle graphing of GeoMagRTKpMonitorResources
        return (rsc instanceof GeoMagRTKpResource);
    }

    @Override
    protected void paintUnits(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        unitsFont = target.initializeFont((String) null, 14.0f,
                new IFont.Style[] { IFont.Style.BOLD });
        unitsFont.setSmoothing(false);
        unitsFont.setScaleFont(false);

        List<DrawableString> strings = new ArrayList<DrawableString>();
        for (IGraphableResource<?, ?> grsc : graphResource) {
            TimeSeriesResource rsc = (TimeSeriesResource) grsc;
            if (rsc == null) {
                continue;
            } else if (rsc.getData() == null) {
                continue;
            } else if (rsc.getData().getData() == null) {
                continue;
            } else if (rsc.getData().getData().size() < 1) {
                continue;
            }
            if (rsc.getProperties().isVisible()) {
                // colorToUse = RGBColors.getRGBColor("white");
                //
                // if
                // (RTKpUtil.KS_PLOT.equals(rsc.getResourceData().getSource()))
                // {
                // colorToUse = rsc.getCapability(ColorableCapability.class)
                // .getColor();
                // }

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

                        String valueDblStr = "" + xAxes[i].getDiscreteValue();
                        if (valueDblStr.contains(".0")) {
                            String value = df.format(xAxes[i]
                                    .getDiscreteValue()) + " ";
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
        }
        target.drawStrings(strings);

        paintDataTimeUnits(target, paintProps, xLabels);
    }

    @Override
    protected void constructVirtualExtent() {

        // make sure all resources are initialized
        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagRTKpResource rsc = (GeoMagRTKpResource) grsc;
            if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                return;
            }
        }

        double[] minMaxY = new double[2];
        xLabels.clear();
        getXaxisIntervals(xLabels);
        double minX1 = 0;
        double maxX1 = 0;
        minMaxY[0] = 0;
        minMaxY[1] = 9;

        if (xLabels.size() > 0) {
            minX1 = xLabels.get(0).getDiscreteValue();
            maxX1 = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        }
        // normalizeAxis now takes into account data that will never be
        // negative like wind speed.
        // normalizeAxis(minMaxY);

        xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(), minMaxY[0],
                minMaxY[1]);
        yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minX1, maxX1);

        updateVirtualExtent();

        newResources = false;

    }

    @Override
    public void zoom(int index, Coordinate gridCoord) {
        // yAxisPlacer.zoom(gridCoord.x - graphExtent.getMinX(), index);
        // xAxisPlacer.zoom(graphExtent.getMaxY() - gridCoord.y, index);
        // double inc = xAxisPlacer.getDataWidth() / 10;
        // double newMin = (int) (xAxisPlacer.getMinDataValue() / inc) * inc;
        // xAxisPlacer.pan(xAxisPlacer.getPixelLoc(newMin));
        // System.out.println("ZOOM");
        // updateVirtualExtent();
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

    private void getXaxisIntervals(List<IGraphLabel<DataTime>> xLabels) {
        for (IGraphableResource<?, ?> grsc : graphResource) {
            if (grsc instanceof GeoMagRTKpResource) {
                GeoMagRTKpResource rsc = (GeoMagRTKpResource) grsc;
                DataTime start = rsc.getStartTime();
                xLabels.add(new DataTimeLabel(start));
                DataTime end = rsc.getEndTime();
                xLabels.add(new DataTimeLabel(end));

                int hrs = ((GeoMagRTKpResourceData) rsc.getResourceData())
                        .getPlotLengthInHours();
                int numInterval = 3;
                if (hrs % 4 == 0)
                    numInterval = 4;
                if (hrs % 6 == 0)
                    numInterval = 6;
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
        }
        Collections.sort(xLabels, new GraphLabelComparator());
    }

    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        titleFont = target.initializeFont((String) null, 14.0f,
                new IFont.Style[] { IFont.Style.BOLD });
        titleFont.setSmoothing(false);
        titleFont.setScaleFont(false);

        Date updatedDate = RTKpUtil.getCurrentTime();

        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();

        if (editor != null) {
            IDisplayPane activePane = editor.getActiveDisplayPane();
            ResourceList acResources = activePane.getDescriptor()
                    .getResourceList();
            int acRscSize = acResources.size();

            for (int i = acRscSize - 1; i >= 0; i--) {
                ResourcePair rp = acResources.get(i);
                AbstractVizResource<?, ?> activeRsc = rp.getResource();

                if (activeRsc != null
                        && activeRsc instanceof GeoMagRTKpResource
                        && rp.getProperties().isVisible()) {

                    KsPlotCapability ksCap = activeRsc
                            .getCapability(KsPlotCapability.class);
                    if (ksCap != null) {
                        if (RTKpUtil.KS_PLOT
                                .equals(((TimeSeriesResourceData) activeRsc
                                        .getResourceData()).getSource())) {
                            colorToUse = activeRsc.getCapability(
                                    KsPlotCapability.class).getTextColor();
                            fontSize = (float) Integer.parseInt(activeRsc
                                    .getCapability(KsPlotCapability.class)
                                    .getTextSize());
                            fontName = activeRsc.getCapability(
                                    KsPlotCapability.class).getTextFont();
                            fontStyle = activeRsc.getCapability(
                                    KsPlotCapability.class).getTextStyle();
                            updatedDate = ((GeoMagRTKpResource) activeRsc)
                                    .getUpdatedDate();
                        } else if (RTKpUtil.KP_PLOT
                                .equals(((TimeSeriesResourceData) activeRsc
                                        .getResourceData()).getSource())) {
                            colorToUse = activeRsc.getCapability(
                                    KpPlotCapability.class).getTextColor();
                            fontSize = (float) Integer.parseInt(activeRsc
                                    .getCapability(KpPlotCapability.class)
                                    .getTextSize());
                            fontName = activeRsc.getCapability(
                                    KpPlotCapability.class).getTextFont();
                            fontStyle = activeRsc.getCapability(
                                    KpPlotCapability.class).getTextStyle();
                        }

                        if (fontStyle.compareTo("Italic") == 0)
                            fontStyleInt = Font.ITALIC;
                        else if (fontStyle.compareTo("Bold") == 0)
                            fontStyleInt = Font.BOLD;
                        else if (fontStyle.compareTo("Bold-Italic") == 0) {
                            fontStyleInt = Font.BOLD | Font.ITALIC;
                        }

                        IFont derivedFont = target.initializeFont(fontName,
                                fontSize,
                                new IFont.Style[] { IFont.Style.BOLD });

                        if (derivedFont != null) {
                            titleFont = derivedFont;
                            titleFont.setSmoothing(false);
                            titleFont.setScaleFont(false);
                        }

                        break;
                    }
                }

            }
        }

        int i = 0;
        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagRTKpResource rsc = (GeoMagRTKpResource) grsc;
            if (rsc.getProperties().isVisible()) {
                String[] rscTitle = rsc.getTitles();
                paintYTitle(target, paintProps, rscTitle[0], colorToUse, i++);

                if (rsc.getResourceData() != null) {
                    paintXTitle(
                            target,
                            paintProps,
                            "Start Time ("
                                    + topLabelSdf.format(rsc.getStartTime()
                                            .getRefTime()) + ")", colorToUse);
                }
            }
        }

        DrawableString titleString = new DrawableString(
                "Real-time Plot for Estimated Kp Index", colorToUse);
        titleString.font = titleFont;
        titleString.textStyle = TextStyle.DROP_SHADOW;
        titleString.horizontalAlignment = HorizontalAlignment.LEFT;
        titleString.horizontalAlignment = HorizontalAlignment.CENTER;
        titleString.verticallAlignment = VerticalAlignment.TOP;
        titleString.magnification = this.currentMagnification;
        double x = graphExtent.getMinX() + graphExtent.getWidth() / 2;
        titleString.setCoordinates(x, 100.0);
        target.drawStrings(titleString);

        paintTopTimeLabels(target, paintProps, updatedDate);
    }

    @Override
    protected void paintYTitle(IGraphicsTarget target,
            PaintProperties paintProps, String title, RGB titleColor, int index)
            throws VizException {

        // Paint the titles
        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();
        DrawableString titleString = new DrawableString(title, titleColor);
        titleString.font = titleFont;
        titleString.textStyle = TextStyle.DROP_SHADOW;
        titleString.horizontalAlignment = HorizontalAlignment.LEFT;
        titleString.verticallAlignment = VerticalAlignment.BOTTOM;
        titleString.rotation = 90;
        titleString.magnification = this.currentMagnification;
        int width = target.getStringsBounds(titleString).getBounds().width;
        int height = target.getStringsBounds(titleString, "N").getBounds().height * 2;
        double x = graphExtent.getMinX() - 40.0 - height * (index);
        double y = graphExtent.getMaxY()
                - ((graphExtent.getHeight() - (width / ratio)) / 2.0) + 30.0;
        titleString.setCoordinates(x, y);

        target.drawStrings(titleString);
    }

    @Override
    protected void paintDataTimeUnits(IGraphicsTarget target,
            PaintProperties paintProps, List<IGraphLabel<DataTime>> xLabels)
            throws VizException {

        unitsFont = target.initializeFont((String) null, 14.0f,
                new IFont.Style[] { IFont.Style.BOLD });
        unitsFont.setSmoothing(false);
        unitsFont.setScaleFont(false);

        List<DrawableString> strings = new ArrayList<DrawableString>(
                xLabels.size());

        for (IGraphLabel<DataTime> xLabel : xLabels) {
            double val = xLabel.getDiscreteValue();
            Date date = xLabel.getUnderlyingObject().getRefTime();

            // RGB labelColor = getColorToUse();
            // RGB labelColor = xLabel.getResource()
            // .getCapability(ColorableCapability.class).getColor();
            DrawableString parameters = new DrawableString(sdf.format(date),
                    colorToUse);
            parameters.font = unitsFont;
            parameters.horizontalAlignment = HorizontalAlignment.CENTER;
            parameters.verticallAlignment = VerticalAlignment.TOP;
            parameters.magnification = this.currentMagnification;

            double offset = yAxisPlacer.getPixelLoc(val);
            Coordinate loc = new Coordinate(graphExtent.getMinX() + offset,
                    graphExtent.getMaxY() + 5, 0);
            parameters.setCoordinates(loc.x, loc.y, loc.z);

            strings.add(parameters);
        }
        target.drawStrings(strings);

        // paint x-axis tick marks
        for (int i = 0; i < xLabels.size(); i++) {

            double val = xLabels.get(i).getDiscreteValue();
            double offset = yAxisPlacer.getPixelLoc(val);

            target.drawLine(graphExtent.getMinX() + offset,
                    graphExtent.getMaxY() - 7, 0, graphExtent.getMinX()
                            + offset, graphExtent.getMaxY(), 0,
                    RGBColors.getRGBColor("white"), 1, LineStyle.SOLID);

        }

    }

    protected void paintTopTimeLabels(IGraphicsTarget target,
            PaintProperties paintProps, Date updatedDate) throws VizException {
        // Paint the top labels

        DrawableString lastDataString = new DrawableString("Last Data "
                + topLabelSdf.format(RTKpUtil.getKpStationsLastDataDate(null,
                        null)), colorToUse);
        lastDataString.font = titleFont;
        lastDataString.textStyle = TextStyle.DROP_SHADOW;
        lastDataString.horizontalAlignment = HorizontalAlignment.CENTER;
        lastDataString.verticallAlignment = VerticalAlignment.BOTTOM;
        lastDataString.magnification = this.currentMagnification;
        double x = graphExtent.getMinX() + graphExtent.getWidth() / 1.3
                - titleFont.getFontSize();
        lastDataString.setCoordinates(x, 45.0);

        target.drawStrings(lastDataString);

        // Date updatedDate = ((GeoMagRTKpResource) graphResource.get(0))
        // .getUpdatedDate();

        // System.out.println(" update date = " + updatedDate.toGMTString()
        // + " size = " + graphResource.size());
        // if (graphResource.size() > 1) {
        // if (((GeoMagRTKpResource) graphResource.get(1)).getUpdatedDate()
        // .after(updatedDate)) {
        // updatedDate = ((GeoMagRTKpResource) graphResource.get(1))
        // .getUpdatedDate();
        //
        // }
        // }

        DrawableString updatedString = new DrawableString("Updated   "
                + topLabelSdf.format(updatedDate), colorToUse);
        updatedString.font = titleFont;
        updatedString.textStyle = TextStyle.DROP_SHADOW;
        updatedString.horizontalAlignment = HorizontalAlignment.CENTER;
        updatedString.verticallAlignment = VerticalAlignment.BOTTOM;
        updatedString.magnification = this.currentMagnification;
        updatedString.setCoordinates(x, 75.0);

        target.drawStrings(updatedString);

    }

    protected void paintXTitle(IGraphicsTarget target,
            PaintProperties paintProps, String title, RGB titleColor)
            throws VizException {
        // Paint the titles
        double ratio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        DrawableString titleString = new DrawableString(title, titleColor);
        titleString.font = titleFont;
        titleString.textStyle = TextStyle.DROP_SHADOW;
        titleString.horizontalAlignment = HorizontalAlignment.CENTER;
        titleString.verticallAlignment = VerticalAlignment.TOP;
        titleString.magnification = this.currentMagnification;
        double x = graphExtent.getMinX() + graphExtent.getWidth() / 2;
        double y = graphExtent.getMaxY() + 60.0;

        titleString.setCoordinates(x, y);

        target.drawStrings(titleString);
    }

    protected RGB getColorToUse() {
        RGB colorToUse = RGBColors.getRGBColor("white");
        AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();

        if (editor != null) {
            IDisplayPane activePane = editor.getActiveDisplayPane();
            ResourceList acResources = activePane.getDescriptor()
                    .getResourceList();
            int acRscSize = acResources.size();

            for (int i = acRscSize - 1; i >= 0; i--) {
                ResourcePair rp = acResources.get(i);
                AbstractVizResource<?, ?> activeRsc = rp.getResource();

                if (activeRsc != null
                        && activeRsc instanceof GeoMagRTKpResource
                        && rp.getProperties().isVisible()) {

                    KsPlotCapability ksCap = activeRsc
                            .getCapability(KsPlotCapability.class);
                    if (ksCap != null) {
                        if (RTKpUtil.KS_PLOT
                                .equals(((TimeSeriesResourceData) activeRsc
                                        .getResourceData()).getSource())) {
                            colorToUse = activeRsc.getCapability(
                                    ColorableCapability.class).getColor();
                        }
                        break;
                    }
                }

            }
        }
        return colorToUse;
    }
}
