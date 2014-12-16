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
package gov.noaa.nws.ncep.viz.rsc.timeseries;

import gov.noaa.nws.ncep.common.dataplugin.geomag.table.KStationCoefficient;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.KStationCoefficientLookup;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.rsc.timeseries.rsc.GeoMagResource;
import gov.noaa.nws.ncep.viz.rsc.timeseries.rsc.GeoMagResourceData;
import gov.noaa.nws.ncep.viz.ui.display.NCTimeSeriesGraph;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
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
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.xy.graph.GraphLabelComparator;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.axis.GraphAxis;
import com.raytheon.uf.viz.xy.graph.axis.IAxis;
import com.raytheon.uf.viz.xy.graph.axis.LinearAxisPlacer;
import com.raytheon.uf.viz.xy.graph.labeling.DataTimeLabel;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The geomag graph, needs to be extracted into AbstractGraph
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/10/2014   1136       qzhou       Changed graphExtent for time series 1000*400
 *                                     Added getStationLocalTime. Added paintBorderRect, Added paintMidnightNoon, 
 *                                     Added paintXTitle. Modified paintTitles. Added ticks on yAxes.
 * 07/28/2014   R4079      sgurung     Changed graphExtent, and x and y title coordinates.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class GeoMagGraph extends NCTimeSeriesGraph {

    private SimpleDateFormat sdf;

    private int duration;

    private double deltaMax = 0.0;

    public GeoMagGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
        sdf = new SimpleDateFormat("HHmm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

    }

    @Override
    protected void createAxes() {
        /*
         * graphExtent(25,999,25,875) is not wide enough for time sereis panes.
         * Make the wide as 2.5 times of height. Make new extent graphExtent(0,
         * 1000, 300, 700).
         */
        graphExtent = new PixelExtent(0, 1000, 275, 700);
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
        double halfDelta = getDeltaMax() / 2.0;
        xAxes = new IAxis[5]; // fixed 5
        for (int i = 0; i < xAxes.length; ++i) {
            xAxes[i] = new GraphAxis();
            xAxes[i].setLineStyle(LineStyle.DOTTED);
            xAxes[i].setDrawAxis(true);

            if (i < xAxes.length / 2)
                xAxes[i].setDiscreteValue(-halfDelta / (i + 1));
            else if (i > xAxes.length / 2)
                xAxes[i].setDiscreteValue(halfDelta / (xAxes.length - i));
            else if (i == xAxes.length / 2)
                xAxes[i].setDiscreteValue(00.0);

        }

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

        double maxX = graphExtent.getMaxX();
        double maxY = graphExtent.getMaxY();

        double[] offsets = yAxisPlacer.placeAxes(yAxes);

        for (int i = 0; i < offsets.length; ++i) {
            double offset = offsets[i];
            yAxes[i].setStartLoc(new Coordinate(maxX - offset, maxY, 0));
            yAxes[i].setEndLoc(new Coordinate(maxX - offset, maxY, 0));
        }
    }

    @Override
    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        // Can only handle graphing of GeoMagResources
        return (rsc instanceof GeoMagResource);
    }

    @Override
    protected void paintUnits(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        RGB colorToUse = null;
        List<DrawableString> strings = new ArrayList<DrawableString>();
        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagResource rsc = (GeoMagResource) grsc;

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

                colorToUse = RGBColors.getRGBColor("white");

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
    public void constructVirtualExtent() {

        // make sure all resources are initialized
        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagResource rsc = (GeoMagResource) grsc;
            if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                return;
            }
        }

        // Loop through resources and create extent then call
        // updateVirtualExtent

        double[] minMaxY = new double[2];
        xLabels.clear();

        getXaxisIntervals(xLabels);
        double minX = 0;
        double maxX = 0;
        minMaxY[0] = 0;
        minMaxY[1] = 0;

        double delta = getDeltaMax();

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
        yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minX, // q
                maxX);

        updateVirtualExtent();

        newResources = false;

    }

    private void getXaxisIntervals(List<IGraphLabel<DataTime>> xLabels) {
        for (IGraphableResource<?, ?> grsc : graphResource) {
            if (grsc instanceof GeoMagResource) {
                GeoMagResource rsc = (GeoMagResource) grsc;
                DataTime[] range = rsc.getDataTimes();

                if (range == null || range.length == 0)
                    continue;

                DataTime start = range[0];
                xLabels.add(new DataTimeLabel(start));
                DataTime end = range[range.length - 1];
                xLabels.add(new DataTimeLabel(end));

                NCTimeMatcher tm = (NCTimeMatcher) rsc.getDescriptor()
                        .getTimeMatcher();

                if (tm != null)
                    duration = tm.getGraphRange();

                int numInterval = 4;
                if (duration > 12 & duration < 72)
                    numInterval = duration / 3;
                else if (duration >= 72 & duration < 168)
                    numInterval = duration / 6;
                else if (duration >= 168)
                    numInterval = duration / 12;

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

    private double getDeltaMax() {

        if (deltaMax < getDeltaFromResource())
            deltaMax = getDeltaFromResource();
        return deltaMax;
    }

    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        RGB colorToUse = RGBColors.getRGBColor("white");
        List<String> baselineAndColor = new ArrayList<String>();

        // paint first x title
        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagResource rsc = (GeoMagResource) grsc;

            if (rsc.getProperties().isVisible()) {
                if (((GeoMagResourceData) rsc.getResourceData())
                        .getYAxesTitle().substring(1, 2).equals(" ")) {
                    String color = ((GeoMagResourceData) rsc.getResourceData())
                            .getDataColor();
                    colorToUse = RGBColors.getRGBColor(color);

                    paintXTitle(target, paintProps,
                            ((GeoMagResourceData) rsc.getResourceData())
                                    .getXAxesTitle(), colorToUse);
                }
                // break;
            }
        }

        for (IGraphableResource<?, ?> grsc : graphResource) {
            GeoMagResource rsc = (GeoMagResource) grsc;

            if (rsc.getProperties().isVisible()
                    && ((GeoMagResourceData) rsc.getResourceData())
                            .getYAxesTitle().substring(1, 2).equals(" ")) {// H,D
                String color = ((GeoMagResourceData) rsc.getResourceData())
                        .getDataColor();
                colorToUse = RGBColors.getRGBColor(color);

                NCTimeMatcher tm = (NCTimeMatcher) rsc.getDescriptor()
                        .getTimeMatcher();

                String rscTitle = rsc.getTitle();

                if (!baselineAndColor.contains((rscTitle + "," + color))) {
                    baselineAndColor.add(rscTitle + "," + color);

                }
            }
        }

        // consider autoUpdate, only last H or D title will be displayed
        List<String> baseline = new ArrayList<String>();
        List<String> baselinePrint = new ArrayList<String>();

        for (int i = baselineAndColor.size() - 1; i >= 0; i--) {
            String[] string = baselineAndColor.get(i).split(",");
            String str1 = string[0].substring(0, string[0].indexOf(" "));

            if (!baseline.contains(str1)) {
                baseline.add(str1);
                baselinePrint.add(baselineAndColor.get(i));
            }
        }

        for (int i = 0; i < baselinePrint.size(); i++) {
            String[] string = baselinePrint.get(i).split(",");
            if (string.length == 2)
                paintYTitle(target, paintProps, string[0],
                        RGBColors.getRGBColor(string[1]), i + 1);

        }

        paintBorderRect(target, paintProps, xLabels);

    }

    protected void paintXTitle(IGraphicsTarget target,
            PaintProperties paintProps, String title, RGB titleColor)
            throws VizException {

        DrawableString titleString = new DrawableString(title, titleColor);
        titleString.textStyle = TextStyle.DROP_SHADOW;
        titleString.horizontalAlignment = HorizontalAlignment.CENTER;
        titleString.verticallAlignment = VerticalAlignment.TOP;
        titleString.magnification = this.currentMagnification;

        double x = graphExtent.getMinX() + graphExtent.getWidth() / 2;
        double y = graphExtent.getMaxY() + 50;
        titleString.setCoordinates(x, y);

        target.drawStrings(titleString);
    }

    @Override
    protected void paintYTitle(IGraphicsTarget target,
            PaintProperties paintProps, String title, RGB titleColor, int index)
            throws VizException {
        // Paint the titles
        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();
        DrawableString titleString = new DrawableString(title, titleColor);
        titleString.textStyle = TextStyle.DROP_SHADOW;
        titleString.horizontalAlignment = HorizontalAlignment.LEFT;
        titleString.verticallAlignment = VerticalAlignment.BOTTOM;
        titleString.rotation = 90;
        titleString.magnification = this.currentMagnification;
        int width = target.getStringsBounds(titleString).getBounds().width;
        int height = target.getStringsBounds(titleString, "H").getBounds().height * 2;
        double x = graphExtent.getMinX() - 50 - height * (index);
        double y = graphExtent.getMaxY()
                - ((graphExtent.getHeight() - (width / ratio)) / 2);
        titleString.setCoordinates(x, y);

        target.drawStrings(titleString);
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

            RGB labelColor = RGBColors.getRGBColor("white");
            DrawableString parameters = new DrawableString(sdf.format(date),
                    labelColor);

            unitsFont = target.initializeFont((String) null, 14.0f,
                    new IFont.Style[] {});
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

        paintMidnightNoon(target, paintProps, xLabels);

        paintTicks(target, paintProps, xLabels);

    }

    private void paintBorderRect(IGraphicsTarget target,
            PaintProperties paintProps, List<IGraphLabel<DataTime>> xLabels)
            throws VizException {

        target.drawRect(graphExtent, RGBColors.getRGBColor("white"), 1, 1);
    }

    private void paintMidnightNoon(IGraphicsTarget target,
            PaintProperties paintProps, List<IGraphLabel<DataTime>> xLabels)
            throws VizException {

        double start = 0;
        double end = 0;
        if (xLabels.size() > 0) {
            start = xLabels.get(0).getDiscreteValue();
            end = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        }

        if (xLabels == null || xLabels.isEmpty()) // q
            return;

        String string = xLabels.get(0).getResource().getName();
        // Example: string = BOU - H_Component - Begin: 0000 UTC 06 May 2013
        double localTime = getStationLocalTime(string.substring(0, 3));

        for (int i = 0; i < xLabels.size(); i++) {
            int hour = xLabels.get(i).getUnderlyingObject().getValidTime()
                    .get(Calendar.HOUR_OF_DAY);

            // print midnight= orange. noon=yellow
            if (hour == 0) {
                double val = xLabels.get(i).getDiscreteValue() + localTime
                        * 3600 * 1000;

                if (val >= start && val <= end) {
                    double offset = yAxisPlacer.getPixelLoc(val);

                    target.drawLine(graphExtent.getMinX() + offset,
                            graphExtent.getMinY(), 0, graphExtent.getMinX()
                                    + offset, graphExtent.getMaxY(), 0,
                            RGBColors.getRGBColor("orange"), 1,
                            LineStyle.DASHED);
                    target.drawString(unitsFont, "M", graphExtent.getMinX()
                            + offset, graphExtent.getMinY(), 0.0,
                            TextStyle.DROP_SHADOW,
                            RGBColors.getRGBColor("orange"),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.BOTTOM, 0.0);
                } else if (val < start) {
                    val = val + 12;// noon
                    double offset = yAxisPlacer.getPixelLoc(val);

                    target.drawLine(graphExtent.getMinX() + offset,
                            graphExtent.getMinY(), 0, graphExtent.getMinX()
                                    + offset, graphExtent.getMaxY(), 0,
                            RGBColors.getRGBColor("yellow"), 1,
                            LineStyle.DASHED);
                    target.drawString(unitsFont, "N", graphExtent.getMinX()
                            + offset, graphExtent.getMinY(), 0.0,
                            TextStyle.DROP_SHADOW,
                            RGBColors.getRGBColor("yellow"),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.BOTTOM, 0.0);
                    // } else if (val > end) { // sometimes get an extra line
                    // val = val - 12;// noon
                    // double offset = yAxisPlacer.getPixelLoc(val);
                    //
                    // target.drawLine(graphExtent.getMinX() + offset,
                    // graphExtent.getMinY(), 0, graphExtent.getMinX()
                    // + offset, graphExtent.getMaxY(), 0,
                    // RGBColors.getRGBColor("yellow"), 1,
                    // LineStyle.DASHED);
                }

            } else if (hour == 12) {
                double val = xLabels.get(i).getDiscreteValue() + localTime
                        * 3600 * 1000;

                if (val > start && val < end) {
                    double offset = yAxisPlacer.getPixelLoc(val);

                    target.drawLine(graphExtent.getMinX() + offset,
                            graphExtent.getMinY(), 0, graphExtent.getMinX()
                                    + offset, graphExtent.getMaxY(), 0,
                            RGBColors.getRGBColor("yellow"), 1,
                            LineStyle.DASHED);
                    target.drawString(unitsFont, "N", graphExtent.getMinX()
                            + offset, graphExtent.getMinY(), 0.0,
                            TextStyle.DROP_SHADOW,
                            RGBColors.getRGBColor("yellow"),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.BOTTOM, 0.0);
                } else if (val < start) {
                    val = val + 12;// noon
                    double offset = yAxisPlacer.getPixelLoc(val);
                    target.drawLine(graphExtent.getMinX() + offset,
                            graphExtent.getMinY(), 0, graphExtent.getMinX()
                                    + offset, graphExtent.getMaxY(), 0,
                            RGBColors.getRGBColor("orange"), 1,
                            LineStyle.DASHED);
                    target.drawString(unitsFont, "M", graphExtent.getMinX()
                            + offset, graphExtent.getMinY(), 0.0,
                            TextStyle.DROP_SHADOW,
                            RGBColors.getRGBColor("orange"),
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.BOTTOM, 0.0);
                    // } else if (val > end) {
                    // val = val - 12;// noon
                    // double offset = yAxisPlacer.getPixelLoc(val);
                    // target.drawLine(graphExtent.getMinX() + offset,
                    // graphExtent.getMinY(), 0, graphExtent.getMinX()
                    // + offset, graphExtent.getMaxY(), 0,
                    // RGBColors.getRGBColor("orange"), 1,
                    // LineStyle.DASHED);
                }
            }
        }
    }

    /*
     * get station local time based on longitude. return the local time when utc
     * time hour is 0.
     */
    private double getStationLocalTime(String stnCode) {
        double local = 0;

        KStationCoefficientLookup lookup = KStationCoefficientLookup
                .getInstance();
        KStationCoefficient coefficient = lookup.getStationByCode(stnCode);
        String longitude = coefficient.getLongitude();

        double lon = Float.parseFloat(longitude);

        local = 24 - (lon / 15);

        return local;
    }

    private void paintTicks(IGraphicsTarget target, PaintProperties paintProps,
            List<IGraphLabel<DataTime>> xLabels) throws VizException {

        // X Axes ticks
        if (duration <= 24) {
            int count = 0;
            double temp = 0;
            for (int i = 0; i < xLabels.size(); i++) {
                if (temp != xLabels.get(i).getDiscreteValue()) {
                    temp = xLabels.get(i).getDiscreteValue();
                    count++;
                }
            }

            int size = (count - 1) * 3 + 1;

            double start = 0;
            double end = 0;
            if (xLabels.size() > 0) {
                start = xLabels.get(0).getDiscreteValue();
                end = xLabels.get(xLabels.size() - 1).getDiscreteValue();
            }
            double diff = end - start;

            for (int i = 0; i < size; ++i) {

                double val = start + (diff * i / (size - 1));
                double offset = yAxisPlacer.getPixelLoc(val);

                if (i % 3 == 0)
                    target.drawLine(graphExtent.getMinX() + offset,
                            graphExtent.getMaxY() - 15, 0,
                            graphExtent.getMinX() + offset,
                            graphExtent.getMaxY(), 0,
                            RGBColors.getRGBColor("white"), 1, LineStyle.SOLID);
                else
                    target.drawLine(graphExtent.getMinX() + offset,
                            graphExtent.getMaxY() - 10, 0,
                            graphExtent.getMinX() + offset,
                            graphExtent.getMaxY(), 0,
                            RGBColors.getRGBColor("white"), 1, LineStyle.SOLID);

            }
        } else {

            for (int i = 0; i < xLabels.size(); ++i) {

                double val = xLabels.get(i).getDiscreteValue();
                double offset = yAxisPlacer.getPixelLoc(val);

                target.drawLine(graphExtent.getMinX() + offset,
                        graphExtent.getMaxY() - 15, 0, graphExtent.getMinX()
                                + offset, graphExtent.getMaxY(), 0,
                        RGBColors.getRGBColor("white"), 1, LineStyle.SOLID);

            }
        }

        // Y Axes ticks, fixed 9
        for (int i = 0; i < 9; ++i) {

            double offset = (graphExtent.getMaxY() - graphExtent.getMinY())
                    / (9 - 1);

            target.drawLine(graphExtent.getMinX(), graphExtent.getMinY()
                    + offset * i, 0, graphExtent.getMinX() + 10,
                    graphExtent.getMinY() + offset * i, 0,
                    RGBColors.getRGBColor("white"), 1, LineStyle.SOLID);

        }
    }
}
