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

package com.raytheon.viz.lpi;

import java.awt.geom.Rectangle2D;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.StyledMapResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.PointCapability;
import org.locationtech.jts.geom.Coordinate;

/**
 * Reads in a "D2D"-native LPI resource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Spe 17, 2007           randerso    Initial Creation.
 * May 16, 2014  3163     bsteffen    Remove WORD_WRAP TextStyle and handle
 *                                    wrapping locally.
 * Aug 21, 2014 #3459     randerso    Restructured Map resource class hierarchy
 * Nov 05, 2015 #5070     randerso    Moved label font management up to AbstractMapResource
 * Aug 09, 2018 6893      bsteffen    Perform additional progressive disclosure.
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class LPIResource
        extends StyledMapResource<LPIResourceData, IMapDescriptor> {

    /** Whether the resource is ready to be drawn */
    private boolean ready = false;

    private List<LPIPoint> points;

    private int pixelSizeHint = 90;

    private class LPIPoint {
        public Coordinate latLon;

        public double[] pixel;

        /*
         * Distance in km from the next closest station with a greater dist
         * value.
         */
        public double dist;

        public String label;

        LPIPoint() {
            latLon = new Coordinate();
        }

    }

    protected LPIResource(LPIResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        getCapability(LabelableCapability.class)
                .setAvailableLabelFields("Label");
        getCapability(LabelableCapability.class).setLabelField("Label");

        String filename = resourceData.getFilename();
        File file = new File(filename);
        if (!file.isAbsolute()) {
            filename = FileUtil.join(VizApp.getMapsDir(), filename);
            file = PathManagerFactory.getPathManager().getStaticFile(filename);
        }
        if ((file == null) || !file.exists()) {
            throw new VizException("Could not find lpi file",
                    new FileNotFoundException(filename));
        }

        points = new LinkedList<>();
        try (BufferedReader in = new BufferedReader(new FileReader(file))) {
            String s = in.readLine();
            while (s != null) {
                LPIPoint p = readPoint(s);
                if (p != null) {
                    points.add(p);
                }
                s = in.readLine();
            }

        } catch (FileNotFoundException e) {
            statusHandler.error("Cannot find LPI file.", e);
        } catch (IOException e) {
            statusHandler.error("Cannot load LPI file.", e);
        }
        points.sort(
                Comparator.comparingDouble((LPIPoint p) -> p.dist).reversed());

        project(this.descriptor.getCRS());

        ready = true;
    }

    public LPIPoint readPoint(String s) {
        LPIPoint p = this.new LPIPoint();
        try (Scanner in = new Scanner(s)) {
            if (!in.hasNextDouble()) {
                return null;
            }
            p.latLon.y = in.nextDouble();
            if (!in.hasNextDouble()) {
                return null;
            }
            p.latLon.x = in.nextDouble();

            if (!in.hasNextDouble()) {
                return null;
            }
            p.dist = in.nextDouble();

            if (!in.hasNext()) {
                return null;
            }
            p.label = in.findInLine("[^\\|]*").trim();
        }

        return p;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (ready) {
            double mapWidth = descriptor.getMapWidth();
            int displayWidth = (int) (mapWidth * paintProps.getZoomLevel());

            double metersPerPixel = displayWidth
                    / paintProps.getCanvasBounds().width;

            double magnification = getCapability(MagnificationCapability.class)
                    .getMagnification();
            /* Distance between points in screen pixels */
            double displayHintSize = this.pixelSizeHint * magnification;
            /* Convert to km and adjust for density. */
            double minSepDist = (displayHintSize * (metersPerPixel / 1000.0))
                    / getCapability(DensityCapability.class).getDensity();

            DrawableString test = new DrawableString("N", null);
            test.font = getFont(target);
            Rectangle2D charSize = target.getStringsBounds(test);
            double charWidth = charSize.getWidth();
            double charHeight = charSize.getHeight();

            double screenToWorldRatio = paintProps.getCanvasBounds().width
                    / paintProps.getView().getExtent().getWidth();

            RGB color = getCapability(ColorableCapability.class).getColor();
            double offsetX = charWidth / 2.0 / screenToWorldRatio;
            double offsetY = charHeight / screenToWorldRatio;
            HorizontalAlignment align = HorizontalAlignment.LEFT;

            PointStyle pointStyle = getCapability(PointCapability.class)
                    .getPointStyle();
            if (pointStyle.equals(PointStyle.NONE)) {
                offsetX = 0;
                offsetY = 0;
                align = HorizontalAlignment.CENTER;
            }
            offsetX += getCapability(LabelableCapability.class).getxOffset()
                    / screenToWorldRatio;
            offsetY -= getCapability(LabelableCapability.class).getyOffset()
                    / screenToWorldRatio;

            double targetWidth = descriptor.getGridGeometry().getGridRange()
                    .getSpan(0);

            boolean isLabeled = getCapability(LabelableCapability.class)
                    .getLabelField() != null;

            List<DrawableString> strings = new ArrayList<>();
            List<double[]> pointList = new ArrayList<>();

            Iterator<LPIPoint> it = points.iterator();
            List<LPIPoint> visiblePoints = new ArrayList<>(512);
            while (it.hasNext()) {
                LPIPoint p = it.next();
                if (p.pixel == null) {
                    continue;
                }
                if (!paintProps.getView().isVisible(p.pixel)) {
                    continue;
                }
                if (p.dist < minSepDist) {
                    break;
                }
                /*
                 * Calculate a real distance to prevent overcrowding in LPI with
                 * unrealistic dist values.
                 */
                boolean tooClose = false;
                for (double[] point : pointList) {
                    double dx = point[0] - p.pixel[0];
                    double dy = point[1] - p.pixel[1];
                    /* Dist in target pixels */
                    double dist = Math.sqrt(dx * dx + dy * dy);
                    /* Convert to km */
                    dist = dist * mapWidth / targetWidth / 1000;
                    if (dist < minSepDist) {
                        tooClose = true;
                        break;
                    }
                }
                if (tooClose) {
                    continue;
                }
                visiblePoints.add(p);
                it.remove();

                pointList.add(p.pixel);

                if (isLabeled && (magnification > 0.0)) {
                    String label = p.label;
                    label = label.replaceAll("([^\n]{3}\\S*)\\s+", "$1\n");
                    DrawableString string = new DrawableString(label, color);
                    string.font = getFont(target);
                    string.setCoordinates(p.pixel[0] + offsetX,
                            p.pixel[1] + offsetY);
                    string.horizontalAlignment = align;
                    string.verticallAlignment = VerticalAlignment.MIDDLE;
                    strings.add(string);
                }
            }
            target.drawStrings(strings);
            target.drawPoints(pointList, color, pointStyle, 1.0f);

            /*
             * Rearrange the points to make the the progressive disclosure more
             * stable. This will cause the next paint to prefer points that were
             * painted this time since they will be closer to the front of the
             * list. This only works if there are many points with the same dist
             * values, otherwise it does nothing.
             */
            points.addAll(0, visiblePoints);
            points.sort(Comparator.comparingDouble((LPIPoint p) -> p.dist)
                    .reversed());
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (points != null) {
            for (LPIPoint p : points) {
                p.pixel = this.descriptor
                        .worldToPixel(new double[] { p.latLon.x, p.latLon.y });
            }
        }
    }

    public int getPixelSizeHint() {
        return pixelSizeHint;
    }

    public void setPixelSizeHint(int pixelSizeHint) {
        this.pixelSizeHint = pixelSizeHint;
    }

}
