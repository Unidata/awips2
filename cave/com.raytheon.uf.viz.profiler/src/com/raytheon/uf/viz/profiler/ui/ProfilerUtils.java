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

package com.raytheon.uf.viz.profiler.ui;

import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.UUID;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerLevel;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PointWindDisplay;
import com.raytheon.viz.pointdata.PointWindDisplay.DisplayType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Collection of static methods used for the Profiler drawing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2009    2219      dhladky     Initial creation
 * Feb 08, 2011    8036      bkowal      Updated GRAPH_OFFSET to come
 *                                       close to centering the graph.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ProfilerUtils {

    public static double[] PRESSURES = new double[] { 10, 30, 50, 70, 100, 150,
            200, 250, 300, 400, 500, 700, 850, 1000 };

    public static int HEIGHTS = 18;

    public static String UNITS = "(kts)";

    public static final int TOP_WIND = 160;

    public static final RGB GRAPH_COLOR = new RGB(255, 255, 255);

    public static final int GRAPH_OFFSET_X = 50;

    public static final int GRAPH_OFFSET_Y = 145;

    public static final int GRAPH_HEIGHT = 700;

    public static final int GRAPH_WIDTH = 900;

    public static final int YLABEL_INC = 1000;

    public static final int LABEL_OFFSET = 10;

    public static final int TITLE_OFFSET = 20;

    public static final int XLABEL_INC = 1;

    public static final int Y_OFFSET = 10;

    public static final int X_OFFSET = 1;

    public static final int BARB_SIZE = 75;

    public static final int BARB_LEN = 30;

    public static final int GRAPH_LINE_WIDTH = 2;

    public static final int PROFILE_LINE_WIDTH = 1;

    public static final RGB LIGHT_GREEN = new RGB(0, 251, 144);

    public static final RGB MED_GREEN = new RGB(0, 187, 0);

    public static final RGB GREEN = new RGB(0, 255, 0);

    public static final RGB LIGHT_YELLOW = new RGB(255, 255, 112);

    public static final RGB YELLOW = new RGB(255, 255, 0);

    public static final RGB DARK_YELLOW = new RGB(208, 208, 96);

    public static final RGB LIGHT_RED = new RGB(255, 96, 96);

    public static final RGB RED = new RGB(255, 0, 0);

    public static final RGB MED_RED = new RGB(218, 0, 0);

    public static final RGB DARK_RED = new RGB(174, 0, 0);

    public static final RGB LIGHT_BLUE = new RGB(0, 224, 255);

    public static final RGB PURPLE = new RGB(231, 0, 255);

    public static final RGB MED_PURPLE = new RGB(255, 112, 255);

    public static final RGB BLACK = new RGB(0, 0, 0);

    public static final RGB ORANGE = new RGB(255, 165, 0);

    public static final RGB MED_GRAY = new RGB(119, 119, 144);

    public static final RGB DARK_GRAY = new RGB(118, 118, 118);

    public static final RGB LIGHT_PINK = new RGB(255, 170, 170);

    public static final RGB MED_PINK = new RGB(238, 140, 140);

    public static final RGB DARK_PINK = new RGB(201, 112, 112);

    public static final RGB BLUE = new RGB(0, 0, 255);

    public static final RGB WHITE = new RGB(255, 255, 255);

    public static final Rectangle profilerRectangle = new Rectangle(
            ProfilerUtils.GRAPH_OFFSET_X, ProfilerUtils.GRAPH_OFFSET_Y,
            ProfilerUtils.GRAPH_WIDTH, ProfilerUtils.GRAPH_HEIGHT);

    public static final String formatString = "yyyy-MM-dd HH:mm";

    public static final String formatHourString = "HH:mm";

    public static final SimpleDateFormat dateFormat = new SimpleDateFormat(
            formatString);

    public static final SimpleDateFormat labelFormat = new SimpleDateFormat(
            formatHourString);

    public static final DecimalFormat decimalFormat = new DecimalFormat();

    public static final float[] colorLabels = new float[] { 0.0f, 20.0f, 40.0f,
            60.0f, 80.0f, 100.0f, 120.0f, 140.0f };

    public static final Float[] colorRange = new Float[] { 0.0f, 156.2f };

    public static final int COLORS = 252;

    /**
     * 
     * Container object for returning plot objects
     * 
     * @author chammack
     * @version 1.0
     */
    public static class PlotObject {
        /** The plot image */
        public IImage image;

        /** The location on the map in world coords */
        public Coordinate coord;

        /** The offset in pixel space */
        public int[] pixelOffset;
    }

    /**
     * Create a set of plot objects from the ProfilerObs, making a table
     * 
     * @param packet
     * @param target
     * @param gridGeometry
     * @param descriptor
     * @return
     * @throws VizException
     */
    public static PlotObject createWindBarb(ProfilerLevel level,
            IGraphicsTarget target, IDescriptor descriptor, Coordinate point,
            IColorMap colorMap) throws VizException {

        PointWindDisplay barb = new PointWindDisplay(BARB_LEN, 1.0, 0,
                BARB_SIZE / 20);
        barb.setImageParameters(BARB_SIZE, BARB_SIZE, 255, 255, 255,
                ProfilerUtils.PROFILE_LINE_WIDTH);

        double u = mpsToKts(level.getUcWind());
        double v = mpsToKts(level.getVcWind());
        double windSpeed = getWindSpeed(u, v);

        PlotObject po = null;
        double windratio = windSpeed / TOP_WIND;
        int index = new Double(windratio * colorMap.getSize()).intValue();
        if (index > colorMap.getSize()) {
            index = colorMap.getSize() - 1;
        }
        Color color = colorMap.getColors().get(index);
        barb.setColor(convert(color));
        barb.setWind(u, v, false);
        BufferedImage img = barb.getWindImage(false, DisplayType.BARB, 1);
        if (target != null) {
            IImage image = convertBufferedImage(target, img, UUID.randomUUID()
                    .toString());
            po = new PlotObject();
            po.image = image;

            ReferencedCoordinate rc = new ReferencedCoordinate(new Coordinate(
                    point.x, point.y), descriptor.getGridGeometry(),
                    Type.GRID_CENTER);
            try {
                po.coord = rc.asPixel(descriptor.getGridGeometry());
                po.pixelOffset = new int[] { 0, 0 };
            } catch (Exception e) {
                throw new VizException(
                        "Unable to transform Profile Coordinate", e);
            }
        }

        return po;
    }

    /**
     * Converting the buffered image to a graphics card image
     * 
     * @param target
     * @param img
     * @param name
     * @return
     */
    public static IImage convertBufferedImage(IGraphicsTarget target,
            BufferedImage img, String name) throws VizException {
        return target.initializeRaster(new IODataPreparer(img, name, 0), null);
    }

    /**
     * gets the windSpeed
     * 
     * @param u
     * @param v
     * @return
     */
    public static double getWindSpeed(double u, double v) {
        return Math.sqrt((u * u) + (v * v));
    }

    /**
     * convert color
     * 
     * @param color
     * @return
     */
    public static Color convert(RGB color) {
        float blue;
        float green;
        float red;
        Color returnColor = null;
        if (color != null) {
            blue = new Float(color.blue / 255.0).floatValue();
            green = new Float(color.green / 255.0).floatValue();
            red = new Float(color.red / 255.0).floatValue();
            returnColor = new Color(red, green, blue);
        }

        return returnColor;
    }

    /**
     * convert color
     * 
     * @param color
     * @return
     */
    public static RGB convert(Color color) {
        int blue;
        int green;
        int red;
        RGB returnColor = null;
        if (color != null) {
            blue = new Float(color.getBlue() * 255.0).intValue();
            green = new Float(color.getGreen() * 255.0).intValue();
            red = new Float(color.getRed() * 255.0).intValue();
            returnColor = new RGB(red, green, blue);
        }

        return returnColor;
    }

    /**
     * Gets a color Map
     * 
     * @return
     */
    public static ColorMap getColorMap() {
        // LIGHT_GREEN > YELLOW > ORANGE > RED > VIOLET > LIGHT_GREEN
        int changes_per_gap = 9;
        int color_gaps = 5;
        int slow_phaser = new Double(51.0 / changes_per_gap).intValue();
        int mid_phaser = new Double(153.0 / changes_per_gap).intValue();
        int fast_phaser = new Double(250.0 / changes_per_gap).intValue();

        int[] greens = new int[changes_per_gap * color_gaps];
        int[] reds = new int[changes_per_gap * color_gaps];
        int[] blues = new int[changes_per_gap * color_gaps];
        // starter
        int green = 250;
        int red = 51;
        int blue = 0;
        int k = 1;
        int j = 0;

        // fade to yellow
        for (int i = 0; i < changes_per_gap; i++) {
            red += mid_phaser;
            reds[i] = red;
            green -= slow_phaser;
            greens[i] = green;
            blues[i] = blue;
            j++;
        }
        k++;
        // fade to orange
        for (int i = j; i < changes_per_gap * k; i++) {
            green -= slow_phaser;
            greens[i] = green;
            reds[i] = red;
            blues[i] = blue;
            j++;
        }
        k++;
        // / fade to red
        for (int i = j; i < changes_per_gap * k; i++) {
            green -= mid_phaser;
            greens[i] = green;
            red += slow_phaser;
            reds[i] = red;
            blues[i] = blue;
            j++;
        }
        k++;
        // fade to violet
        for (int i = j; i < changes_per_gap * k; i++) {
            greens[i] = green;
            red -= slow_phaser;
            reds[i] = red;
            blue += fast_phaser;
            blues[i] = blue;
            j++;
        }
        k++;
        // fade to light_green
        for (int i = j; i < changes_per_gap * k; i++) {
            green += mid_phaser;
            greens[i] = green;
            red -= mid_phaser;
            reds[i] = red;
            blue -= fast_phaser;
            blues[i] = blue;
            j++;
        }

        float[] fgreens = new float[changes_per_gap * color_gaps];
        float[] freds = new float[changes_per_gap * color_gaps];
        float[] fblues = new float[changes_per_gap * color_gaps];

        // convert them
        for (int m = 0; m < changes_per_gap * color_gaps; m++) {
            RGB rgb = new RGB(reds[m], greens[m], blues[m]);
            Color color = convert(rgb);
            fgreens[m] = color.getGreen();
            freds[m] = color.getRed();
            fblues[m] = color.getBlue();
        }

        return new ColorMap("Profiler Map", freds, fgreens, fblues);
    }

    /**
     * Convert a wind speed in knots to meters per second.
     * 
     * @param value
     *            A value in knots.
     * @return Speed converted to meters per second.
     */
    public static double mpsToKts(double value) {
        return value * 1.94384449;
    }
}
