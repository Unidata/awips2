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
package com.raytheon.viz.hydro.gagedisplay;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydrocommon.data.GageData;

public class HydroImageMaker {

    public static enum ImageSize {
        VERY_SMALL(11, 20), SMALL(13, 25), MEDIUM(15, 30), LARGE(17, 35);

        private final int width;

        private final int height;

        ImageSize(int width, int height) {
            this.width = width;
            this.height = height;
        }

        public int getWidth() {
            return width;
        }

        public int getHeight() {
            return height;
        }
    }

    /**
     * 
     * @param gage
     *            The GageData object
     * @param size
     *            The size of the image
     * @return
     */
    public static BufferedImage getImage(GageData gage, ImageSize size) {
        return getImage(gage.getDispClass(), size, gage.getColor());
    }

    /**
     * The basic class you call when you need a HydroImage
     * 
     * @param gage
     *            The GageData object
     * @param size
     *            The size of the image
     * @param rgb
     *            An override to the GageData color
     * @return
     */
    public static BufferedImage getImage(GageData gage, ImageSize size, RGB rgb) {
        return getImage(gage.getDispClass(), size, rgb);
    }

    /**
     * 
     * @param gageDispClass
     *            The GageData object dispClass
     * @param size
     *            The size of the image
     * @return
     */
    public static BufferedImage getImage(String gageDispClass, ImageSize size) {
        return getImage(gageDispClass, size, null);
    }

    /**
     * The basic class you call when you need a HydroImage
     * 
     * @param gageDispClass
     *            The GageData object dispClass
     * @param size
     *            The size of the image
     * @param rgb
     *            An override to the GageData color
     * @return
     */
    public static BufferedImage getImage(String gageDispClass, ImageSize size,
            RGB rgb) {
        boolean isFcstPt = false;
        boolean isReservoir = false;
        boolean isRiver = false;
        boolean isNonRiver = false;
        boolean isUnknown = false;

        if (gageDispClass != null) {
            if (gageDispClass.contains(GageData.OFFICIAL_RIVER)) { // F
                isFcstPt = true;
            } else {
                isFcstPt = false;
            }

            if (gageDispClass.contains(GageData.RESERVOIR)) { // D
                isReservoir = true;
            } else {
                isReservoir = false;
            }

            if (gageDispClass.contains(GageData.RIVER) || // R
                    isFcstPt || isReservoir) {
                isRiver = true;
            } else {
                isRiver = false;
            }

            if (gageDispClass.contains(GageData.PRECIP)
                    || gageDispClass.contains(GageData.SNOW)
                    || gageDispClass.contains(GageData.TEMPERATURE)
                    || gageDispClass.contains(GageData.OTHER)) {
                isNonRiver = true;
            } else {
                isNonRiver = false;
            }

            if (gageDispClass.contains(GageData.UNKNOWN)) {
                isUnknown = true;
            } else {
                isUnknown = false;
            }
        }

        BufferedImage img = new BufferedImage(size.getWidth(),
                size.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Color color = convert(rgb);
        if (isRiver) {
            drawRiverDataPoint(img, color);
        }
        if (isFcstPt) {
            drawForecastCircle(img, color);
        }
        if (isReservoir) {
            drawReservoirLine(img, color);
        }
        if (isNonRiver) {
            drawMeteorologicalStationPoint(img);
        }
        if (isUnknown) {
            drawUnknown(img, color);
        }

        return img;
    }

    /**
     * creates the river basic data point
     * 
     * @param color
     * @return
     */
    private static void drawRiverDataPoint(BufferedImage img, Color color) {
        int width = img.getWidth();
        int height = img.getHeight();
        Graphics2D g = img.createGraphics();
        // this makes it transparent
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f));
        Rectangle2D.Double rect = new Rectangle2D.Double(0, 0, img.getWidth(),
                img.getHeight());
        g.fill(rect);

        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(color);
        // make a triangle of chosen color
        Polygon poly = new Polygon();
        poly.addPoint(1, height - height / 5);
        poly.addPoint(width / 2, height / 4);
        poly.addPoint(width - 1, height - height / 5);
        poly.addPoint(1, height - height / 5);

        g.fillPolygon(poly);
        g.dispose();
    }

    /**
     * Creates the meteorological station point
     * 
     * @param color
     * @return
     */
    private static void drawMeteorologicalStationPoint(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();
        // make circle in center
        Graphics2D g = img.createGraphics();
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        // always red
        g.setColor(new Color(255, 0, 0));
        g.fillOval(width / 2 - (height / 6 / 2), height / 2, height / 6,
                height / 6);
        g.dispose();
    }

    /**
     * convert color
     * 
     * @param color
     * @return
     */
    private static Color convert(RGB color) {
        int blue;
        int green;
        int red;
        Color returnColor;
        if (color != null) {
            blue = color.blue;
            green = color.green;
            red = color.red;
            returnColor = new Color(red, green, blue);
        } else {
            blue = PDCConstants.DEFAULT_COLOR.blue;
            green = PDCConstants.DEFAULT_COLOR.green;
            red = PDCConstants.DEFAULT_COLOR.red;
            returnColor = new Color(red, green, blue);
        }

        return returnColor;
    }

    /**
     * Draw the forecast hat
     * 
     * @param image
     * @return
     */
    private static void drawForecastCircle(BufferedImage image, Color color) {
        int width = image.getWidth();
        int height = image.getHeight();
        // add the circle to the top
        Graphics2D g = image.createGraphics();
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(color);
        g.setStroke(new BasicStroke(2));
        g.drawOval(width / 2 - (height / 6 / 2), height / 7, height / 6,
                height / 6);
        g.dispose();
    }

    /**
     * draw the line at the bottom of reservoir icons.
     * 
     * @param image
     * @return
     */
    private static void drawReservoirLine(BufferedImage image, Color color) {
        int width = image.getWidth();
        int height = image.getHeight();
        Graphics2D g = image.createGraphics();
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(color);
        g.setStroke(new BasicStroke(2));
        g.drawLine(1, height - height / 7, width, height - height / 7);
        g.dispose();
    }

    private static void drawUnknown(BufferedImage image, Color color) {
        int height = image.getHeight();
        Graphics2D g = image.createGraphics();
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(color);
        g.setStroke(new BasicStroke(2));
        g.fillOval(0, height / 4, height / 2, height / 2);
        g.dispose();
    }

    /**
     * Create the Dam icons.
     * 
     * @return
     */
    public static BufferedImage getDamIcon() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String iconColor = appsDefaults.getToken("dam_icon_color");
        RGB rgb = RGBColors.getRGBColor(iconColor);
        Color damColor = new Color(rgb.red / 255f, rgb.green / 255f,
                rgb.blue / 255f);
        BufferedImage damImage = new BufferedImage(15, 15,
                BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = damImage.createGraphics();
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f));
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(damColor);
        g.fillArc(1, 1, 15, 8, 90, -180);
        g.dispose();
        return damImage;
    }
}