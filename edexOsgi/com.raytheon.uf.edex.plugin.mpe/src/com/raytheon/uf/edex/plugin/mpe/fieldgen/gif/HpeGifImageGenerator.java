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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.gif;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Iterator;

import javax.imageio.ImageIO;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.GeoData;
import com.raytheon.uf.edex.plugin.mpe.geo.Town;
import org.locationtech.jts.geom.Coordinate;

/**
 * Generates a gif image based on a mpe fieldgen mosaic. Based on:
 * hpe_fieldgen/TEXT/save_gif.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HpeGifImageGenerator {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final NumberFormat LEVEL_FORMAT = new DecimalFormat("0.00");

    private static final String GIF_EXTENSION = "gif";

    private final GeoData geoData;

    private final Rectangle geoGridData;

    private final double[][] mosaic;

    private final String formattedXmrgDate;

    private final double autoGraphicScale;

    private final LegendDataLevelRange legendDataLevelRange;

    private final Font mediumBoldFont;

    private final Font smallFont;

    private final Color lemonChiffon;

    private final Color wheat;

    private final Color ivory;

    private final ImageAttributes imageAttributes;

    public HpeGifImageGenerator(final GeoData geoData,
            final Rectangle geoGridData, final double[][] mosaic,
            final String formattedXmrgDate, final double autoGraphicScale) {
        this.geoData = geoData;
        this.geoGridData = geoGridData;
        this.mosaic = mosaic;
        this.formattedXmrgDate = formattedXmrgDate;
        this.autoGraphicScale = autoGraphicScale;

        /*
         * Prepare colors and fonts.
         */
        legendDataLevelRange = initDataColors();
        mediumBoldFont = new Font(Font.MONOSPACED, Font.BOLD, 12);
        smallFont = new Font(Font.MONOSPACED, Font.BOLD, 9);
        lemonChiffon = new Color((int) (DataLevel.COLOR_FACTOR * 1000),
                (int) (DataLevel.COLOR_FACTOR * 973),
                (int) (DataLevel.COLOR_FACTOR * 776));
        wheat = new Color((int) (DataLevel.COLOR_FACTOR * 961),
                (int) (DataLevel.COLOR_FACTOR * 871),
                (int) (DataLevel.COLOR_FACTOR * 702));
        ivory = new Color((int) (DataLevel.COLOR_FACTOR * 1000),
                (int) (DataLevel.COLOR_FACTOR * 1000),
                (int) (DataLevel.COLOR_FACTOR * 941));

        /*
         * Determine the size of the image, etc.
         */
        imageAttributes = initImageAttributes();
    }

    public void generate(final Path outputGifPath) {
        /*
         * Prepare the image.
         */
        final BufferedImage bi = new BufferedImage(imageAttributes.getWidth(),
                imageAttributes.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g2d = bi.createGraphics();

        /*
         * Set the image background.
         */
        g2d.setBackground(Color.BLACK);
        g2d.clearRect(0, 0, imageAttributes.getWidth(),
                imageAttributes.getHeight());

        renderMosaicData(g2d);
        renderGeoData(g2d);
        renderLegend(g2d);

        try {
            ImageIO.write(bi, GIF_EXTENSION, outputGifPath.toFile());
            logger.info("Successfully wrote gif file: {}.",
                    outputGifPath.toString());
        } catch (IOException e) {
            logger.error("Failed to write gif file: " + outputGifPath.toString()
                    + ".", e);
        }
    }

    private LegendDataLevelRange initDataColors() {
        LegendDataLevelRange legendDataLevelRange = new LegendDataLevelRange();
        legendDataLevelRange.addDataLevel(new DataLevel(0.00, 0, 0, 0));
        legendDataLevelRange.addDataLevel(new DataLevel(0.01, 396, 451, 514));
        legendDataLevelRange.addDataLevel(new DataLevel(0.10, 600, 600, 600));
        legendDataLevelRange.addDataLevel(new DataLevel(0.20, 776, 886, 1000));
        legendDataLevelRange.addDataLevel(new DataLevel(0.30, 529, 808, 980));
        legendDataLevelRange.addDataLevel(new DataLevel(0.40, 361, 702, 1000));
        legendDataLevelRange.addDataLevel(new DataLevel(0.50, 188, 431, 1000));
        legendDataLevelRange.addDataLevel(new DataLevel(0.75, 243, 663, 624));
        legendDataLevelRange.addDataLevel(new DataLevel(1.00, 529, 969, 90));
        legendDataLevelRange.addDataLevel(new DataLevel(1.25, 694, 984, 90));
        legendDataLevelRange.addDataLevel(new DataLevel(1.50, 1000, 988, 90));
        legendDataLevelRange.addDataLevel(new DataLevel(1.75, 831, 627, 90));
        legendDataLevelRange.addDataLevel(new DataLevel(2.00, 718, 678, 349));
        legendDataLevelRange.addDataLevel(new DataLevel(2.50, 973, 502, 90));
        return legendDataLevelRange;
    }

    private ImageAttributes initImageAttributes() {
        ImageAttributes imageAttributes = new ImageAttributes();
        imageAttributes.setScaleX(autoGraphicScale);
        imageAttributes.setXor(geoGridData.x);
        imageAttributes.setYor(geoGridData.y);
        imageAttributes.setHrapX(geoGridData.width);
        imageAttributes.setHrapY(geoGridData.height);
        imageAttributes.setUnitsFactor(25.4);
        imageAttributes.setScaleFactor(100.0);
        imageAttributes.setZoomFactor(1.0);
        imageAttributes.setOrigX(0.0);
        imageAttributes.setOrigY(0.0);
        imageAttributes.setScaleY(imageAttributes.getScaleX());
        imageAttributes.setOffset(50);
        final int width = (int) (imageAttributes.getOffset()
                + imageAttributes.getScaleX() * (2 + imageAttributes.hrapX));
        imageAttributes.setWidth(width);
        final int height = (int) (imageAttributes.getOffset()
                + imageAttributes.getScaleY()
                        * (2 + imageAttributes.getHrapY()));
        imageAttributes.setHeight(height);
        final float slope = (float) (imageAttributes.getHrapX()
                / imageAttributes.getHrapY());
        imageAttributes.setSlope(slope);
        return imageAttributes;
    }

    private void renderMosaicData(final Graphics2D g2d) {
        for (int i = 0; i < geoGridData.getWidth(); i++) {
            int x = (int) (((i * imageAttributes.scaleX)
                    - (int) (imageAttributes.origX))
                    * imageAttributes.zoomFactor);
            for (int j = 0; j < geoGridData.getHeight(); j++) {
                int y = (int) ((((imageAttributes.hrapY
                        - (j - imageAttributes.origY)) * imageAttributes.scaleY)
                        - (int) (imageAttributes.origY))
                        * imageAttributes.zoomFactor);

                short precipAmt = (short) (mosaic[j][i]
                        * imageAttributes.scaleFactor);
                final Color legendColor = legendDataLevelRange
                        .lookupLegendColor(imageAttributes.scaleFactor,
                                precipAmt);
                g2d.setColor(legendColor);
                g2d.fillRect(x, (y - 2), 3, 3);
            }
        }
    }

    private void renderGeoData(final Graphics2D g2d) {
        /*
         * Overlay RFC Basin Boundary.
         */
        final Polygon rfcBoundaryPolygon = new Polygon();
        for (Coordinate coord : geoData.getRfcBoundaryOverlay().getHrap()) {
            double x = (((coord.x - imageAttributes.xor)
                    * imageAttributes.scaleX) - imageAttributes.origX)
                    * imageAttributes.zoomFactor;
            double y = (((imageAttributes.hrapY
                    - (coord.y - imageAttributes.yor)) * imageAttributes.scaleY)
                    - imageAttributes.origY) * imageAttributes.zoomFactor;
            x = Math.min(x, 3000);
            x = Math.max(x, -500);
            y = Math.min(y, 2000);
            y = Math.max(y, -500);
            rfcBoundaryPolygon.addPoint((int) x, (int) y);
        }
        g2d.setColor(wheat);
        g2d.drawPolygon(rfcBoundaryPolygon);

        /*
         * Overlay towns
         */
        g2d.setFont(mediumBoldFont);
        for (Town town : geoData.getTownGeoData().getTowns()) {
            final Coordinate coord = town.getHrapCoord();
            double x = ((coord.x - imageAttributes.xor) * imageAttributes.scaleX
                    - imageAttributes.origX * imageAttributes.scaleX)
                    * imageAttributes.zoomFactor;
            double y = ((imageAttributes.hrapY - (coord.y - imageAttributes.yor)
                    - imageAttributes.scaleY) * imageAttributes.scaleY
                    - (imageAttributes.origY) * imageAttributes.scaleY)
                    * imageAttributes.zoomFactor;
            x = Math.min(x, 3000);
            x = Math.max(x, -500);
            y = Math.min(y, 2000);
            y = Math.max(y, -500);
            g2d.setColor(ivory);
            g2d.drawArc((int) x, (int) y, 2, 2, 0, 360);

            g2d.setColor(Color.WHITE);
            g2d.drawString(town.getName(), (int) (x + 1), (int) (y + 1));
        }
    }

    private void renderLegend(final Graphics2D g2d) {
        Iterator<DataLevel> dataLevelItr = legendDataLevelRange
                .getDataLevelIterator();
        g2d.setFont(smallFont);
        g2d.setColor(Color.BLACK);
        int counter = 0;
        final int legendItemSpan = (int) ((0.11 * imageAttributes.height)
                - (0.07 * imageAttributes.height));
        if (imageAttributes.getSlope() < 1.0) {
            g2d.fillRect((int) (0.90 * imageAttributes.width), 0,
                    (int) imageAttributes.width,
                    (int) (0.90 * imageAttributes.height));

            while (dataLevelItr.hasNext()) {
                double ul = 0.07 + (counter * 0.04);

                DataLevel dataLevel = dataLevelItr.next();
                g2d.setColor(dataLevel.getLegendColor());
                g2d.fillRect((int) (0.91 * imageAttributes.width),
                        (int) ((ul * imageAttributes.height) - 9),
                        (int) (imageAttributes.width
                                - (0.96 * imageAttributes.width)),
                        legendItemSpan);

                g2d.setColor(Color.WHITE);
                g2d.drawString(LEVEL_FORMAT.format(dataLevel.getBegin()),
                        (int) (0.96 * imageAttributes.width),
                        (int) (ul * imageAttributes.height));

                ++counter;
            }
        } else {
            g2d.fillRect(0, (int) (0.90 * imageAttributes.height),
                    (int) imageAttributes.width,
                    (int) (imageAttributes.height));

            while (dataLevelItr.hasNext()) {
                double ul = 0.07 + (counter * 0.04);

                DataLevel dataLevel = dataLevelItr.next();
                g2d.setColor(dataLevel.getLegendColor());
                g2d.fillRect((int) (ul * imageAttributes.width),
                        (int) ((0.92 * imageAttributes.height)),
                        (int) (imageAttributes.width
                                - (0.96 * imageAttributes.width)),
                        legendItemSpan);

                g2d.setColor(Color.WHITE);
                g2d.drawString(LEVEL_FORMAT.format(dataLevel.getBegin()),
                        (int) (ul * imageAttributes.width),
                        (int) (0.91 * imageAttributes.height));

                ++counter;
            }
        }

        g2d.setColor(lemonChiffon);
        g2d.setFont(mediumBoldFont);
        if (imageAttributes.getSlope() < 1.0) {
            g2d.drawString("RFCWide", (int) (0.80 * imageAttributes.width),
                    (int) (0.91 * imageAttributes.height));
            g2d.drawString(formattedXmrgDate + "z",
                    (int) (0.80 * imageAttributes.width),
                    (int) (0.95 * imageAttributes.height));
        } else {
            g2d.drawString("RFCWide", (int) (0.77 * imageAttributes.width),
                    (int) (0.95 * imageAttributes.height));
            g2d.drawString(formattedXmrgDate + "z",
                    (int) (0.84 * imageAttributes.width),
                    (int) (0.95 * imageAttributes.height));
        }
    }

    protected final class ImageAttributes {

        private int width;

        private int height;

        private int xor;

        private int yor;

        private int hrapX;

        private int hrapY;

        private float slope;

        private double zoomFactor;

        private double origX;

        private double origY;

        private double scaleX;

        private double scaleY;

        private double unitsFactor;

        private double scaleFactor;

        private int offset;

        public int getWidth() {
            return width;
        }

        public void setWidth(int width) {
            this.width = width;
        }

        public int getHeight() {
            return height;
        }

        public void setHeight(int height) {
            this.height = height;
        }

        public int getXor() {
            return xor;
        }

        public void setXor(int xor) {
            this.xor = xor;
        }

        public int getYor() {
            return yor;
        }

        public void setYor(int yor) {
            this.yor = yor;
        }

        public int getHrapX() {
            return hrapX;
        }

        public void setHrapX(int hrapX) {
            this.hrapX = hrapX;
        }

        public int getHrapY() {
            return hrapY;
        }

        public void setHrapY(int hrapY) {
            this.hrapY = hrapY;
        }

        public float getSlope() {
            return slope;
        }

        public void setSlope(float slope) {
            this.slope = slope;
        }

        public double getZoomFactor() {
            return zoomFactor;
        }

        public void setZoomFactor(double zoomFactor) {
            this.zoomFactor = zoomFactor;
        }

        public double getOrigX() {
            return origX;
        }

        public void setOrigX(double origX) {
            this.origX = origX;
        }

        public double getOrigY() {
            return origY;
        }

        public void setOrigY(double origY) {
            this.origY = origY;
        }

        public double getScaleX() {
            return scaleX;
        }

        public void setScaleX(double scaleX) {
            this.scaleX = scaleX;
        }

        public double getScaleY() {
            return scaleY;
        }

        public void setScaleY(double scaleY) {
            this.scaleY = scaleY;
        }

        public double getUnitsFactor() {
            return unitsFactor;
        }

        public void setUnitsFactor(double unitsFactor) {
            this.unitsFactor = unitsFactor;
        }

        public double getScaleFactor() {
            return scaleFactor;
        }

        public void setScaleFactor(double scaleFactor) {
            this.scaleFactor = scaleFactor;
        }

        public int getOffset() {
            return offset;
        }

        public void setOffset(int offset) {
            this.offset = offset;
        }

    }
}