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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.graphics.KmlFont;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;
import com.vividsolutions.jts.geom.Coordinate;

import de.micromata.opengis.kml.v_2_2_0.ScreenOverlay;
import de.micromata.opengis.kml.v_2_2_0.Units;
import de.micromata.opengis.kml.v_2_2_0.Vec2;

/**
 * Converts canvas rendering into KML screen overlays.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 26, 2012           bsteffen     Initial creation
 * Jan 14, 2013  2313     bsteffen     Add image rendering
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlCanvasRenderingExtension extends
        GraphicsExtension<KmlGraphicsTarget> implements
        ICanvasRenderingExtension {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlCanvasRenderingExtension.class);

    @Override
    public void drawStrings(PaintProperties paintProps,
            DrawableString... parameters) throws VizException {
        getGenerator(paintProps).addStrings(parameters);
    }

    @Override
    public void drawLines(PaintProperties paintProps,
            DrawableLine... parameters) throws VizException {
        getGenerator(paintProps).addLines(parameters);
    }

    @Override
    public void drawColorRamp(PaintProperties paintProps,
            DrawableColorMap colorMap) throws VizException {
        getGenerator(paintProps).addColorMaps(colorMap);
    }

    @Override
    public void drawImages(PaintProperties paintProps, DrawableImage... images)
            throws VizException {
        getGenerator(paintProps).addImages(images);

    }

    protected Generator getGenerator(PaintProperties paintProps) {
        for (KmlFeatureGenerator generator : target.getGenerators()) {
            if (generator instanceof Generator) {
                return (Generator) generator;
            }
        }
        Generator generator = new Generator(paintProps.getCanvasBounds());
        target.addGenerator(generator);
        return generator;
    }

    @Override
    public int getCompatibilityValue(KmlGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    /**
     * 
     * Renders all canvas drawables to one or more screen overlays. The number
     * of screen overlays is determined by overlapping the rendering area all
     * objects and drawing any overlapping objects to a single overlay. This
     * provides a good compromise between a single overlay which does not work
     * well on different sized clients and one screen overlay per object, which
     * causes problems when nearby objects do not line up(such as radar tables).
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jun 26, 2012            bsteffen     Initial creation
     * 
     * </pre>
     * 
     * @author bsteffen
     * @version 1.0
     */
    private static class Generator extends KmlFeatureGenerator {

        private final Rectangle canvasBounds;

        private final List<Object> objects = new ArrayList<Object>();

        public Generator(Rectangle canvasBounds) {
            this.canvasBounds = canvasBounds;
        }

        public void addStrings(DrawableString... strings) {
            this.objects.addAll(Arrays.asList(strings));
        }

        public void addLines(DrawableLine... lines) {
            this.objects.addAll(Arrays.asList(lines));
        }

        public void addColorMaps(DrawableColorMap... colorMaps) {
            this.objects.addAll(Arrays.asList(colorMaps));
        }

        public void addImages(DrawableImage... colorMaps) {
            this.objects.addAll(Arrays.asList(colorMaps));
        }

        @Override
        public void addFeature(KmlOutputManager outputManager) {
            Map<Object, Rectangle2D> boundsMap = new IdentityHashMap<Object, Rectangle2D>();
            List<Rectangle2D> combinedBounds = new ArrayList<Rectangle2D>();
            for (Object object : objects) {
                Rectangle2D bounds = getBounds(object);
                boundsMap.put(object, bounds);
                addBounds(combinedBounds, bounds);
            }
            for (Rectangle2D bounds : combinedBounds) {
                BufferedImage bi = new BufferedImage((int) Math.ceil(bounds
                        .getWidth()), (int) Math.ceil(bounds.getHeight()),
                        BufferedImage.TYPE_INT_ARGB);
                Graphics graphics = bi.getGraphics();
                for (Object object : objects) {
                    if (bounds.contains(boundsMap.get(object))) {
                        draw(graphics, bounds, object);
                    }
                }
                graphics.dispose();
                graphics.finalize();
                bi.flush();
                ScreenOverlay overlay = new ScreenOverlay();
                overlay.setName("ScreenOverlay");
                Vec2 overlayxy = overlay.createAndSetOverlayXY();
                overlayxy.withXunits(Units.FRACTION).withYunits(Units.FRACTION);
                Vec2 screenxy = overlay.createAndSetScreenXY();
                screenxy.withXunits(Units.FRACTION).withYunits(Units.FRACTION);

                /*
                 * This is fairly complex placement code but it produces rather
                 * nice results on any size of display. Basically if something
                 * is flush against either the left or right of the screen it
                 * ends up anchored on that side, if some is smack in the center
                 * than the anchor point is in the middle of both the overlay
                 * and the display. Now if an object is offcenter to the left or
                 * right than the anchor point moves in the correct direction in
                 * proportion to how far off center it is, its hard to explain
                 * without a picture but at the end of the day it produces a
                 * nice result.
                 */
                double leftFrac = bounds.getMinX() / canvasBounds.width;
                double rightFrac = 1.0 - bounds.getMaxX() / canvasBounds.width;
                if (leftFrac < rightFrac) {
                    overlayxy.setX(leftFrac / rightFrac / 2);
                } else {
                    overlayxy.setX(1.0 - rightFrac / leftFrac / 2);
                }
                double x = (bounds.getX() + bounds.getWidth()
                        * overlayxy.getX())
                        / canvasBounds.width;
                screenxy.setX(x);

                double topFrac = bounds.getMinY() / canvasBounds.height;
                double botFrac = 1.0 - bounds.getMaxY() / canvasBounds.height;
                if (topFrac < botFrac) {
                    overlayxy.setY(1.0 - topFrac / botFrac / 2);
                } else {
                    overlayxy.setY(botFrac / topFrac / 2);
                }
                /*
                 * all this 1.0 - stuff is because the math is mostly assuming 0
                 * is up and 1.0 is down but kml expects the opposite.
                 */
                double y = 1.0
                        - (bounds.getY() + bounds.getHeight()
                                * (1.0 - overlayxy.getY()))
                        / canvasBounds.height;
                screenxy.setY(y);

                overlay.createAndSetIcon().setHref(
                        outputManager.addImage(bi, "ScreenOverlay.png"));
                outputManager.addFeature(overlay);
            }
        }

        private void addBounds(List<Rectangle2D> allBounds, Rectangle2D bounds) {
            Iterator<Rectangle2D> it = allBounds.iterator();
            while (it.hasNext()) {
                Rectangle2D b = it.next();
                if (b.intersects(bounds)) {
                    if (!b.contains(bounds)) {
                        /*
                         * need to recheck the larger rectangle of b for any new
                         * intersections.
                         */
                        it.remove();
                        b.add(bounds);
                        addBounds(allBounds, b);
                    }
                    return;
                }
            }
            allBounds.add(bounds);
        }

        protected void draw(Graphics graphics, Rectangle2D imageBounds,
                Object object) {
            if (object instanceof DrawableString) {
                drawStrings(graphics, imageBounds, (DrawableString) object);
            } else if (object instanceof DrawableLine) {
                drawLines(graphics, imageBounds, (DrawableLine) object);
            } else if (object instanceof DrawableColorMap) {
                drawColorMap(graphics, imageBounds, (DrawableColorMap) object);
            } else if (object instanceof DrawableImage) {
                drawImage(graphics, imageBounds, (DrawableImage) object);
            }
        }

        protected void drawStrings(Graphics graphics, Rectangle2D imageBounds,
                DrawableString string) {
            KmlFont kmlFont = (KmlFont) string.font;
            if (kmlFont == null) {
                kmlFont = new KmlFont();
            }
            graphics.setFont(kmlFont.getFont());
            double x = (string.basics.x - imageBounds.getX());
            double y = (string.basics.y - imageBounds.getY());
            String[] text = string.getText();
            RGB[] colors = string.getColors();
            for (int i = 0; i < text.length; i += 1) {
                Rectangle2D bounds = graphics.getFontMetrics().getStringBounds(
                        text[i], graphics);
                double realX = x;
                double realY = y;
                if (HorizontalAlignment.RIGHT == string.horizontalAlignment) {
                    realX -= bounds.getWidth();
                } else if (HorizontalAlignment.CENTER == string.horizontalAlignment) {
                    realX -= bounds.getWidth() / 2;
                }
                if (VerticalAlignment.TOP == string.verticallAlignment) {
                    realY -= bounds.getY();
                } else if (VerticalAlignment.MIDDLE == string.verticallAlignment) {
                    realY -= bounds.getY() / 2;
                }
                if (string.textStyle == TextStyle.BLANKED) {
                    setColor(graphics, backgroundColor);
                    graphics.fillRect((int) realX,
                            (int) (realY + bounds.getY()),
                            (int) bounds.getWidth(),
                            (int) bounds.getHeight() + 1);
                }
                setColor(graphics, colors[i]);
                graphics.drawString(text[i], (int) realX, (int) realY);
                y += bounds.getHeight();
            }
        }

        protected void drawLines(Graphics graphics, Rectangle2D imageBounds,
                DrawableLine line) {
            setColor(graphics, line.basics.color);
            double[] pt1 = line.points.get(0);
            pt1[0] -= imageBounds.getX();
            pt1[1] -= imageBounds.getY();
            for (int i = 1; i < line.points.size(); i++) {
                double[] pt2 = line.points.get(i);
                pt2[0] -= imageBounds.getX();
                pt2[1] -= imageBounds.getY();
                graphics.drawLine((int) pt1[0], (int) pt1[1], (int) pt2[0],
                        (int) pt2[1]);
                pt1 = pt2;
            }
        }

        protected void drawColorMap(Graphics graphics, Rectangle2D imageBounds,
                DrawableColorMap colorMap) {
            double xStart = colorMap.extent.getMinX() - imageBounds.getX();
            double yStart = colorMap.extent.getMinY() - imageBounds.getY();

            setColor(graphics, backgroundColor);
            graphics.fillRect((int) xStart, (int) yStart,
                    (int) colorMap.extent.getWidth(),
                    (int) colorMap.extent.getHeight());
            double x1 = xStart;
            double xd = colorMap.extent.getWidth()
                    / colorMap.getColorMapParams().getColorMap().getSize();
            for (com.raytheon.uf.common.colormap.Color color : colorMap
                    .getColorMapParams().getColorMap().getColors()) {
                graphics.setColor(new Color(color.getRed(), color.getGreen(),
                        color.getBlue(), color.getAlpha()));
                graphics.fillRect((int) x1, (int) yStart, (int) Math.ceil(xd),
                        (int) colorMap.extent.getHeight());
                x1 += xd;
            }
        }

        protected void drawImage(Graphics graphics, Rectangle2D imageBounds,
                DrawableImage image) {
            KmlRasterImage kmlImage = (KmlRasterImage) image.getImage();
            PixelCoverage coverage = image.getCoverage();
            Coordinate ul = coverage.getUl();
            Coordinate lr = coverage.getLr();
            try {
                Image toDraw = (Image) kmlImage.getImage();
                int x = (int) (ul.x - imageBounds.getX());
                int y = (int) (ul.y - imageBounds.getY());
                int w = (int) (lr.x - ul.x);
                int h = (int) (lr.y - ul.y);
                graphics.drawImage(toDraw, x, y, w, h, null);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        protected void setColor(Graphics graphics, RGB color) {
            graphics.setColor(new Color(color.red, color.green, color.blue));
        }

        protected Rectangle2D getBounds(Object object) {
            if (object instanceof DrawableString) {
                return getStringBounds((DrawableString) object);
            } else if (object instanceof DrawableLine) {
                return getLineBounds((DrawableLine) object);
            } else if (object instanceof DrawableColorMap) {
                return getColorMapBounds((DrawableColorMap) object);
            } else if (object instanceof DrawableImage) {
                return getImageBounds((DrawableImage) object);
            }
            return null;
        }

        protected Rectangle2D getLineBounds(DrawableLine line) {
            Rectangle2D bounds = null;
            for (double[] point : line.points) {
                if (bounds == null) {
                    bounds = new Rectangle2D.Double(point[0], point[1], 0, 0);
                } else {
                    bounds.add(point[0], point[1]);
                }
            }
            /*
             * add a buffer region to ensure we have room for wide lines on the
             * edge.
             */
            bounds.add(bounds.getMinX() - line.width, bounds.getMinY()
                    - line.width);
            bounds.add(bounds.getMaxX() + line.width, bounds.getMaxY()
                    + line.width);
            return bounds;
        }

        protected Rectangle2D getStringBounds(DrawableString string) {
            KmlFont kmlFont = (KmlFont) string.font;
            if (kmlFont == null) {
                kmlFont = new KmlFont();
            }
            FontRenderContext frc = new FontRenderContext(null, false, false);
            Rectangle2D bounds = null;
            String[] text = string.getText();
            double x = string.basics.x;
            double y = string.basics.y;
            for (int i = 0; i < text.length; i += 1) {
                Rectangle2D b = kmlFont.getFont().getStringBounds(text[i], frc);
                b.setFrame(x, y, b.getWidth(), b.getHeight());
                y += b.getHeight();
                if (bounds == null) {
                    bounds = b;
                } else {
                    bounds.add(b);
                }
            }
            if (HorizontalAlignment.RIGHT == string.horizontalAlignment) {
                bounds.setFrame(bounds.getMinX() - bounds.getWidth(),
                        bounds.getMinY(), bounds.getWidth(), bounds.getHeight());
            } else if (HorizontalAlignment.CENTER == string.horizontalAlignment) {
                bounds.setFrame(bounds.getMinX() - bounds.getWidth() / 2,
                        bounds.getMinY(), bounds.getWidth(), bounds.getHeight());
            }
            if (VerticalAlignment.BOTTOM == string.verticallAlignment) {
                bounds.setFrame(bounds.getMinX(),
                        bounds.getMinY() - bounds.getHeight(),
                        bounds.getWidth(), bounds.getHeight());
            } else if (VerticalAlignment.MIDDLE == string.verticallAlignment) {
                bounds.setFrame(bounds.getMinX(),
                        bounds.getMinY() - bounds.getHeight() / 2,
                        bounds.getWidth(), bounds.getHeight());
            }
            return bounds;
        }

        protected Rectangle2D getColorMapBounds(DrawableColorMap colorMap) {
            return new Rectangle2D.Double(colorMap.extent.getMinX(),
                    colorMap.extent.getMinY(), colorMap.extent.getWidth(),
                    colorMap.extent.getHeight());
        }

        protected Rectangle2D getImageBounds(DrawableImage image) {
            PixelCoverage coverage = image.getCoverage();
            Coordinate ul = coverage.getUl();
            Coordinate lr = coverage.getLr();
            return new Rectangle2D.Double(ul.x, ul.y, lr.x - ul.x, lr.y - ul.y);
        }
    }

}
