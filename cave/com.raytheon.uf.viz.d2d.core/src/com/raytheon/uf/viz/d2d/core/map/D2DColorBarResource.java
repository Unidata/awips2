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
package com.raytheon.uf.viz.d2d.core.map;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters.LabelEntry;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.preferences.ColorFactory;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DColorBarResource extends
        AbstractVizResource<GenericResourceData, IDescriptor> {

    private static RGB COLOR = ColorFactory.getInstance().getColor(
            D2DColorBarResource.class.getName() + "Color");

    private static enum ColorBarLoc {
        LEFT, RIGHT
    }

    private IFont colorBarFont;

    private Map<List<LabelEntry>, List<LabelEntry>> modifiedMap;

    public D2DColorBarResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        modifiedMap = new HashMap<List<LabelEntry>, List<LabelEntry>>();
    }

    @Override
    public String getName() {
        return "D2D Color Bar Resource";
    }

    @Override
    protected void disposeInternal() {
        colorBarFont.dispose();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        colorBarFont = target.initializeFont(D2DColorBarResource.class
                .getName() + "Font");
        colorBarFont.setScaleFont(false);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        target.clearClippingPlane();
        ResourceList rl = descriptor.getResourceList();
        for (ResourcePair rp : rl) {
            paintResource(target, paintProps, rp, ColorBarLoc.LEFT);
        }
        target.setupClippingPlane(paintProps.getClippingPane());
    }

    private void paintResource(IGraphicsTarget target,
            PaintProperties paintProps, ResourcePair rp, ColorBarLoc loc)
            throws VizException {
        ResourceProperties props = rp.getProperties();
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (props.isSystemResource() == false && props.isVisible()
                && rsc != null) {
            if (rsc.hasCapability(ColorMapCapability.class)) {
                paintColorBar(target, paintProps, rsc,
                        rsc.getCapability(ColorMapCapability.class)
                                .getColorMapParameters(), loc);
            }

            if (rsc.hasCapability(BlendableCapability.class)) {
                ResourceList rl = rsc.getCapability(BlendableCapability.class)
                        .getResourceList();
                // Assume 2 resources
                if (rl.size() > 0) {
                    paintResource(target, paintProps, rl.get(0),
                            ColorBarLoc.LEFT);
                }
                if (rl.size() > 1) {
                    paintResource(target, paintProps, rl.get(1),
                            ColorBarLoc.RIGHT);
                }
            }
        }
    }

    private void paintColorBar(IGraphicsTarget target,
            PaintProperties paintProps, AbstractVizResource<?, ?> rsc,
            ColorMapParameters colorMapParameters, ColorBarLoc loc)
            throws VizException {
        if (colorMapParameters == null
                || colorMapParameters.getColorMap() == null) {
            return;
        }
        switch (loc) {
        case LEFT: {
            paintLeftColorBar(target, paintProps, rsc, colorMapParameters);
            break;
        }
        case RIGHT: {
            paintRightColorBar(target, paintProps, rsc, colorMapParameters);
            break;
        }
        }
    }

    private void paintLeftColorBar(IGraphicsTarget target,
            PaintProperties paintProps, AbstractVizResource<?, ?> rsc,
            ColorMapParameters colorMapParameters) throws VizException {
        ImagingCapability cap = rsc.hasCapability(ImagingCapability.class) ? rsc
                .getCapability(ImagingCapability.class) : null;
        float alpha = cap != null ? cap.getAlpha() : paintProps.getAlpha();
        float brightness = cap != null ? cap.getBrightness() : 1.0f;
        float contrast = cap != null ? cap.getContrast() : 1.0f;
        double x1 = 3;
        double drawnWidth = (paintProps.getCanvasBounds().width / 2.0) - 7.0;
        double x2 = x1 + drawnWidth;
        double y1 = 0;
        int pixels = paintProps.getCanvasBounds().width < 500 ? 18 : 25;
        double y2 = y1 + (pixels);

        DrawableColorMap cmap = new DrawableColorMap(colorMapParameters);
        cmap.extent = new PixelExtent(x1, x2, y1, y2);
        cmap.alpha = alpha;
        cmap.brightness = brightness;
        cmap.contrast = contrast;
        target.getExtension(ICanvasRenderingExtension.class).drawColorRamp(
                paintProps, cmap);

        if (rsc.hasCapability(BlendedCapability.class)) {
            alpha *= 2.0;
        }
        double yPos = y1 + ((y2 - y1) * .4);
        double zPos = 0; // draw the colorbar on the plane z = 0

        List<LabelEntry> labels = colorMapParameters.getLabels();
        PaintProperties labelProps = new PaintProperties(paintProps);
        labelProps.setAlpha(alpha);

        paintLabels(target, labelProps, x1, x2, yPos, zPos, labels);
    }

    private void paintRightColorBar(IGraphicsTarget target,
            PaintProperties paintProps, AbstractVizResource<?, ?> rsc,
            ColorMapParameters colorMapParameters) throws VizException {
        ImagingCapability cap = rsc.hasCapability(ImagingCapability.class) ? rsc
                .getCapability(ImagingCapability.class) : null;
        float alpha = cap != null ? cap.getAlpha() : paintProps.getAlpha();
        float brightness = cap != null ? cap.getBrightness() : 1.0f;
        float contrast = cap != null ? cap.getContrast() : 1.0f;
        double x1 = 0;
        double drawnWidth = ((paintProps.getCanvasBounds().width / 2.0) - 7.0);
        double x2 = paintProps.getCanvasBounds().width - 3;
        x1 = x2 - drawnWidth;
        double y1 = 0;
        int pixels = paintProps.getCanvasBounds().width < 500 ? 18 : 25;
        double y2 = y1 + pixels;

        DrawableColorMap cmap = new DrawableColorMap(colorMapParameters);
        cmap.extent = new PixelExtent(x1, x2, y1, y2);
        cmap.alpha = alpha;
        cmap.brightness = brightness;
        cmap.contrast = contrast;
        target.getExtension(ICanvasRenderingExtension.class).drawColorRamp(
                paintProps, cmap);

        alpha *= 2.0;
        double yPos = y1 + ((y2 - y1) * .4);
        double zPos = 0; // draw the colorbar on the plane z = 0

        List<LabelEntry> labels = colorMapParameters.getLabels();
        PaintProperties labelProps = new PaintProperties(paintProps);
        labelProps.setAlpha(alpha);
        paintLabels(target, labelProps, x1, x2, yPos, zPos, labels);
    }

    /**
     * @param numericIndecies
     * @return
     */
    private boolean inARow(List<Integer> numericIndecies) {
        int prev = numericIndecies.get(0);
        boolean valid = true;
        for (int i = 1; i < numericIndecies.size() && valid; ++i) {
            int next = numericIndecies.get(i);
            if (next - prev != 1) {
                valid = false;
            }
            prev = next;
        }
        return valid;
    }

    public int check(Double value) {
        return value > 0.0 ? 1 : -1;
    }

    @Override
    public ResourceOrder getResourceOrder() {
        return RenderingOrderFactory.ResourceOrder.HIGHEST;
    }

    private void paintLabels(IGraphicsTarget target,
            PaintProperties paintProps, double x1, double x2, double yPos,
            double zPos, List<LabelEntry> labels) throws VizException {
        if (labels != null) {
            List<LabelEntry> modifiedLabels = modifiedMap.get(labels);
            if (modifiedLabels == null) {
                modifiedLabels = new ArrayList<LabelEntry>();
                modifiedMap.put(labels, modifiedLabels);
                List<Integer> numericIndecies = new ArrayList<Integer>();
                List<Double> values = new ArrayList<Double>();
                for (int i = 0; i < labels.size(); ++i) {
                    try {
                        values.add(Double.parseDouble(labels.get(i).getText()));
                        numericIndecies.add(i);
                    } catch (Throwable t) {
                        // ignore
                    }
                }

                if (numericIndecies.size() < 3
                        || values.size() != numericIndecies.size()
                        && inARow(numericIndecies)) {
                    modifiedLabels.addAll(labels);
                } else {
                    int i = 0;
                    for (; i < labels.size(); ++i) {
                        if (numericIndecies.contains(i)) {
                            break;
                        } else {
                            modifiedLabels.add(labels.get(i));
                        }
                    }

                    // Here we start the -0+ trick
                    int zeroIndex = -1;
                    int numZeros = 0;
                    for (int j = 0; j < values.size(); ++j) {
                        if (values.get(j) == 0) {
                            zeroIndex = j;
                            ++numZeros;
                        }
                    }

                    if (zeroIndex != -1 && numZeros == 1) {
                        // is zero the last or first index?
                        if (zeroIndex == 0 || zeroIndex == values.size() - 1) {
                            // yes, are all the values to left or right of zero
                            // same
                            // sign?
                            int inc = zeroIndex == 0 ? 1 : -1;
                            int j = zeroIndex + inc;
                            int prev = check(values.get(j));
                            j += inc;
                            boolean good = true;
                            for (int count = 2; count < values.size() && good; count++, j += inc) {
                                int next = check(values.get(j));
                                if (next != prev) {
                                    good = false;
                                }
                                prev = next;
                            }

                            if (good) {
                                // all the values are the same sign
                                LabelEntry zeroLabel = labels
                                        .get(numericIndecies.get(zeroIndex));
                                String s = zeroLabel.getText();
                                if (zeroIndex == 0) {
                                    zeroLabel = new LabelEntry(s
                                            + (prev > 0 ? "" : "-"),
                                            zeroLabel.getLocation());
                                    modifiedLabels.add(zeroLabel);
                                    ++i;
                                    for (int k = 1; k < numericIndecies.size(); ++k, ++i) {
                                        if (prev > 0) {
                                            modifiedLabels
                                                    .add(labels
                                                            .get(numericIndecies
                                                                    .get(k)));
                                        } else {
                                            LabelEntry le = labels
                                                    .get(numericIndecies.get(k));
                                            String l = le.getText()
                                                    .substring(1);
                                            modifiedLabels.add(new LabelEntry(
                                                    l, le.getLocation()));
                                        }
                                    }
                                } else {
                                    zeroLabel = new LabelEntry((prev > 0 ? "+"
                                            : "") + s, zeroLabel.getLocation());

                                    for (int k = 0; k < numericIndecies.size() - 1; ++k, ++i) {
                                        if (prev > 0) {
                                            modifiedLabels
                                                    .add(labels
                                                            .get(numericIndecies
                                                                    .get(k)));
                                        } else {
                                            if (k == 0) {
                                                LabelEntry le = labels
                                                        .get(numericIndecies
                                                                .get(k));
                                                String l = le.getText();
                                                modifiedLabels
                                                        .add(new LabelEntry(
                                                                l,
                                                                le.getLocation()));
                                            } else {
                                                LabelEntry le = labels
                                                        .get(numericIndecies
                                                                .get(k));
                                                String l = le.getText()
                                                        .substring(1);
                                                modifiedLabels
                                                        .add(new LabelEntry(
                                                                l,
                                                                le.getLocation()));
                                            }
                                        }
                                    }
                                    modifiedLabels.add(zeroLabel);
                                    ++i;
                                }
                            }
                        } else {
                            // zero is in the middle somewhere!!!
                            boolean leftSame = true;
                            int leftPrev = check(values.get(0));
                            for (int j = 1; j < zeroIndex && leftSame; ++j) {
                                int leftNext = check(values.get(j));
                                if (leftNext != leftPrev) {
                                    leftSame = false;
                                }
                                leftPrev = leftNext;
                            }

                            if (leftSame) {
                                // continue to right...
                                boolean rightSame = true;
                                int rightPrev = check(values.get(zeroIndex + 1));
                                for (int j = zeroIndex + 2; j < values.size()
                                        && rightSame; ++j) {
                                    int rightNext = check(values.get(j));
                                    if (rightNext != rightPrev) {
                                        rightSame = false;
                                    }
                                    rightPrev = rightNext;
                                }

                                // All the same on the right side and opposite
                                // of
                                // left
                                if (rightSame && rightPrev != leftPrev) {
                                    // add the left ones
                                    for (int j = 0; j < zeroIndex; ++j, ++i) {
                                        int idx = numericIndecies.get(j);
                                        if (leftPrev > 0 || j == 0) {
                                            // positive on left, add
                                            modifiedLabels.add(labels.get(idx));
                                        } else {
                                            LabelEntry le = labels.get(idx);
                                            String l = le.getText()
                                                    .substring(1);
                                            modifiedLabels.add(new LabelEntry(
                                                    l, le.getLocation()));
                                        }
                                    }
                                    LabelEntry zeroLabel = labels
                                            .get(numericIndecies.get(zeroIndex));
                                    modifiedLabels.add(new LabelEntry(
                                            (leftPrev > 0 ? "+" : "-")
                                                    + zeroLabel.getText()
                                                    + (rightPrev > 0 ? "+"
                                                            : "-"), zeroLabel
                                                    .getLocation()));
                                    ++i;
                                    for (int j = zeroIndex + 1; j < values
                                            .size(); ++j, ++i) {
                                        int idx = numericIndecies.get(j);
                                        if (rightPrev > 0 || idx == 0) {
                                            // positive on left, add
                                            modifiedLabels.add(labels.get(idx));
                                        } else {
                                            LabelEntry le = labels.get(idx);
                                            String l = le.getText()
                                                    .substring(1);
                                            modifiedLabels.add(new LabelEntry(
                                                    l, le.getLocation()));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    for (; i < labels.size(); ++i) {
                        modifiedLabels.add(labels.get(i));
                    }
                }
            }

            double lastXPos = Double.NEGATIVE_INFINITY;
            double padding = 3;
            if (paintProps.getCanvasBounds().width < 500) {
                colorBarFont.setMagnification(0.9f);
            }

            List<DrawableString> drawables = new ArrayList<DrawableString>(
                    modifiedLabels.size());

            for (LabelEntry label : modifiedLabels) {
                double xPos = x1 + ((x2 - x1) * label.getLocation());

                String s = label.getText();

                if (s.startsWith("0.")) {
                    try {
                        Double.parseDouble(s);
                        s = s.substring(1);
                    } catch (Exception e) {

                    }
                } else if (s.startsWith("-0.")) {
                    try {
                        Double.parseDouble(s);
                        s = "-" + s.substring(2);
                    } catch (Exception e) {

                    }
                }

                if ("".equals(s.trim())) {
                    continue;
                }

                DrawableString drawable = new DrawableString(s, COLOR);
                drawable.font = colorBarFont;
                drawable.setCoordinates(xPos, yPos, zPos);
                drawable.horizontalAlignment = IGraphicsTarget.HorizontalAlignment.CENTER;
                drawable.verticallAlignment = IGraphicsTarget.VerticalAlignment.MIDDLE;
                drawable.textStyle = TextStyle.BLANKED;
                drawable.basics.alpha = paintProps.getAlpha();

                Rectangle2D rect = target.getStringsBounds(drawable);
                double widthDiv2 = (rect.getWidth() / 2);
                if (xPos - widthDiv2 > lastXPos) {
                    drawables.add(drawable);
                    lastXPos = xPos + widthDiv2 + padding;
                }
            }
            target.getExtension(ICanvasRenderingExtension.class).drawStrings(
                    paintProps, drawables.toArray(new DrawableString[0]));
        }
    }
}
