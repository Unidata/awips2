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
package com.raytheon.uf.viz.kml.export.graphics;

import java.awt.FontFormatException;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractGraphicsTarget;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.graphics.basicgen.KmlCirclesGenerator;
import com.raytheon.uf.viz.kml.export.graphics.basicgen.KmlLinesGenerator;
import com.raytheon.uf.viz.kml.export.graphics.basicgen.KmlPointsGenerator;
import com.raytheon.uf.viz.kml.export.graphics.basicgen.KmlRectGenerator;
import com.raytheon.uf.viz.kml.export.graphics.basicgen.KmlStringsGenerator;
import com.raytheon.uf.viz.kml.export.graphics.ext.KmlRasterImage;

/**
 * 
 * Takes graphics operations and produces a list of KmlFeatureGenerators that
 * can be used to make KML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlGraphicsTarget extends AbstractGraphicsTarget {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlGraphicsTarget.class);

    private final KmlFont defaultFont = new KmlFont();

    private List<KmlFeatureGenerator> generators = new ArrayList<KmlFeatureGenerator>(
            256);

    protected IView view;

    public KmlGraphicsTarget() {
        super();
    }

    public void setView(IView view) {
        this.view = view;
    }

    @Override
    public KmlFont initializeFont(String fontId) {
        FontRegistry registry = PlatformUI.getWorkbench().getThemeManager()
                .getCurrentTheme().getFontRegistry();
        if (registry.hasValueFor(fontId)) {
            FontData[] data = registry.getFontData(fontId);
            FontData fd = data[0];
            if (fd == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "No font data found for id: " + fontId);
            }
            float size = fd.height;
            String name = fd.getName();
            List<IFont.Style> styles = new ArrayList<IFont.Style>();

            int style = fd.getStyle();
            if ((style & SWT.BOLD) != 0) {
                styles.add(IFont.Style.BOLD);
            }
            if ((style & SWT.ITALIC) != 0) {
                styles.add(IFont.Style.ITALIC);
            }

            return new KmlFont(name, size,
                    styles.toArray(new IFont.Style[styles.size()]));
        } else {
            return getDefaultFont();
        }
    }

    @Override
    public KmlFont initializeFont(String fontName, float size, Style[] styles) {
        return new KmlFont(fontName, size, styles);
    }

    @Override
    public KmlFont initializeFont(File fontFile, float size, Style[] styles) {
        try {
            return new KmlFont(fontFile, size, styles);
        } catch (FontFormatException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return new KmlFont((String) null, size, styles);
    }

    @Override
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, IFont font, float alpha)
            throws VizException {
        addGenerator(new KmlWireframeShape.Generator((KmlWireframeShape) shape,
                alpha, color, lineWidth));
    }

    @Override
    public KmlWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel) {
        return new KmlWireframeShape(geom);
    }

    @Override
    public KmlWireframeShape createWireframeShape(boolean mutableFlag,
            GeneralGridGeometry geom) {
        return new KmlWireframeShape(geom);
    }

    @Override
    public KmlWireframeShape createWireframeShape(boolean mutable,
            GeneralGridGeometry geom, float simplificationLevel,
            boolean spatialChopFlag, IExtent extent) {
        return new KmlWireframeShape(geom);
    }

    @Override
    public KmlFont getDefaultFont() {
        return defaultFont;
    }

    public RGB getBackgroundColor() {
        return backgroundColor;
    }

    @Override
    public Rectangle2D getStringsBounds(DrawableString parameters, String string) {
        KmlFont kmlFont = (KmlFont) parameters.font;
        if (kmlFont == null) {
            kmlFont = getDefaultFont();
        }
        FontRenderContext frc = new FontRenderContext(null, false, false);
        Rectangle2D rect = kmlFont.getFont().getStringBounds(string, frc);
        double width = rect.getWidth() * kmlFont.getMagnification();
        double height = rect.getHeight() * kmlFont.getMagnification();
        return new Rectangle2D.Double(0, 0, width, height);
    }

    @Override
    public void drawStrings(Collection<DrawableString> parameters)
            throws VizException {
        addGenerator(new KmlStringsGenerator(parameters));
    }

    @Override
    public void drawPoints(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) throws VizException {
        addGenerator(new KmlPointsGenerator(locations, color, pointStyle,
                magnification));
    }

    @Override
    public void drawLine(DrawableLine... lines) throws VizException {
        addGenerator(new KmlLinesGenerator(lines));
    }

    @Override
    public void drawCircle(DrawableCircle... circles) throws VizException {
        addGenerator(new KmlCirclesGenerator(circles));
    }

    @Override
    public IImage initializeRaster(IRenderedImageCallback imageCallback) {
        return new KmlRasterImage(imageCallback);
    }

    @Override
    public IShadedShape createShadedShape(boolean mutable,
            GeneralGridGeometry targetGeometry, boolean tesselate) {
        return new KmlShadedShape(targetGeometry);
    }

    @Override
    public void drawShadedShapes(float alpha, float brightness,
            IShadedShape... shapes) throws VizException {
        for (IShadedShape shape : shapes) {
            addGenerator(new KmlShadedShape.Generator(alpha,
                    (KmlShadedShape) shape));
        }
    }

    @Override
    public void drawRect(IExtent pe, RGB color, float lineWidth, double alpha)
            throws VizException {
        addGenerator(new KmlRectGenerator(pe, color, alpha, lineWidth, false));
    }

    @Override
    public void drawShadedRect(IExtent pe, RGB color, double alpha,
            byte[] pattern) throws VizException {
        addGenerator(new KmlRectGenerator(pe, color, alpha, 1.0f, true));

    }

    @Override
    public void init() {
        // this function intentionally left blank
    }

    @Override
    public void beginFrame(IView view, boolean isClearBackground) {
        setNeedsRefresh(false);
        generators.clear();
    }

    @Override
    public void endFrame() {
        // this function intentionally left blank
    }

    @Override
    public void resize() {
        // no need
    }

    @Override
    public void dispose() {
        // nothing
    }

    @Override
    public BufferedImage screenshot() {
        // No one should be doing this.
        return null;
    }

    @Override
    public void setupClippingPlane(IExtent extent) {
        // for now always ignore cliiping panes
    }

    @Override
    public void clearClippingPlane() {
        // for now always ignore cliiping panes
    }

    @Override
    public void drawColorRamp(DrawableColorMap colorMap) throws VizException {
        // currently this is handled outside the target, it might move here some
        // day but screen overlay rendering through the target is a bit
        // difficult for labels and things.
    }

    /**
     * Used by graphics extensions to add generators to the current frame.
     * 
     * @param generator
     */
    public void addGenerator(KmlFeatureGenerator generator) {
        generators.add(generator);
    }

    /**
     * Get the generators used during the last frame paint.
     * 
     * @return
     */
    public List<KmlFeatureGenerator> getGenerators() {
        return generators;
    }

    @Override
    public void drawWireframeShape(IWireframeShape shape, RGB color,
            float lineWidth, LineStyle lineStyle, float alpha)
            throws VizException {
        drawWireframeShape(shape, color, lineWidth, lineStyle,
                getDefaultFont(), alpha);
    }

    @Override
    public IView getView() {
        return view;
    }

}
