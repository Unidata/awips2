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
package com.raytheon.uf.viz.core.drawables.ext.colormap;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * For targets which cannot optimize support of {@link IColormapShadedShape}
 * this will provide an inefficient default that simply generates a shaded shape
 * whenever draw is called with new colors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 23, 2014  2363     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GeneralColormapShadedShapeExtension extends
        GraphicsExtension<IGraphicsTarget> implements
        IColormapShadedShapeExtension {

    @Override
    public GeneralColormapShadedShape createColormapShadedShape(
            GeneralGridGeometry targetGeometry, boolean tesselate) {
        return new GeneralColormapShadedShape(targetGeometry, tesselate);
    }

    @Override
    public IShadedShape createShadedShape(IColormapShadedShape baseShape,
            Map<Object, RGB> colors) {
        GeneralColormapShadedShape generalShape = (GeneralColormapShadedShape) baseShape;
        return generalShape.generateShape(target, colors);
    }

    @Override
    public void drawColormapShadedShape(IColormapShadedShape shape,
            Map<Object, RGB> colors, float alpha, float brightness)
            throws VizException {
        if (shape.isDrawable()) {
            GeneralColormapShadedShape generalShape = (GeneralColormapShadedShape) shape;
            IShadedShape shadedShape = generalShape.getShape(target,
                    colors);
            target.drawShadedShape(shadedShape, alpha, brightness);
        }
    }

    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        return Compatibilty.GENERIC;
    }

    /**
     * Contains all the interesting logic for this extension. Basic
     * functionality is to save off the LineStrings for each add operation so
     * that a real {@link IShadedShape} can be generated when the colors are
     * provided. Also keeps around a shape after rendering for potential reuse
     * if the colors don't change.
     */
    private static class GeneralColormapShadedShape implements
            IColormapShadedShape {

        private final GeneralGridGeometry targetGeometry;

        private final boolean tesselate;

        private List<AddPair> addPairs = new ArrayList<AddPair>();

        private Map<Object, RGB> lastColors;

        private IShadedShape lastShape;

        private GeneralColormapShadedShape(GeneralGridGeometry targetGeometry,
                boolean tesselate) {
            this.targetGeometry = targetGeometry;
            this.tesselate = tesselate;
        }

        /**
         * Get a shaded shape that can be used to render this. This method also
         * implements the caching if colors doesn't change.
         * 
         * @param target
         *            the target be rendered onto.
         * @param colors
         *            the colors to use for the shape
         * @return the shape to render.
         */
        public IShadedShape getShape(IGraphicsTarget target,
                Map<Object, RGB> colors) {
            if (!colors.equals(lastColors)) {
                if (lastShape != null) {
                    lastShape.dispose();
                }
                lastShape = generateShape(target, colors);
                lastColors = new HashMap<Object, RGB>(colors);
            }

            return lastShape;
        }

        /**
         * Generate a new {@link IShadedShape} that renders identically to this.
         * 
         * @param target
         *            the target be rendered onto.
         * @param colors
         *            the colors to use for the shape
         * @return a new shape that will render the same as this shape with
         *         colors applied.
         */
        public IShadedShape generateShape(IGraphicsTarget target,
                Map<Object, RGB> colors) {
            IShadedShape shape = target.createShadedShape(true, targetGeometry,
                    tesselate);
            for (AddPair pair : addPairs) {
                if (pair.pixelSpace) {
                    shape.addPolygonPixelSpace(pair.lineString,
                            colors.get(pair.colorKey));
                } else {
                    shape.addPolygon(pair.lineString, colors.get(pair.colorKey));
                }
            }
            return shape;
        }

        @Override
        public void compile() {
            if (lastShape != null) {
                lastShape.compile();
            }
        }

        @Override
        public boolean isMutable() {
            if (lastShape != null) {
                return lastShape.isMutable();
            }
            return true;
        }

        @Override
        public boolean isDrawable() {
            return !addPairs.isEmpty();
        }

        @Override
        public void dispose() {
            if (lastShape != null) {
                lastShape.dispose();
                lastShape = null;
            }
            addPairs = new ArrayList<AddPair>();
        }

        @Override
        public void reset() {
            lastShape.reset();
            lastColors.clear();
            addPairs = new ArrayList<AddPair>();
        }

        @Override
        public Collection<Object> getColorKeys() {
            Set<Object> keys = new HashSet<Object>(addPairs.size(), 1.0f);

            return keys;
        }

        @Override
        public void addPolygon(LineString[] lineString, Object colorKey) {
            addPairs.add(new AddPair(lineString, colorKey, false));
            if (lastShape != null && lastColors != null) {
                lastShape.addPolygon(lineString, lastColors.get(colorKey));
            }
        }

        @Override
        public void addPolygonPixelSpace(LineString[] contours, Object colorKey) {
            addPairs.add(new AddPair(contours, colorKey, true));
            if (lastShape != null && lastColors != null) {
                lastShape.addPolygonPixelSpace(contours,
                        lastColors.get(colorKey));
            }
        }

        @Override
        public void addGeometry(Geometry geometry, Object colorKey) {
            if (geometry instanceof GeometryCollection) {
                GeometryCollection geomCollection = (GeometryCollection) geometry;
                for (int i = 0; i < geomCollection.getNumGeometries(); i++) {
                    addGeometry(geomCollection.getGeometryN(i), colorKey);
                }
            } else if (geometry instanceof LineString) {
                LineString[] lineStrings = { (LineString) geometry };
                addPolygon(lineStrings, colorKey);
            } else if (geometry instanceof Polygon) {
                LineString[] lineStrings = { ((Polygon) geometry)
                        .getExteriorRing() };
                addPolygon(lineStrings, colorKey);
            }
        }

    }

    /**
     * Simple Object for storing the parameters to any of the add methods on
     * {@link IColormapShadedShape}.
     */
    private static class AddPair {
        public final LineString[] lineString;

        public final Object colorKey;

        public final boolean pixelSpace;

        public AddPair(LineString[] lineString, Object colorKey,
                boolean pixelSpace) {
            this.lineString = lineString;
            this.colorKey = colorKey;
            this.pixelSpace = pixelSpace;
        }

    }

}
