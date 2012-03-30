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

import java.util.Collection;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * This extension allows you to create a shaded shape with no color information.
 * Instead of specifying a color each polygon has a colorKey. This colormap
 * shape can then be combined with a map of colors to create a shaded shape.
 * This is useful when you have many shaded shapes containing the same polygons
 * but in different colors. If the shapes are created using this extension then
 * they can share the polygon information which significantly speeds up creation
 * and uses less memory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public interface IColormapShadedShapeExtension extends
        IGraphicsExtensionInterface {

    public interface IColormapShadedShape {

        /**
         * get the colormap keys
         * 
         * @return
         */
        public Collection<Object> getColorKeys();

        /**
         * Add a sequence of latitude and longitudes which form a (closed)
         * polygon
         * 
         * @param lineString
         *            the sequence of shape rings
         * @param colorKey
         *            the key object used to color the polygon
         */
        public void addPolygon(LineString[] lineString, Object colorKey);

        /**
         * Add a sequence of points in the pixel space
         * 
         * @param contours
         *            the sequence of shape rings
         * @param colorKey
         *            the key object used to color the polygon
         */
        public void addPolygonPixelSpace(LineString[] contours, Object colorKey);

        /**
         * Add a geometry in Lat/Lon space
         * 
         * @param geometry
         *            the geometry to add
         * @param colorKey
         *            the key object used to color the polygon
         */
        public void addGeometry(Geometry geometry, Object colorKey);

        public void dispose();

    }

    /**
     * Create an colormap shaded shape object which can be combined with color
     * data to make a shaded shape
     * 
     * @param mutable
     *            whether the shape changes after creation
     * @param descriptor
     *            the map descriptor
     * @param tesselate
     *            whether a shape requires tesselation to be convex
     * @return a shaded shape object
     */
    public IColormapShadedShape createColormapShadedShape(
            IDescriptor descriptor, boolean tesselate);

    /**
     * Create a shaded shape object
     * 
     * @param baseShape
     *            colormap shape that serves as base data for a new Shaded
     *            shape.
     * @param colors
     *            the color to use for each colorKey
     * @return a shaded shape object
     */
    public IShadedShape createShadedShape(IColormapShadedShape baseShape,
            Map<Object, RGB> colors);

    /**
     * Colormap and draw a
     * 
     * @param shape
     *            the shaded shape object
     * @param colors
     *            the colors to use
     * @param alpha
     *            the alpha blending coefficient
     * @param brightness
     *            the brightness blending coefficient
     * @throws VizException
     */
    public void drawColormapShadedShape(IColormapShadedShape shape,
            Map<Object, RGB> colors, float alpha, float brightness)
            throws VizException;

}
