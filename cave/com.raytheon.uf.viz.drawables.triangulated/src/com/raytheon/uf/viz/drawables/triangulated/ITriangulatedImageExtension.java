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
package com.raytheon.uf.viz.drawables.triangulated;


import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * This extension supports rendering multiple triangles using a colormap. The
 * vertices of each triangle are associated with a datavalue and the values are
 * evenly interpolated across the face of the triangle. These interpolated
 * values are then colormapped the same way as {@link IColormappedImage}s.
 * Rendering multiple triangles over a surface, with shared edges and vertices
 * will generate a continuous, smoothly interpolated area over the entire
 * surface using as many triangles as necessary to include all data points.
 * 
 * This extension can be used for generating an image from a set of scattered
 * points with data values. The points must first be triangulated, which can be
 * done with well documented techniques such as Delauney Triangulation, and the
 * resulting triangles can be used to render an image of the points.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 18, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public interface ITriangulatedImageExtension extends
        IGraphicsExtensionInterface {

    /**
     * Create a new {@link ITriangulatedImage}. Callbacks are provided instead
     * of raw data to allow the graphics layer more control over managing its
     * memory by having the ability to unload and reload image data when
     * necessary.
     * 
     * @param colorMapParameters
     *            the information needed to apply colors to the data values
     * @param locations
     *            provides information about the locations of triangles
     * @param dataCallback
     *            provides data values for each vertex in the triangle. The
     *            resulting {@link ColorMapData} should be one dimensional.
     * 
     * @return a new triangulated image.
     * @throws VizException
     *             If the extension is unable to create a new image.
     */
    public ITriangulatedImage initializeImage(
            ColorMapParameters colorMapParameters,
            ITriangleLocationCallback locations,
            IColorMapDataRetrievalCallback dataCallback) throws VizException;

    /**
     * Draw an {@link ITriangulatedImage} to the target.
     * 
     * @param paintProps
     *            the properties to use when drawing
     * @param image
     *            the image to draw
     * @throws VizException
     *             if the image cannot be drawn.
     */
    public void drawImage(PaintProperties paintProps, ITriangulatedImage image)
            throws VizException;

}
