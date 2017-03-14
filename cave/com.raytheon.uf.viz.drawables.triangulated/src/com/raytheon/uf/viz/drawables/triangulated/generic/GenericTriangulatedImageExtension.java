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
package com.raytheon.uf.viz.drawables.triangulated.generic;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangleLocationCallback;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImage;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImageExtension;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Generic implementation of {@link ITriangulatedImageExtension} that uses a
 * {@link TriangleFlattener} to convert the triangles into a grid for use with
 * an {@link IColormappedImage}. See {@link TriangleFlattener} for more details
 * on the limitations of this implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------
 * Aug 18, 2015  4709     bsteffen  Initial creation
 * Dec 04, 2015  5146     bsteffen  Limit the size of the image
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GenericTriangulatedImageExtension extends
        GraphicsExtension<IGraphicsTarget> implements
        ITriangulatedImageExtension {

    @Override
    public ITriangulatedImage initializeImage(
            ColorMapParameters colorMapParameters,
            ITriangleLocationCallback locationCallback,
            IColorMapDataRetrievalCallback dataCallback) throws VizException {
        IColormappedImageExtension imageExtension = target
                .getExtension(IColormappedImageExtension.class);
        TriangleFlattener flattener = new TriangleFlattener(dataCallback,
                locationCallback, getViewArea());
        IColormappedImage image = imageExtension.initializeRaster(flattener,
                colorMapParameters);
        return new GenericTriangulatedImage(image, flattener);
    }

    private Envelope getViewArea() {
        /*
         * Use the extent area multiplied by the zoom level to determine the
         * full view size when zoomed out.
         */
        IView view = target.getView();
        IExtent extent = view.getExtent();
        double zoom = view.getZoom();
        double width = extent.getWidth() / zoom;
        double height = extent.getHeight() / zoom;
        return new Envelope(0, width, 0, height);
    }

    @Override
    public void drawImage(PaintProperties paintProps, ITriangulatedImage image)
            throws VizException {
        if (image instanceof GenericTriangulatedImage) {
            target.drawRasters(paintProps,
                    ((GenericTriangulatedImage) image).getImage());
            return;
        }
        throw new IllegalStateException(this.getClass().getSimpleName()
                + " can only draw "
                + GenericTriangulatedImage.class.getSimpleName() + "s");
    }

    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        // Since there is a GeneralColormappedImageExtension I don't see how
        // anyone could not have one but you never know
        try {
            IColormappedImageExtension imageExtension = target
                    .getExtension(IColormappedImageExtension.class);
            if (imageExtension != null) {
                return Compatibilty.GENERIC;
            }
        } catch (VizException e) {
            return Compatibilty.INCOMPATIBLE;
        }
        return Compatibilty.INCOMPATIBLE;
    }

}
