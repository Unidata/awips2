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
package com.raytheon.uf.viz.core.drawables.ext;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Interface extensions that return IImage objects should implement
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IImagingExtension extends IGraphicsExtensionInterface {

    /**
     * Draw the images passed in
     * 
     * @param paintProps
     * @param images
     * @return whether all images were drawn or not
     * @throws VizException
     */
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException;

    public static class ImagingSupport {

        protected static final TextureLoader textureLoader = TextureLoader
                .getInstance();

        /**
         * Prepares images for painting by staging and/or targeting the image
         * 
         * @param target
         * @param images
         * @throws VizException
         */
        public static void prepareImages(IGraphicsTarget target,
                DrawableImage... images) throws VizException {
            for (DrawableImage di : images) {
                IImage image = di.getImage();
                RasterMode mode = di.getMode();

                if (image.getStatus() != Status.LOADED) {
                    if (image.getStatus() != Status.STAGED) {
                        if (mode == RasterMode.ASYNCHRONOUS) {
                            textureLoader.requestLoad(image);
                            target.setNeedsRefresh(true);
                        } else if (mode == RasterMode.SYNCHRONOUS) {
                            image.stage();
                        }
                    }

                    if (image.getStatus() == Status.STAGED) {
                        image.target(target);
                    }
                }
            }
        }

        /**
         * Routes the images to be rendered by their proper extensions
         * 
         * @param target
         * @param paintProps
         * @param images
         * @return
         * @throws VizException
         */
        public static boolean routeImages(IGraphicsTarget target,
                PaintProperties paintProps, DrawableImage[] images)
                throws VizException {
            boolean rval = true;
            boolean skipped = false;
            List<DrawableImage> bulk = new ArrayList<DrawableImage>();
            Class<? extends IImagingExtension> lastExt = null;
            for (DrawableImage di : images) {
                IImage image = di.getImage();
                if (image.getStatus() == IImage.Status.LOADED) {
                    Class<? extends IImagingExtension> imageExt = image
                            .getExtensionClass();
                    if (imageExt.equals(lastExt) == false && bulk.size() > 0) {
                        DrawableImage[] extImages = bulk
                                .toArray(new DrawableImage[bulk.size()]);
                        // Render what we have
                        IImagingExtension impl = target.getExtension(lastExt);
                        rval &= impl.drawRasters(paintProps, extImages);
                        bulk.clear();
                    }

                    bulk.add(di);
                    lastExt = imageExt;
                } else {
                    skipped = true;
                }
            }

            if (bulk.size() > 0) {
                // Render what is left
                IImagingExtension impl = target.getExtension(lastExt);
                rval &= impl.drawRasters(paintProps,
                        bulk.toArray(new DrawableImage[bulk.size()]));
            }

            return rval & skipped;
        }

        /**
         * Prepares the images, then routes them for rendering
         * 
         * @param paintProps
         * @param images
         * @return
         */
        public static boolean drawRasters(IGraphicsTarget target,
                PaintProperties paintProps, DrawableImage[] images)
                throws VizException {
            prepareImages(target, images);
            return routeImages(target, paintProps, images);
        }
    }
}
