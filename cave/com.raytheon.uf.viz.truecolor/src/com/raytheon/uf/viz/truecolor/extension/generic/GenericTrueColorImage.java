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
package com.raytheon.uf.viz.truecolor.extension.generic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.ITrueColorImage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Generic {@link ITrueColorImage} which stores all the information and uses it
 * to generate {@link RenderedImage}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 06, 2016  5400     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GenericTrueColorImage implements ITrueColorImage {

    /**
     * To avoid performance problems the generated image is generated at a lower
     * resolution specified by this factor.
     */
    private static final int DOWNSAMPLE_FACTOR = 4;

    private final IGraphicsTarget target;

    private float brightness = 1.0f;

    private float contrast = 1.0f;

    private boolean interpolated = true;

    private int[] bounds;

    private IExtent imageExtent;

    private Map<Channel, DrawableImage[]> channelMap = new HashMap<>();

    private Map<Channel, Double> gammaMap = new HashMap<>();

    /**
     * The most recently created image to render
     */
    private DrawableImage renderedImage;

    /**
     * While the {@link #renderedImage} has a status of {@link Status#LOADING}
     * then this will contain the most recent valid image with a status of
     * {@link Status#LOADED} so that it can be displayed instead of a blank
     * pane.
     */
    private DrawableImage previousRenderedImage;

    /**
     * Avoid queueing up multiple images in the {@link Status#LOADING} state. If
     * changes occur that cause the {@link #renderedImage} to be outdated while
     * it is still loading then this will be true so that the image will be
     * refreshed once loaded.
     */
    private boolean updateRenderedImage = true;

    public GenericTrueColorImage(IGraphicsTarget target, int[] bounds,
            IExtent imageExtent) {
        this.target = target;
        this.bounds = bounds;
        this.imageExtent = imageExtent;
    }

    @Override
    public void stage() throws VizException {
        getRenderedImage().getImage().stage();
        if (previousRenderedImage != null) {
            previousRenderedImage.dispose();
            previousRenderedImage = null;
        }

    }

    @Override
    public Status getStatus() {
        if (previousRenderedImage != null) {
            return Status.LOADED;
        } else if (renderedImage == null) {
            return Status.UNLOADED;
        } else {
            return renderedImage.getImage().getStatus();
        }
    }

    @Override
    public void setInterpolated(boolean isInterpolated) {
        this.interpolated = isInterpolated;
        if (previousRenderedImage != null) {
            previousRenderedImage.getImage().setInterpolated(isInterpolated);
        }
        if (renderedImage != null) {
            renderedImage.getImage().setInterpolated(isInterpolated);
        }
    }

    @Override
    public void dispose() {
        if (previousRenderedImage != null) {
            previousRenderedImage.dispose();
            previousRenderedImage = null;
        }
        if (renderedImage != null) {
            renderedImage.dispose();
            renderedImage = null;
        }
    }

    @Override
    public int getWidth() {
        return bounds[0];
    }

    @Override
    public int getHeight() {
        return bounds[1];
    }

    @Override
    public void setBrightness(float brightness) {
        this.brightness = brightness;
        if (previousRenderedImage != null) {
            previousRenderedImage.getImage().setBrightness(brightness);
        }
        if (renderedImage != null) {
            renderedImage.getImage().setBrightness(brightness);
        }
    }

    @Override
    public void setContrast(float contrast) {
        this.contrast = contrast;
        if (previousRenderedImage != null) {
            previousRenderedImage.getImage().setContrast(contrast);
        }
        if (renderedImage != null) {
            renderedImage.getImage().setContrast(contrast);
        }
    }

    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return GenericTrueColorImagingExtension.class;
    }

    @Override
    public void setImages(Channel channel, double gamma, DrawableImage... images) {
        /*
         * Try to determine if images has changed at all and if all images have
         * changed. If only some images have changed then assume that the same
         * data is being displayed and continue using the current image until a
         * new one is done. If all images have changed then this image may be
         * for a new frame and the old image is discarded.
         */
        boolean anyChanged;
        boolean allChanged;
        DrawableImage[] oldImages = channelMap.put(channel, images);
        if (oldImages == images) {
            anyChanged = allChanged = false;
        } else if (oldImages == null || images == null) {
            anyChanged = allChanged = true;
        } else if (oldImages.length != images.length) {
            anyChanged = allChanged = true;
            Set<DrawableImage> oldSet = new HashSet<>(Arrays.asList(oldImages));
            for (DrawableImage image : images) {
                if (oldSet.contains(image)) {
                    allChanged = false;
                    break;
                }
            }
        } else {
            anyChanged = false;
            allChanged = true;
            for (int i = 0; i < images.length; ++i) {
                if (images[i].getImage() == oldImages[i].getImage()) {
                    allChanged = false;
                } else {
                    anyChanged = true;
                }
            }
        }
        Double prevGamma = gammaMap.put(channel, gamma);
        if (prevGamma == null || prevGamma != gamma) {
            anyChanged = true;
        }
        if (anyChanged) {
            if (allChanged) {
                dispose();
            } else {
                updateRenderedImage = true;
            }
        }
    }

    @Override
    public DrawableImage[] getImages(Channel channel) {
        return channelMap.get(channel);
    }

    @Override
    public void setSize(int[] bounds) {
        if (Arrays.equals(bounds, this.bounds) == false) {
            this.bounds = bounds;
            updateRenderedImage = true;
        }
    }

    @Override
    public void setImageExtent(IExtent extent) {
        if (extent.equals(this.imageExtent) == false) {
            updateRenderedImage = true;
            this.imageExtent = extent;
        }
    }

    private DrawableImage getRenderedImage() {
        if (renderedImage == null) {
            int[] newBounds = { bounds[0] / DOWNSAMPLE_FACTOR,
                    bounds[1] / DOWNSAMPLE_FACTOR };
            IImage image = target
                    .initializeRaster(new TrueColorRenderedImageCallback(
                            newBounds, imageExtent, channelMap, gammaMap));
            image.setContrast(contrast);
            image.setBrightness(brightness);
            image.setInterpolated(interpolated);
            Coordinate ul = new Coordinate(imageExtent.getMinX(),
                    imageExtent.getMinY());
            Coordinate ur = new Coordinate(imageExtent.getMaxX(),
                    imageExtent.getMinY());
            Coordinate lr = new Coordinate(imageExtent.getMaxX(),
                    imageExtent.getMaxY());
            Coordinate ll = new Coordinate(imageExtent.getMinX(),
                    imageExtent.getMaxY());
            renderedImage = new DrawableImage(image, new PixelCoverage(ul,
                    ur, lr, ll), RasterMode.ASYNCHRONOUS);
            updateRenderedImage = false;
        }
        return renderedImage;
    }

    public List<DrawableImage> getRenderedImages(){
        if(renderedImage != null){
            Status status = renderedImage.getImage().getStatus();
            if(status == Status.LOADED || status == Status.STAGED){
                if(previousRenderedImage != null){
                    previousRenderedImage.dispose();
                    previousRenderedImage = null;
                }
            }
        }
        if (updateRenderedImage && previousRenderedImage == null) {
            previousRenderedImage = renderedImage;
            renderedImage = null;
        }
        if (previousRenderedImage == null) {
            return Collections.singletonList(getRenderedImage());
        }
        List<DrawableImage> images = new ArrayList<>(2);
        images.add(previousRenderedImage);
        images.add(getRenderedImage());
        return images;
    }

}
