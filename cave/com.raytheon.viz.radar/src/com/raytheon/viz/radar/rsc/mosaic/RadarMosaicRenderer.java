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
package com.raytheon.viz.radar.rsc.mosaic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.radar.rsc.MosaicPaintProperties;
import com.raytheon.viz.radar.rsc.RadarImageResource;
import com.raytheon.viz.radar.rsc.mosaic.RadarMosaicRendererFactory.IRadarMosaicRenderer;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicImageExtension.IMosaicImage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Radar Mosaic rendering class using IRadarMosaicImageExtension to render
 * mosaic image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarMosaicRenderer implements IRadarMosaicRenderer,
        IRefreshListener {

    /** This instances offscreen texture */
    private IMosaicImage writeTo = null;

    /** Last extent painted, if extent changes, repaint to texture */
    private IExtent lastExtent = null;

    /** The coverage of the offscreen texture (used for drawing on screen) */
    private PixelCoverage writeToCoverage = null;

    /**
     * Default constructor, needed since class is instantiated through eclipse
     * extension point
     */
    public RadarMosaicRenderer() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.IRadarMosaicRenderer#mosaic(com.raytheon.viz
     * .radar.rsc.IGraphicsTarget, com.raytheon.viz.radar.rsc.PaintProperties,
     * com.raytheon.viz.radar.rsc.MaxMosaicResource)
     */
    @Override
    public void mosaic(IGraphicsTarget target, PaintProperties paintProps,
            RadarMosaicResource mosaicToRender) throws VizException {

        // Get the image tiles we are going to render

        ColorMapParameters params = mosaicToRender.getCapability(
                ColorMapCapability.class).getColorMapParameters();

        // If first paint, initialize and wait for next paint
        if (writeTo == null) {
            init(target, paintProps, params);

            // Listen for refreshes on the underlying resources
            for (ResourcePair rp : mosaicToRender.getResourceList()) {
                if (rp.getResource() != null) {
                    rp.getResource().registerListener(this);
                }
            }
            mosaicToRender.registerListener(this);
        } else if (Arrays.equals(
                new int[] { writeTo.getWidth(), writeTo.getHeight() },
                new int[] { paintProps.getCanvasBounds().width,
                        paintProps.getCanvasBounds().height }) == false) {
            // If Window size changed, recreate the off screen buffer
            dispose();
            init(target, paintProps, params);
        }

        MosaicPaintProperties props = (MosaicPaintProperties) paintProps;

        synchronized (this) {
            if (props.isForceRepaint()
                    || paintProps.getView().getExtent().equals(lastExtent) == false) {
                List<DrawableImage> images = new ArrayList<DrawableImage>();

                // paint radar using mosaic target
                for (ResourcePair rp : mosaicToRender.getResourceList()) {
                    AbstractVizResource<?, ?> rsc = rp.getResource();
                    DataTime time = mosaicToRender.getTimeForResource(rsc);
                    if (rsc instanceof BestResResource) {
                        rsc = ((BestResResource) rsc).getBestResResource(time);
                    }
                    if (rsc != null && time != null) {
                        RadarImageResource rr = (RadarImageResource) rsc;
                        DrawableImage di = rr.getImage(target, time);
                        if (di != null && di.getImage() != null
                                && di.getCoverage() != null
                                && di.getCoverage().getMesh() != null) {
                            // If image is ready to go, add
                            images.add(di);
                        } else {
                            mosaicToRender.issueRefresh();
                        }
                    }
                }

                writeTo.setImagesToMosaic(images
                        .toArray(new DrawableImage[images.size()]));
                lastExtent = paintProps.getView().getExtent().clone();
                writeTo.setImageExtent(lastExtent);

                Coordinate ul = new Coordinate(lastExtent.getMinX(),
                        lastExtent.getMaxY());
                Coordinate ur = new Coordinate(lastExtent.getMaxX(),
                        lastExtent.getMaxY());
                Coordinate lr = new Coordinate(lastExtent.getMaxX(),
                        lastExtent.getMinY());
                Coordinate ll = new Coordinate(lastExtent.getMinX(),
                        lastExtent.getMinY());

                writeToCoverage = new PixelCoverage(ul, ur, lr, ll);
            }

            writeTo.setContrast(mosaicToRender.getCapability(
                    ImagingCapability.class).getContrast());
            writeTo.setBrightness(mosaicToRender.getCapability(
                    ImagingCapability.class).getBrightness());

            target.drawRaster(writeTo, writeToCoverage, paintProps);
        }
    }

    /**
     * @param target
     */
    private void init(IGraphicsTarget target, PaintProperties paintProps,
            ColorMapParameters params) throws VizException {
        // Construct texture for mosaicing
        writeTo = target.getExtension(IRadarMosaicImageExtension.class)
                .initializeRaster(
                        new int[] { paintProps.getCanvasBounds().width,
                                paintProps.getCanvasBounds().height },
                        paintProps.getView().getExtent(), params);
    }

    @Override
    public void dispose() {
        // Dispose of all data, offscreen texture
        if (writeTo != null) {
            writeTo.dispose();
            writeTo = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IRefreshListener#refresh()
     */
    @Override
    public void refresh() {
        synchronized (this) {
            lastExtent = null;
        }
    }

}
