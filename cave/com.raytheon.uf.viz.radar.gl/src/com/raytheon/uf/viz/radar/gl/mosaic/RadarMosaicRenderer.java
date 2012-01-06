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
package com.raytheon.uf.viz.radar.gl.mosaic;

import java.nio.ByteBuffer;
import java.util.Arrays;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.radar.gl.MosaicGLTarget;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.radar.rsc.MosaicPaintProperties;
import com.raytheon.viz.radar.rsc.RadarImageResource;
import com.raytheon.viz.radar.rsc.mosaic.RadarMosaicResource;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicRendererFactoryExtension.IRadarMosaicRenderer;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GL Mosaic rendering class using gl frame buffer objects and custom fragment
 * shader.
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
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarMosaicRenderer.class);

    /** The current offscreen texture that is getting rendered to */
    private static IImage currentWrite = null;

    /** This instances offscreen texture */
    private IImage writeTo = null;

    /** Bounds of the offscreen texture */
    private int[] mosaicBounds = null;

    /** Last extent painted, if extent changes, repaint to texture */
    private IExtent lastExtent = null;

    /** The coverage of the offscreen texture (used for drawing on screen) */
    private PixelCoverage writeToCoverage = null;

    private IGraphicsTarget mosaicTarget = null;

    private IGLTarget glTarget = null;

    /**
     * This function returns the current gl image that is being mosaic'd to. It
     * depends on the fact that painting is a synchronous operation and no 2
     * mosaics will be drawing at exactly the same time.
     * 
     * @return currently writing offscreen mosaic texture
     */
    public static IImage getCurrentMosaicImage() {
        return currentWrite;
    }

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

        IGLTarget glTarget = (IGLTarget) target;

        if (glTarget != this.glTarget) {
            this.glTarget = glTarget;
            this.mosaicTarget = new MosaicGLTarget(glTarget);
        }

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
        }

        // If Window size changed, recreate the off screen buffer
        if (Arrays.equals(
                mosaicBounds,
                new int[] { paintProps.getCanvasBounds().width,
                        paintProps.getCanvasBounds().height }) == false) {
            dispose();
            init(target, paintProps, params);
        }

        MosaicPaintProperties props = (MosaicPaintProperties) paintProps;

        synchronized (this) {
            if (props.isForceRepaint()
                    || paintProps.getView().getExtent().equals(lastExtent) == false) {
                IOffscreenRenderingExtension offscreenExt = target
                        .getExtension(IOffscreenRenderingExtension.class);
                offscreenExt.renderOffscreen(writeTo);
                try {
                    currentWrite = writeTo;
                    // paint radar using mosaic target
                    for (ResourcePair rp : mosaicToRender.getResourceList()) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        DataTime time = mosaicToRender.getTimeForResource(rsc);
                        if (rsc instanceof BestResResource) {
                            rsc = ((BestResResource) rsc)
                                    .getBestResResource(time);
                        }
                        if (rsc != null) {
                            RadarImageResource rr = (RadarImageResource) rsc;
                            paintProps.setDataTime(time);
                            rr.paintRadar(mosaicTarget, paintProps);
                        }
                    }
                } finally {
                    offscreenExt.renderOnscreen();
                }

                lastExtent = paintProps.getView().getExtent().clone();

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

            currentWrite = null;
        }
    }

    /**
     * @param target
     */
    private void init(IGraphicsTarget target, PaintProperties paintProps,
            ColorMapParameters params) throws VizException {
        // Construct texture for offscreen rendering
        mosaicBounds = new int[] { paintProps.getCanvasBounds().width,
                paintProps.getCanvasBounds().height };
        writeTo = target
                .getExtension(IOffscreenRenderingExtension.class)
                .constructOffscreenImage(ByteBuffer.class, mosaicBounds, params);
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
