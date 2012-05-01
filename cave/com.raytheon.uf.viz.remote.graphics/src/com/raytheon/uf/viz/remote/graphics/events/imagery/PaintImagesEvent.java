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
package com.raytheon.uf.viz.remote.graphics.events.imagery;

import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;
import com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingImage;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingMesh;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class PaintImagesEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private float alpha;

    @DynamicSerializeElement
    private PaintImageEvent[] imageEvents;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#createDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public PaintImagesEvent createDiffObject(IRenderEvent event) {
        PaintImagesEvent diff = (PaintImagesEvent) event;
        PaintImagesEvent diffEvent = new PaintImagesEvent();
        diffEvent.alpha = diff.alpha;
        if (diff.imageEvents != null) {
            if (imageEvents != null
                    && diff.imageEvents.length == imageEvents.length) {
                diffEvent.imageEvents = new PaintImageEvent[diff.imageEvents.length];
                for (int i = 0; i < imageEvents.length; ++i) {
                    PaintImageEvent paintEvent = imageEvents[i];
                    PaintImageEvent diffPaintEvent = diff.imageEvents[i];
                    if (paintEvent.equals(diffPaintEvent) == false) {
                        diffEvent.imageEvents[i] = paintEvent
                                .createDiffObject(diffPaintEvent);
                    }
                }
            } else {
                diffEvent.imageEvents = diff.imageEvents;
            }
        }
        return diffEvent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        PaintImagesEvent event = (PaintImagesEvent) diffEvent;
        PaintImageEvent[] diffImageEvents = event.imageEvents;
        alpha = event.alpha;
        if (diffImageEvents == null) {
            imageEvents = null;
        } else if (imageEvents == null) {
            imageEvents = event.imageEvents;
        } else if (imageEvents.length != diffImageEvents.length) {
            imageEvents = event.imageEvents;
        } else {
            for (int i = 0; i < imageEvents.length; ++i) {
                PaintImageEvent diffPaintEvent = diffImageEvents[i];
                if (diffPaintEvent != null) {
                    imageEvents[i].applyDiffObject(diffPaintEvent);
                }
            }
        }
    }

    /**
     * @return the imageEvents
     */
    public PaintImageEvent[] getImageEvents() {
        return imageEvents;
    }

    /**
     * @param imageEvents
     *            the imageEvents to set
     */
    public void setImageEvents(PaintImageEvent[] imageEvents) {
        this.imageEvents = imageEvents;
    }

    /**
     * @return the alpha
     */
    public float getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }

    /**
     * @param images
     *            the images to set
     */
    public static PaintImageEvent[] toPaintEvents(DrawableImage[] images) {
        PaintImageEvent[] imageEvents = new PaintImageEvent[images.length];

        for (int i = 0; i < images.length; ++i) {
            DrawableImage di = images[i];
            AbstractDispatchingImage<?> dispatchingImage = (AbstractDispatchingImage<?>) di
                    .getImage();

            // If image parameters have been modified, update
            dispatchingImage.updateState();

            PixelCoverage coverage = di.getCoverage();
            PixelCoverage targetCoverage = new PixelCoverage(coverage.getUl(),
                    coverage.getUr(), coverage.getLr(), coverage.getLl());

            PaintImageEvent paintEvent = RemoteGraphicsEventFactory
                    .createEvent(PaintImageEvent.class, dispatchingImage);
            paintEvent.setPixelCoverage(coverage);

            IMesh mesh = coverage.getMesh();
            IMesh targetMesh = null;
            if (mesh != null) {
                DispatchingMesh dmesh = (DispatchingMesh) mesh;
                targetMesh = dmesh.getWrappedObject();
                paintEvent.setMeshId(dmesh.getObjectId());
                targetCoverage.setMesh(targetMesh);
            }

            imageEvents[i] = paintEvent;
        }
        return imageEvents;
    }

    /**
     * @param images
     *            the images to set
     */
    public static DrawableImage[] extractTargetImages(DrawableImage[] images) {
        DrawableImage[] targeted = new DrawableImage[images.length];

        for (int i = 0; i < images.length; ++i) {
            DrawableImage di = images[i];
            AbstractDispatchingImage<?> dispatchingImage = (AbstractDispatchingImage<?>) di
                    .getImage();

            IImage targetImage = dispatchingImage.getWrappedObject();
            PixelCoverage coverage = di.getCoverage();
            PixelCoverage targetCoverage = new PixelCoverage(coverage.getUl(),
                    coverage.getUr(), coverage.getLr(), coverage.getLl());

            IMesh mesh = coverage.getMesh();
            if (mesh != null) {
                DispatchingMesh dmesh = (DispatchingMesh) mesh;
                targetCoverage.setMesh(dmesh.getWrappedObject());
            }

            targeted[i] = new DrawableImage(targetImage, targetCoverage,
                    di.getMode());
        }
        return targeted;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        PaintImagesEvent other = (PaintImagesEvent) obj;
        if (Float.floatToIntBits(alpha) != Float.floatToIntBits(other.alpha))
            return false;
        if (!Arrays.equals(imageEvents, other.imageEvents))
            return false;
        return true;
    }

}
