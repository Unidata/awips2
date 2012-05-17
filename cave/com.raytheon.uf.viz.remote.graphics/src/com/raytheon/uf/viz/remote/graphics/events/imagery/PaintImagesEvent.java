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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsBulkRenderEvent;
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
public class PaintImagesEvent extends
        AbstractRemoteGraphicsBulkRenderEvent<PaintImageEvent> {

    @DynamicSerializeElement
    private float alpha;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent
     * #createDiffObject(com.raytheon.uf.viz
     * .remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public PaintImagesEvent createDiffObject(IRenderEvent event) {
        PaintImagesEvent diffObject = (PaintImagesEvent) super
                .createDiffObject(event);
        diffObject.alpha = ((PaintImagesEvent) event).alpha;
        return diffObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent
     * #applyDiffObject(com.raytheon.uf.viz.
     * remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        super.applyDiffObject(diffEvent);
        this.alpha = ((PaintImagesEvent) diffEvent).alpha;
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
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent#getObjectClass()
     */
    @Override
    protected Class<PaintImageEvent> getObjectClass() {
        return PaintImageEvent.class;
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
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        PaintImagesEvent other = (PaintImagesEvent) obj;
        if (Float.floatToIntBits(alpha) != Float.floatToIntBits(other.alpha))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent#clone()
     */
    @Override
    public Object clone() {
        PaintImagesEvent event = (PaintImagesEvent) super.clone();
        event.alpha = alpha;
        return event;
    }

}
