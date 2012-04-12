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
package com.raytheon.uf.viz.remote.graphics.extensions;

import java.nio.Buffer;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;
import com.raytheon.uf.viz.remote.graphics.events.offscreen.CreateOffscreenImageEvent;
import com.raytheon.uf.viz.remote.graphics.events.offscreen.RenderOffscreenEvent;
import com.raytheon.uf.viz.remote.graphics.events.offscreen.RenderOnscreenEvent;
import com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingImage;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingOffscreenImage;

/**
 * Offscreen rendering extension that creates remote images for offscreen
 * rendering
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingOffscreenRenderingExtension extends
        GraphicsExtension<DispatchGraphicsTarget> implements
        IOffscreenRenderingExtension {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * renderOffscreen(com.raytheon.uf.viz.core.drawables.IImage)
     */
    @Override
    public void renderOffscreen(IImage offscreenImage) throws VizException {
        renderOffscreen(offscreenImage, target.getView().getExtent());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * renderOffscreen(com.raytheon.uf.viz.core.drawables.IImage)
     */
    @Override
    public void renderOffscreen(IImage offscreenImage, IExtent extent)
            throws VizException {
        AbstractDispatchingImage<?> image = (AbstractDispatchingImage<?>) offscreenImage;
        // Render off screen for target image
        target.getWrappedObject()
                .getExtension(IOffscreenRenderingExtension.class)
                .renderOffscreen(image.getWrappedObject(), extent);
        // Send event for offscreen rendering
        RenderOffscreenEvent event = RemoteGraphicsEventFactory.createEvent(
                RenderOffscreenEvent.class, image);
        event.setIExtent(extent);
        image.dispatch(event);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * renderOnscreen()
     */
    @Override
    public void renderOnscreen() throws VizException {
        target.getWrappedObject()
                .getExtension(IOffscreenRenderingExtension.class)
                .renderOnscreen();
        // Send event
        target.dispatch(RemoteGraphicsEventFactory.createEvent(
                RenderOnscreenEvent.class, target));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(int[])
     */
    @Override
    public IImage constructOffscreenImage(int[] dimensions) throws VizException {
        IImage wrapped = target.getWrappedObject()
                .getExtension(IOffscreenRenderingExtension.class)
                .constructOffscreenImage(dimensions);
        return createOffscreenImage(wrapped, null, dimensions, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(java.lang.Class, int[])
     */
    @Override
    public IImage constructOffscreenImage(Class<? extends Buffer> dataType,
            int[] dimensions) throws VizException {
        return constructOffscreenImage(dataType, dimensions, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.IOffscreenRenderingExtension#
     * constructOffscreenImage(java.lang.Class, int[],
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public IImage constructOffscreenImage(Class<? extends Buffer> dataType,
            int[] dimensions, ColorMapParameters parameters)
            throws VizException {
        IImage offscreenImage = target.getWrappedObject()
                .getExtension(IOffscreenRenderingExtension.class)
                .constructOffscreenImage(dataType, dimensions, parameters);
        return createOffscreenImage(offscreenImage, dataType, dimensions,
                parameters);
    }

    private DispatchingOffscreenImage createOffscreenImage(IImage wrapped,
            Class<? extends Buffer> dataType, int[] dimensions,
            ColorMapParameters parameters) {
        DispatchingOffscreenImage wrapper = new DispatchingOffscreenImage(
                wrapped, target.getDispatcher(), parameters);
        // Send event of offscreen image creation
        CreateOffscreenImageEvent event = RemoteGraphicsEventFactory
                .createEvent(CreateOffscreenImageEvent.class, wrapper);
        event.setDimensions(dimensions);
        if (dataType != null) {
            event.setBufferType(dataType.getName());
        }
        if (parameters != null) {
            event.setColorMapParamters(UpdateColorMapParametersEvent
                    .createEvent(wrapper, parameters));
        }
        wrapper.dispatch(event);
        return wrapper;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(DispatchGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
