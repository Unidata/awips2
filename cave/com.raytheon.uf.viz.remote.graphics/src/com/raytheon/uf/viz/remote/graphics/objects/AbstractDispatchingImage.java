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
package com.raytheon.uf.viz.remote.graphics.objects;

import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.imagery.UpdateImageDataEvent;

/**
 * Abstract dispatching image object created from graphics image and forwards
 * key events to remote clients
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractDispatchingImage<T extends IImage> extends
        DispatchingObject<T> implements IImage {

    protected boolean dirty = false;

    protected UpdateImageDataEvent imageData;

    private Class<? extends IImagingExtension> extensionClass;

    /**
     * @param targetObject
     * @param dispatcher
     */
    public AbstractDispatchingImage(T targetObject,
            Class<? extends IImagingExtension> extensionClass,
            Dispatcher dispatcher) {
        super(targetObject, dispatcher);
        this.extensionClass = extensionClass;
        this.imageData = RemoteGraphicsEventFactory.createEvent(
                UpdateImageDataEvent.class, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getStatus()
     */
    @Override
    public Status getStatus() {
        return wrappedObject.getStatus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#setInterpolated(boolean)
     */
    @Override
    public void setInterpolated(boolean isInterpolated) {
        if (isInterpolated != imageData.isInterpolated()) {
            imageData.setInterpolated(isInterpolated);
            wrappedObject.setInterpolated(isInterpolated);
            dirty = true;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#dispose()
     */
    @Override
    public void dispose() {
        wrappedObject.dispose();
        // Dispatch the dispose
        dispatch(RemoteGraphicsEventFactory.createEvent(
                DisposeObjectEvent.class, this));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getWidth()
     */
    @Override
    public int getWidth() {
        return wrappedObject.getWidth();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getHeight()
     */
    @Override
    public int getHeight() {
        return wrappedObject.getHeight();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#setBrightness(float)
     */
    @Override
    public void setBrightness(float brightness) {
        if (brightness != imageData.getBrightness()) {
            imageData.setBrightness(brightness);
            wrappedObject.setBrightness(brightness);
            dirty = true;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#setContrast(float)
     */
    @Override
    public void setContrast(float contrast) {
        if (contrast != imageData.getContrast()) {
            imageData.setContrast(contrast);
            wrappedObject.setContrast(contrast);
            dirty = true;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#stage()
     */
    @Override
    public void stage() throws VizException {
        wrappedObject.stage();
    }

    /**
     * Dispatches event updating image state
     */
    public void updateState() {
        if (dirty) {
            dispatch(imageData);
            dirty = false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getExtensionClass()
     */
    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return extensionClass;
    }

}
