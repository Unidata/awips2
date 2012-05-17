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
package com.raytheon.uf.viz.remote.graphics.events.rendering;

import java.lang.reflect.Array;
import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract class for bulk rendering other objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@SuppressWarnings("unchecked")
@DynamicSerialize
public abstract class AbstractRemoteGraphicsBulkRenderEvent<T extends IRenderEvent>
        extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private T[] objects;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#createDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public AbstractRemoteGraphicsBulkRenderEvent<T> createDiffObject(
            IRenderEvent event) {
        AbstractRemoteGraphicsBulkRenderEvent<T> diff = (AbstractRemoteGraphicsBulkRenderEvent<T>) event;
        try {
            AbstractRemoteGraphicsBulkRenderEvent<T> diffEvent = (AbstractRemoteGraphicsBulkRenderEvent<T>) event
                    .getClass().newInstance();
            if (diff.objects != null) {
                if (objects != null && diff.objects.length == objects.length) {
                    diffEvent.objects = (T[]) Array.newInstance(
                            diff.getObjectClass(), diff.objects.length);
                    for (int i = 0; i < objects.length; ++i) {
                        T paintEvent = objects[i];
                        T diffPaintEvent = diff.objects[i];
                        if (paintEvent.equals(diffPaintEvent) == false) {
                            diffEvent.objects[i] = (T) paintEvent
                                    .createDiffObject(diffPaintEvent);
                        }
                    }
                } else {
                    diffEvent.objects = diff.objects;
                }
            }
            return diffEvent;
        } catch (Exception e) {
            throw new RuntimeException("Error creating diff object", e);
        }
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
        AbstractRemoteGraphicsBulkRenderEvent<T> event = (AbstractRemoteGraphicsBulkRenderEvent<T>) diffEvent;
        T[] diffImageEvents = event.objects;
        if (diffImageEvents == null) {
            objects = null;
        } else if (objects == null) {
            objects = event.objects;
        } else if (objects.length != diffImageEvents.length) {
            objects = event.objects;
        } else {
            for (int i = 0; i < objects.length; ++i) {
                T diffPaintEvent = diffImageEvents[i];
                if (diffPaintEvent != null) {
                    objects[i].applyDiffObject(diffPaintEvent);
                }
            }
        }
    }

    /**
     * @return the objects
     */
    public T[] getObjects() {
        return objects;
    }

    /**
     * @param objects
     *            the objects to set
     */
    public void setObjects(T[] objects) {
        this.objects = objects;
    }

    protected abstract Class<T> getObjectClass();

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
        AbstractRemoteGraphicsBulkRenderEvent<T> other = (AbstractRemoteGraphicsBulkRenderEvent<T>) obj;
        if (!Arrays.equals(objects, other.objects))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() {
        try {
            AbstractRemoteGraphicsBulkRenderEvent<T> newObject = getClass()
                    .newInstance();
            if (objects != null) {
                newObject.objects = (T[]) Array.newInstance(getObjectClass(),
                        objects.length);
                for (int i = 0; i < objects.length; ++i) {
                    newObject.objects[i] = (T) objects[i].clone();
                }
            }
            return newObject;
        } catch (Exception e) {
            throw new RuntimeException("Could not clone "
                    + getClass().getSimpleName() + " object");
        }
    }

}
