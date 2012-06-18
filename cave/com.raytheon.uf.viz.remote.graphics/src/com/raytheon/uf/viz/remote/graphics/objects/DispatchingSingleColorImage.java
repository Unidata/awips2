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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension.ISingleColorImage;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.imagery.CreateSingleColorImage;
import com.raytheon.uf.viz.remote.graphics.events.imagery.UpdateSingleColorImage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingSingleColorImage extends DispatchingImage implements
        ISingleColorImage {

    private RGB color;

    /**
     * @param targetObject
     * @param extensionClass
     * @param dispatcher
     */
    public DispatchingSingleColorImage(ISingleColorImage targetObject,
            Class<? extends IImagingExtension> extensionClass,
            Dispatcher dispatcher, RGB startingColor,
            DispatchingRenderedImageCallback callback) {
        super(targetObject, extensionClass, callback, dispatcher);
        color = startingColor;
        CreateSingleColorImage event = RemoteGraphicsEventFactory.createEvent(
                CreateSingleColorImage.class, this);
        event.setColor(color);
        dispatch(event);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension.
     * ISingleColorImage#setColor(org.eclipse.swt.graphics.RGB)
     */
    @Override
    public void setColor(RGB color) {
        if (color.equals(this.color) == false) {
            this.color = color;
            ((ISingleColorImage) wrappedObject).setColor(color);
            UpdateSingleColorImage event = RemoteGraphicsEventFactory
                    .createEvent(UpdateSingleColorImage.class, this);
            event.setColor(color);
            dispatch(event);
        }
    }

}
