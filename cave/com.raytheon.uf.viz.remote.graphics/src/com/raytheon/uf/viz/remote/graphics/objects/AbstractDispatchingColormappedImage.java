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

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColorMapParametersListener;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;

/**
 * TODO Add Description
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

public abstract class AbstractDispatchingColormappedImage<T extends IImage>
        extends AbstractDispatchingImage<T> implements
        IColorMapParametersListener {

    private IColorMap colorMap;

    /**
     * @param targetObject
     * @param extensionClass
     * @param dispatcher
     */
    public AbstractDispatchingColormappedImage(T targetObject,
            Class<? extends IImagingExtension> extensionClass,
            Dispatcher dispatcher, ColorMapParameters parameters) {
        super(targetObject, extensionClass, dispatcher);
        if (parameters != null) {
            parameters.addListener(this);
        }
    }

    public abstract ColorMapParameters getColorMapParameters();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColorMapParametersListener#
     * colorMapChanged()
     */
    @Override
    public void colorMapChanged() {
        ColorMapParameters parameters = getColorMapParameters();
        if (parameters != null) {
            dispatch(createColorMapParametersUpdateEvent(parameters));
        }
    }

    protected UpdateColorMapParametersEvent createColorMapParametersUpdateEvent(
            ColorMapParameters parameters) {
        UpdateColorMapParametersEvent event = UpdateColorMapParametersEvent
                .createEvent(this, parameters);
        if (parameters.getColorMap() == colorMap && colorMap != null) {
            // Same colormap, discard cm data
            event.setRed(null);
            event.setBlue(null);
            event.setGreen(null);
            event.setAlpha(null);
        } else {
            colorMap = parameters.getColorMap();
        }
        return event;
    }

}
