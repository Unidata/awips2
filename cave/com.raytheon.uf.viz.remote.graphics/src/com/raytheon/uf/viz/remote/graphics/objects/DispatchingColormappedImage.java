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
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.UpdateColorMapParametersEvent;

/**
 * Dispatching colormapped image object created from graphics image and forwards
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

public class DispatchingColormappedImage<T extends IColormappedImage> extends
        AbstractDispatchingImage<T> implements IColorMapParametersListener,
        IColormappedImage {

    private IColorMap colorMap;

    /**
     * @param targetObject
     * @param extensionClass
     * @param dispatcher
     */
    public DispatchingColormappedImage(T targetObject,
            Class<? extends IImagingExtension> extensionClass,
            Dispatcher dispatcher, ColorMapParameters parameters) {
        super(targetObject, extensionClass, dispatcher);
        if (parameters != null) {
            parameters.addListener(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IColormappedImage#getColorMapParameters
     * ()
     */
    @Override
    public ColorMapParameters getColorMapParameters() {
        return wrappedObject.getColorMapParameters();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IColormappedImage#setColorMapParameters
     * (com.raytheon.uf.viz.core.drawables.ColorMapParameters)
     */
    @Override
    public void setColorMapParameters(ColorMapParameters params) {
        ColorMapParameters parameters = getColorMapParameters();
        if (params != parameters) {
            if (parameters != null) {
                parameters.removeListener(this);
            }
            wrappedObject.setColorMapParameters(params);
            if (params != null) {
                params.addListener(this);
                updateColorMapParameters();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColormappedImage#getValue(int,
     * int)
     */
    @Override
    public double getValue(int x, int y) {
        return wrappedObject.getValue(x, y);
    }

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
            updateColorMapParameters();
        }
    }

    public void updateColorMapParameters() {
        ColorMapParameters parameters = getColorMapParameters();
        dispatch(UpdateColorMapParametersEvent.createEvent(this, parameters));
        if (parameters.getColorMap() != colorMap) {
            colorMap = parameters.getColorMap();
            dispatch(UpdateColorMapEvent.createEvent(this, colorMap));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingImage#
     * dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        ColorMapParameters params = getColorMapParameters();
        if (params != null) {
            params.removeListener(this);
        }
    }

}
