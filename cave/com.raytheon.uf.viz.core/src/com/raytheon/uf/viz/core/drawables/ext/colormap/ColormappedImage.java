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
package com.raytheon.uf.viz.core.drawables.ext.colormap;

import java.awt.image.RenderedImage;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.data.prep.Colormapper;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColorMapParametersListener;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * General colormapped image, regenerates image if parameters change
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColormappedImage implements IColormappedImage,
        IRenderedImageCallback, IColorMapParametersListener {

    private IImage image;

    private IColorMapDataRetrievalCallback callback;

    private ColorMapParameters parameters;

    public ColormappedImage(IGraphicsTarget target,
            IColorMapDataRetrievalCallback callback,
            ColorMapParameters parameters) {
        this.callback = callback;
        setColorMapParameters(parameters);
        image = target.initializeRaster(this);
    }

    /**
     * Get the wrapped image for the colormapped image
     * 
     * @return
     */
    public IImage getWrappedImage() {
        return image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getStatus()
     */
    @Override
    public Status getStatus() {
        return image.getStatus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#setInterpolated(boolean)
     */
    @Override
    public void setInterpolated(boolean isInterpolated) {
        image.setInterpolated(isInterpolated);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#dispose()
     */
    @Override
    public void dispose() {
        if (image != null) {
            image.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getWidth()
     */
    @Override
    public int getWidth() {
        return image.getWidth();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getHeight()
     */
    @Override
    public int getHeight() {
        return image.getHeight();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#setBrightness(float)
     */
    @Override
    public void setBrightness(float brightness) {
        image.setBrightness(brightness);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#setContrast(float)
     */
    @Override
    public void setContrast(float contrast) {
        image.setContrast(contrast);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#getExtensionClass()
     */
    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return GeneralColormappedImageExtension.class;
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
        return parameters;
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
        if (params != this.parameters) {
            if (this.parameters != null) {
                this.parameters.removeListener(this);
            }
            this.parameters = params;
            if (this.parameters != null) {
                this.parameters.addListener(this);
            }
            dispose();
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
        return Double.NaN;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.data.IRenderedImageCallback#getImage()
     */
    @Override
    public RenderedImage getImage() throws VizException {
        if (parameters == null || parameters.getColorMap() == null) {
            return null;
        }
        ColorMapData colorMapData = callback.getColorMapData();
        return Colormapper.colorMap(colorMapData, parameters);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColorMapParametersListener#
     * colorMapChanged()
     */
    @Override
    public void colorMapChanged() {
        dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IImage#stage()
     */
    @Override
    public void stage() throws VizException {
        image.stage();
    }

}
