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
package com.raytheon.viz.core.gl.images;

import javax.measure.unit.Unit;
import javax.media.opengl.GL;

import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * GL-based colormapped image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            mschenke    Initial creation
 * Mar 21, 2013 1806       bsteffen    Update GL mosaicing to use dynamic data
 *                                     format for offscreen textures.
 * Oct 16, 2013 2333       mschenke    Moved shared logic into base class
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class GLColormappedImage extends AbstractGLColormappedImage {

    protected GLRetrievableCMTextureData data;

    public GLColormappedImage(IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters params,
            Class<? extends IImagingExtension> extensionClass) {
        super(GLRetrievableCMTextureData.getGlTextureId(dataCallback), params,
                extensionClass);
        this.data = (GLRetrievableCMTextureData) super.data;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.internal.GLImage#loadTexture(javax.media.opengl
     * .GLContext)
     */
    @Override
    public void loadTexture(GL gl) throws VizException {
        super.loadTexture(gl);
        // No need to keep around texture data after loading
        data.disposeTextureData();
    }

    public ColorMapDataType getColorMapDataType() {
        return data.getColorMapDataType();
    }

    @Override
    public double getValue(int x, int y) {
        double val = Double.NaN;
        if (data != null) {
            val = data.getValue(x, y);
        }
        return val;
    }

    @Override
    public void dispose() {
        super.dispose();
        data = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.images.AbstractGLImage#usaAsFrameBuffer()
     */
    @Override
    public void usaAsFrameBuffer() throws VizException {
        data.disposeTextureData();
        super.usaAsFrameBuffer();
    }

    @Override
    public Unit<?> getDataUnit() {
        if (data != null && data.getDataUnit() != null) {
            return data.getDataUnit();
        }
        return getColorMapParameters().getDataUnit();
    }

}
