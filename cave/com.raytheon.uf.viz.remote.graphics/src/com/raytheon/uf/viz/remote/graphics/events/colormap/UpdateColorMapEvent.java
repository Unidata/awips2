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
package com.raytheon.uf.viz.remote.graphics.events.colormap;

import java.util.Arrays;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingImage;

/**
 * Event for updating the IColorMap on an IColormappedImage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 25, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class UpdateColorMapEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private float[] red;

    @DynamicSerializeElement
    private float[] blue;

    @DynamicSerializeElement
    private float[] green;

    @DynamicSerializeElement
    private float[] alpha;

    /**
     * @return the red
     */
    public float[] getRed() {
        return red;
    }

    /**
     * @param red
     *            the red to set
     */
    public void setRed(float[] red) {
        this.red = red;
    }

    /**
     * @return the blue
     */
    public float[] getBlue() {
        return blue;
    }

    /**
     * @param blue
     *            the blue to set
     */
    public void setBlue(float[] blue) {
        this.blue = blue;
    }

    /**
     * @return the green
     */
    public float[] getGreen() {
        return green;
    }

    /**
     * @param green
     *            the green to set
     */
    public void setGreen(float[] green) {
        this.green = green;
    }

    /**
     * @return the alpha
     */
    public float[] getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(float[] alpha) {
        this.alpha = alpha;
    }

    public IColorMap getColorMap() {
        return createColormap(red, green, blue, alpha);
    }

    public void setColorMap(IColorMap colorMap) {
        if (colorMap != null) {
            red = colorMap.getRed();
            green = colorMap.getGreen();
            blue = colorMap.getBlue();
            alpha = colorMap.getAlpha();
        }
    }

    /**
     * @param wrapper
     * @param parameters
     * @return
     */
    public static UpdateColorMapEvent createEvent(
            AbstractDispatchingImage<?> wrapper, IColorMap colorMap) {
        UpdateColorMapEvent event = RemoteGraphicsEventFactory.createEvent(
                UpdateColorMapEvent.class, wrapper);
        event.setColorMap(colorMap);
        return event;
    }

    /**
     * Given the component colros creates a colormap with a unique name.
     * 
     * @param red
     * @param green
     * @param blue
     * @param alpha
     * @return
     */
    public static ColorMap createColormap(float[] red, float[] green,
            float[] blue, float[] alpha) {
        ColorMap colorMap = null;
        if (red != null && green != null && blue != null && alpha != null) {
            // The target may be caching colormaps based off the name so make
            // the name as unique as possible, we do not want to use the same
            // name as the data provider because we may have a local colormap
            // with the same name but different colors.
            StringBuilder colorMapName = new StringBuilder("remoteColormap");
            colorMapName.append("-red=");
            colorMapName.append(Arrays.hashCode(red));
            colorMapName.append("-green=");
            colorMapName.append(Arrays.hashCode(green));
            colorMapName.append("-blue=");
            colorMapName.append(Arrays.hashCode(blue));
            colorMapName.append("-alpha=");
            colorMapName.append(Arrays.hashCode(alpha));
            colorMap = new ColorMap(colorMapName.toString(), red, green, blue,
                    alpha);
        }
        return colorMap;
    }
}
