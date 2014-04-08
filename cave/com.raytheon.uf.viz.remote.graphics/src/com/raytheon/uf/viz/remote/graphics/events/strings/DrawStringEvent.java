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
package com.raytheon.uf.viz.remote.graphics.events.strings;

import java.util.Arrays;
import java.util.EnumSet;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingFont;

/**
 * Drawing event that wraps a DrawableString object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 10, 2012           mschenke    Initial creation
 * Apr 04, 2014  2920     bsteffen    Allow strings to use mulitple styles.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

@DynamicSerialize
public class DrawStringEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private int fontId = -1;

    @DynamicSerializeElement
    private String[] text;

    @DynamicSerializeElement
    private RGB[] colors;

    @DynamicSerializeElement
    private float alpha;

    @DynamicSerializeElement
    private HorizontalAlignment horizontalAlignment;

    @DynamicSerializeElement
    private VerticalAlignment verticalAlignment;

    @DynamicSerializeElement
    private EnumSet<TextStyle> textStyles;

    @DynamicSerializeElement
    private RGB boxColor;

    @DynamicSerializeElement
    private RGB shadowColor;

    @DynamicSerializeElement
    private double magnification = 1.0f;

    @DynamicSerializeElement
    private double rotation = 0.0;

    @DynamicSerializeElement
    private double[] point;

    @DynamicSerializeElement
    private boolean xOrColors;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsRenderEvent
     * #createDiffObject(com.raytheon.uf.viz.remote
     * .graphics.events.rendering.IRenderEvent)
     */
    @Override
    public DrawStringEvent createDiffObject(IRenderEvent event) {
        DrawStringEvent diffEvent = (DrawStringEvent) event;
        DrawStringEvent diffObject = new DrawStringEvent();
        diffObject.alpha = diffEvent.alpha;
        diffObject.boxColor = diffEvent.boxColor;
        diffObject.shadowColor = diffEvent.shadowColor;
        diffObject.xOrColors = diffEvent.xOrColors;
        diffObject.fontId = diffEvent.fontId;
        diffObject.magnification = diffEvent.magnification;
        diffObject.rotation = diffEvent.rotation;
        if (Arrays.equals(colors, diffEvent.colors) == false) {
            diffObject.colors = diffEvent.colors;
        }
        if (Arrays.equals(text, diffEvent.text) == false) {
            diffObject.text = diffEvent.text;
        }
        if (Arrays.equals(point, diffEvent.point) == false) {
            diffObject.point = diffEvent.point;
        }
        if (horizontalAlignment != diffEvent.horizontalAlignment) {
            diffObject.horizontalAlignment = diffEvent.horizontalAlignment;
        }
        if (verticalAlignment != diffEvent.verticalAlignment) {
            diffObject.verticalAlignment = diffEvent.verticalAlignment;
        }
        if (!textStyles.equals(diffEvent.textStyles)) {
            diffObject.textStyles = diffEvent.textStyles;
        }
        return diffObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent
     * #applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        DrawStringEvent diffObject = (DrawStringEvent) diffEvent;
        alpha = diffObject.alpha;
        boxColor = diffObject.boxColor;
        shadowColor = diffObject.shadowColor;
        xOrColors = diffObject.xOrColors;
        if (diffObject.colors != null) {
            colors = diffObject.colors;
        }
        fontId = diffObject.fontId;
        if (diffObject.horizontalAlignment != null) {
            horizontalAlignment = diffObject.horizontalAlignment;
        }
        if (diffObject.verticalAlignment != null) {
            verticalAlignment = diffObject.verticalAlignment;
        }
        magnification = diffObject.magnification;
        if (diffObject.point != null) {
            point = diffObject.point;
        }
        rotation = diffObject.rotation;
        if (diffObject.text != null) {
            text = diffObject.text;
        }
        if (diffObject.textStyles != null) {
            textStyles = diffObject.textStyles;
        }
    }

    public void setDrawableString(DrawableString string) {
        this.text = string.getText();
        this.colors = string.getColors();
        this.alpha = string.basics.alpha;
        this.boxColor = string.boxColor;
        this.shadowColor = string.shadowColor;
        this.xOrColors = string.basics.xOrColors;
        this.horizontalAlignment = string.horizontalAlignment;
        this.verticalAlignment = string.verticallAlignment;
        this.textStyles = string.getTextStyles();
        this.magnification = string.magnification;
        this.point = new double[] { string.basics.x, string.basics.y,
                string.basics.z };
        this.rotation = string.rotation;
        if (string.font instanceof DispatchingFont) {
            fontId = ((DispatchingFont) string.font).getObjectId();
        }
    }

    public DrawableString getDrawableString() {
        DrawableString ds = new DrawableString(text, colors);
        ds.basics.alpha = alpha;
        ds.basics.xOrColors = xOrColors;
        ds.boxColor = boxColor;
        ds.shadowColor = shadowColor;
        ds.horizontalAlignment = horizontalAlignment;
        ds.verticallAlignment = verticalAlignment;
        for (TextStyle textStyle : textStyles) {
            ds.addTextStyle(textStyle);
        }
        ds.magnification = magnification;
        ds.setCoordinates(point[0], point[1], point[2]);
        ds.rotation = rotation;
        return ds;
    }

    /**
     * @return the fontId
     */
    public int getFontId() {
        return fontId;
    }

    /**
     * @param fontId
     *            the fontId to set
     */
    public void setFontId(int fontId) {
        this.fontId = fontId;
    }

    /**
     * @return the text
     */
    public String[] getText() {
        return text;
    }

    /**
     * @param text
     *            the text to set
     */
    public void setText(String[] text) {
        this.text = text;
    }

    /**
     * @return the colors
     */
    public RGB[] getColors() {
        return colors;
    }

    /**
     * @param colors
     *            the colors to set
     */
    public void setColors(RGB[] colors) {
        this.colors = colors;
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
     * @return the horizontalAlignment
     */
    public HorizontalAlignment getHorizontalAlignment() {
        return horizontalAlignment;
    }

    /**
     * @param horizontalAlignment
     *            the horizontalAlignment to set
     */
    public void setHorizontalAlignment(HorizontalAlignment horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }

    /**
     * @return the verticalAlignment
     */
    public VerticalAlignment getVerticalAlignment() {
        return verticalAlignment;
    }

    /**
     * @param verticalAlignment
     *            the verticalAlignment to set
     */
    public void setVerticalAlignment(VerticalAlignment verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }

    /**
     * @return the textStyle
     */
    public EnumSet<TextStyle> getTextStyles() {
        return textStyles;
    }

    /**
     * @param textStyle
     *            the textStyle to set
     */
    public void setTextStyle(EnumSet<TextStyle> textStyles) {
        this.textStyles = textStyles;
    }

    /**
     * @return the boxColor
     */
    public RGB getBoxColor() {
        return boxColor;
    }

    /**
     * @param boxColor
     *            the boxColor to set
     */
    public void setBoxColor(RGB boxColor) {
        this.boxColor = boxColor;
    }

    /**
     * @return the shadowColor
     */
    public RGB getShadowColor() {
        return shadowColor;
    }

    /**
     * @param shadowColor
     *            the shadowColor to set
     */
    public void setShadowColor(RGB shadowColor) {
        this.shadowColor = shadowColor;
    }

    /**
     * @return the magnification
     */
    public double getMagnification() {
        return magnification;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(double magnification) {
        this.magnification = magnification;
    }

    /**
     * @return the rotation
     */
    public double getRotation() {
        return rotation;
    }

    /**
     * @param rotation
     *            the rotation to set
     */
    public void setRotation(double rotation) {
        this.rotation = rotation;
    }

    /**
     * @return the point
     */
    public double[] getPoint() {
        return point;
    }

    /**
     * @param point
     *            the point to set
     */
    public void setPoint(double[] point) {
        this.point = point;
    }

    /**
     * @return the xOrColors
     */
    public boolean isxOrColors() {
        return xOrColors;
    }

    /**
     * @param xOrColors
     *            the xOrColors to set
     */
    public void setxOrColors(boolean xOrColors) {
        this.xOrColors = xOrColors;
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
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DrawStringEvent other = (DrawStringEvent) obj;
        if (Float.floatToIntBits(alpha) != Float.floatToIntBits(other.alpha))
            return false;
        if (boxColor == null) {
            if (other.boxColor != null)
                return false;
        } else if (!boxColor.equals(other.boxColor))
            return false;
        if (!Arrays.equals(colors, other.colors))
            return false;
        if (fontId != other.fontId)
            return false;
        if (horizontalAlignment != other.horizontalAlignment)
            return false;
        if (Double.doubleToLongBits(magnification) != Double
                .doubleToLongBits(other.magnification))
            return false;
        if (!Arrays.equals(point, other.point))
            return false;
        if (Double.doubleToLongBits(rotation) != Double
                .doubleToLongBits(other.rotation))
            return false;
        if (!Arrays.equals(text, other.text))
            return false;
        if (!textStyles.equals(other.textStyles))
            return false;
        if (verticalAlignment != other.verticalAlignment)
            return false;
        if (xOrColors != other.xOrColors)
            return false;
        return true;
    }

}
