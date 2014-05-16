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
import java.util.Map;
import java.util.Map.Entry;

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
 * mAY 16, 2014  3163     bsteffen    Add support for reading colored text styles.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

@DynamicSerialize
public class DrawStringEvent extends AbstractRemoteGraphicsRenderEvent {

    /**
     * Flipping this boolean breaks support for older clients. The plan is to
     * phase in the style colors over multiple releases to ensure that each
     * release is compatible with the previous release, however if there is a
     * use case requiring support sooner then it is safe to flip this flag
     * assuming all clients support it.
     */
    private static boolean SUPPORT_STYLE_COLORS = Boolean
            .getBoolean("collaboration.supportStringStyleColors");

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

    /**
     * For backwards compatibility this field should never be set, but for
     * forward compatibility it can be deserialized.
     * 
     * @see DrawStringEvent#SUPPORT_STYLE_COLORS
     */
    @DynamicSerializeElement
    private Map<TextStyle, RGB> textStyleColorMap;

    /**
     * @deprecated Once backwards compatibility is not needed switch to
     *             {@link #textStyleColorMap}
     * @see DrawStringEvent#SUPPORT_STYLE_COLORS
     */
    @DynamicSerializeElement
    @Deprecated
    private EnumSet<TextStyle> textStyles;

    /**
     * @deprecated Once backwards compatibility is not needed switch to
     *             {@link #textStyleColorMap}
     * @see DrawStringEvent#SUPPORT_STYLE_COLORS
     */
    @DynamicSerializeElement
    @Deprecated
    private RGB boxColor;


    /**
     * @deprecated Once backwards compatibility is not needed switch to
     *             {@link #textStyleColorMap}
     */
    @DynamicSerializeElement
    @Deprecated
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
        if (diffEvent.textStyleColorMap != null) {
            diffObject.textStyleColorMap = diffEvent.textStyleColorMap;
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
        if (diffObject.textStyleColorMap != null) {
            textStyleColorMap = diffObject.textStyleColorMap;
        }
    }

    public void setDrawableString(DrawableString string) {
        this.text = string.getText();
        this.colors = string.getColors();
        this.alpha = string.basics.alpha;

        this.xOrColors = string.basics.xOrColors;
        this.horizontalAlignment = string.horizontalAlignment;
        this.verticalAlignment = string.verticallAlignment;
        this.magnification = string.magnification;
        this.point = new double[] { string.basics.x, string.basics.y,
                string.basics.z };
        this.rotation = string.rotation;
        if (string.font instanceof DispatchingFont) {
            fontId = ((DispatchingFont) string.font).getObjectId();
        }
        Map<TextStyle, RGB> textStyleColorMap = string.getTextStyleColorMap();
        if (textStyleColorMap != null && !textStyleColorMap.isEmpty()) {
            if (SUPPORT_STYLE_COLORS) {
                this.textStyleColorMap = textStyleColorMap;
            } else {
                this.textStyles = EnumSet.copyOf(textStyleColorMap.keySet());
                if (textStyleColorMap.containsKey(TextStyle.BOXED)) {
                    this.boxColor = textStyleColorMap.get(TextStyle.BLANKED);
                }
                if (textStyleColorMap.containsKey(TextStyle.DROP_SHADOW)) {
                    this.shadowColor = textStyleColorMap
                            .get(TextStyle.DROP_SHADOW);
                }
            }
        }

    }

    public DrawableString getDrawableString() {
        DrawableString ds = new DrawableString(text, colors);
        ds.basics.alpha = alpha;
        ds.basics.xOrColors = xOrColors;
        
        ds.horizontalAlignment = horizontalAlignment;
        ds.verticallAlignment = verticalAlignment;
        ds.magnification = magnification;
        ds.setCoordinates(point[0], point[1], point[2]);
        ds.rotation = rotation;
        if (textStyleColorMap != null) {
            for (Entry<TextStyle, RGB> entry : textStyleColorMap.entrySet()) {
                ds.addTextStyle(entry.getKey(), entry.getValue());
            }
        }else{
            if(textStyles != null){
                for(TextStyle style : textStyles){
                    if (style.equals(TextStyle.BOXED)) {
                        ds.addTextStyle(style);
                        ds.addTextStyle(TextStyle.BLANKED, boxColor);
                    } else if (style.equals(TextStyle.DROP_SHADOW)) {
                        ds.addTextStyle(style, shadowColor);

                    } else {
                        ds.addTextStyle(style);
                    }
                }
            }
        }
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

    public Map<TextStyle, RGB> getTextStyleColorMap() {
        return textStyleColorMap;
    }

    public void setTextStyleColorMap(Map<TextStyle, RGB> textStyleColorMap) {
        this.textStyleColorMap = textStyleColorMap;
    }

    /**
     * @param textStyles
     *            the textStyles to set
     */
    public void setTextStyles(EnumSet<TextStyle> textStyles) {
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Float.floatToIntBits(alpha);
        result = prime * result
                + ((boxColor == null) ? 0 : boxColor.hashCode());
        result = prime * result + Arrays.hashCode(colors);
        result = prime * result + fontId;
        result = prime
                * result
                + ((horizontalAlignment == null) ? 0 : horizontalAlignment
                        .hashCode());
        long temp;
        temp = Double.doubleToLongBits(magnification);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + Arrays.hashCode(point);
        temp = Double.doubleToLongBits(rotation);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((shadowColor == null) ? 0 : shadowColor.hashCode());
        result = prime * result + Arrays.hashCode(text);
        result = prime
                * result
                + ((textStyleColorMap == null) ? 0 : textStyleColorMap
                        .hashCode());
        result = prime * result
                + ((textStyles == null) ? 0 : textStyles.hashCode());
        result = prime
                * result
                + ((verticalAlignment == null) ? 0 : verticalAlignment
                        .hashCode());
        result = prime * result + (xOrColors ? 1231 : 1237);
        return result;
    }

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
        if (shadowColor == null) {
            if (other.shadowColor != null)
                return false;
        } else if (!shadowColor.equals(other.shadowColor))
            return false;
        if (!Arrays.equals(text, other.text))
            return false;
        if (textStyleColorMap == null) {
            if (other.textStyleColorMap != null)
                return false;
        } else if (!textStyleColorMap.equals(other.textStyleColorMap))
            return false;
        if (textStyles == null) {
            if (other.textStyles != null)
                return false;
        } else if (!textStyles.equals(other.textStyles))
            return false;
        if (verticalAlignment != other.verticalAlignment)
            return false;
        if (xOrColors != other.xOrColors)
            return false;
        return true;
    }

}
