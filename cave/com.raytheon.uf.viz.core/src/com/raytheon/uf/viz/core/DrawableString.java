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
package com.raytheon.uf.viz.core;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;

/**
 * Object used to store information for drawing a string to an IGraphicsTarget.
 * RGB in basics object is ignored
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawableString extends AbstractDrawableObject {

    /** The font to draw in */
    public IFont font;

    /** The lines of text to draw */
    private String[] text;

    /** The left/right alignment of the text */
    public HorizontalAlignment horizontalAlignment = HorizontalAlignment.LEFT;

    /** The up/down alignment of the text */
    public VerticalAlignment verticallAlignment = VerticalAlignment.BOTTOM;

    /**
     * Magnification of the text, text will be scaled (zoomed). For smooth
     * magnification, set it on the IFont which then should internally change
     * the font size
     */
    public double magnification = 1.0f;

    /** amount of rotation in degrees from right */
    public double rotation = 0.0;

    /** The colors to use for the strings */
    private RGB[] colors;

    /** The text style to use when drawing */
    public TextStyle textStyle = TextStyle.NORMAL;

    /** The color of the shadow created when using TextStyle.DROP_SHADOW */
    public RGB shadowColor = new RGB(0, 0, 0);

    /**
     * The color of the background when using TextStyle.BOX. If null, normal
     * target background color should be used.
     */
    public RGB boxColor;

    /**
     * Construct parameters with text, splits by newline, all text will be drawn
     * with color "color"
     * 
     * @param text
     */
    public DrawableString(String text, RGB color) {
        setText(text, color);
    }

    /**
     * Construct parameters with lines of text, all text will be drawn with
     * color "color"
     * 
     * @param text
     */
    public DrawableString(String[] text, RGB color) {
        setText(text, color);
    }

    /**
     * Construct parameters with lines of text and color for each line
     * 
     * @param text
     * @param colors
     */
    public DrawableString(String[] text, RGB[] colors) {
        setText(text, colors);
    }

    /**
     * Set the text to be drawn, splits by newline
     * 
     * @param text
     */
    public void setText(String text, RGB color) {
        setText(text.split("[\n]"), color);
    }

    /**
     * Set the lines of text to be drawn
     * 
     * @param text
     * @param color
     *            color of text to be drawn
     */
    public void setText(String[] text, RGB color) {
        RGB[] colors = new RGB[text.length];
        for (int i = 0; i < text.length; ++i) {
            colors[i] = color;
        }
        setText(text, colors);
    }

    /**
     * Set the lines of text to be drawn
     * 
     * @param text
     * @param colors
     *            colors of text strings to be drawn
     */
    public void setText(String[] text, RGB[] colors) {
        this.text = text;
        this.colors = colors;
    }

    /**
     * Get the lines of text to draw
     * 
     * @return the lines of text
     */
    public String[] getText() {
        return text;
    }

    /**
     * Get the colors of the text to draw
     * 
     * @return the colors of the lines of text
     */
    public RGB[] getColors() {
        return colors;
    }

}
