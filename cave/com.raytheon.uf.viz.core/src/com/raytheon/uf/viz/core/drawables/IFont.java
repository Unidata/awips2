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

package com.raytheon.uf.viz.core.drawables;

/**
 * 
 * Represents a font object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface IFont {

    /** Style options for fonts */
    public static enum Style {
        BOLD, ITALIC
    };

    /**
     * Return the name of the font
     * 
     * @return the font name
     */
    public abstract String getFontName();

    /**
     * Return the size of the font
     * 
     * @return
     */
    public abstract float getFontSize();

    /**
     * Return the style bits associated with a font
     * 
     * @return the style options (may be a zero length array)
     */
    public abstract Style[] getStyle();

    /**
     * Dispose the font
     */
    public abstract void dispose();

    public abstract IFont deriveWithSize(float size);

    /**
     * Set a magnification on the font
     * 
     * @param magnification
     */
    public abstract void setMagnification(float magnification);

    /**
     * Set a magnification on the font
     * 
     * @param magnification
     * @param scaleFont
     *            if true change the font size by the magnification factor, if
     *            false render the same font size but magnified.
     */
    public abstract void setMagnification(float magnification, boolean scaleFont);

    /**
     * Get the magnification for the font
     * 
     * @return magnification
     */
    public abstract float getMagnification();

    /**
     * Set smoothing enabled on the font
     * 
     * @param smooth
     */
    public abstract void setSmoothing(boolean smooth);

    /**
     * Get the smoothing flag for the font
     * 
     * @return
     */
    public abstract boolean getSmoothing();

    /**
     * Check to see if the font should be scaled
     * 
     * @return
     */
    public abstract boolean isScaleFont();

    /**
     * Set if the font should be scaled
     * 
     * @param scaleFont
     */
    public abstract void setScaleFont(boolean scaleFont);
}
