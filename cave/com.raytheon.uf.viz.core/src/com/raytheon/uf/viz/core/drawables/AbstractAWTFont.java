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

import java.awt.Font;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Simple abstract base class for an AWT-based IFont implementations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2013       2189 mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractAWTFont implements IFont {

    protected Font font;

    protected boolean scaleFont;

    protected boolean smoothing;

    protected AbstractAWTFont(String fontName, float fontSize, Style[] styles) {
        this(new Font(fontName, toAwtStyle(styles), (int) fontSize));
    }

    protected AbstractAWTFont(File fontFile, FontType fontType, float fontSize,
            Style[] styles) {
        this(createFont(fontFile, fontType, fontSize, styles));
    }

    protected AbstractAWTFont(Font font) {
        this.font = font;
    }

    @Override
    public final String getFontName() {
        return font.getName();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#getStyle()
     */
    @Override
    public final Style[] getStyle() {
        return toVizStyles(font.getStyle());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#setSmoothing(boolean)
     */
    @Override
    public final void setSmoothing(boolean smooth) {
        this.smoothing = smooth;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#getSmoothing()
     */
    @Override
    public final boolean getSmoothing() {
        return smoothing;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#isScaleFont()
     */
    @Override
    public final boolean isScaleFont() {
        return scaleFont;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#setScaleFont(boolean)
     */
    @Override
    public final void setScaleFont(boolean scaleFont) {
        this.scaleFont = scaleFont;
    }

    protected static Font createFont(File fontFile, FontType fontType,
            float fontSize, Style[] styles) {
        try {
            return Font.createFont(toAwtFontType(fontType), fontFile)
                    .deriveFont(fontSize).deriveFont(toAwtStyle(styles));
        } catch (Exception e) {
            throw new IllegalArgumentException("Bad Font File", e);
        }
    }

    protected static int toAwtFontType(FontType type) {
        switch (type) {
        case TYPE1:
            return Font.TYPE1_FONT;
        case TRUETYPE:
        default:
            return Font.TRUETYPE_FONT;
        }
    }

    protected static int toAwtStyle(Style[] styles) {
        int styleInt = Font.PLAIN;
        if (styles == null || styles.length == 0) {
            return styleInt;
        }
        for (Style style : styles) {
            if (style == Style.BOLD) {
                styleInt |= Font.BOLD;
            } else if (style == Style.ITALIC) {
                styleInt |= Font.ITALIC;
            }
        }
        return styleInt;
    }

    protected static Style[] toVizStyles(int style) {
        List<Style> styles = new ArrayList<Style>();
        if ((style & Font.BOLD) != 0) {
            styles.add(Style.BOLD);
        }
        if ((style & Font.ITALIC) != 0) {
            styles.add(Style.ITALIC);
        }
        return styles.toArray(new Style[0]);
    }
}
