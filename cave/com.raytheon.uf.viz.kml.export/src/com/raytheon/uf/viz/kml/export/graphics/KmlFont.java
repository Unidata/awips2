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
package com.raytheon.uf.viz.kml.export.graphics;

import java.awt.Font;
import java.awt.FontFormatException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.drawables.IFont;

/**
 * 
 * KML has really bad font support so only support the minimum operations
 * required to avoid errors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlFont implements IFont {

    private Font font;

    private float magnification;

    private boolean scaleFont;

    private boolean smoothing;

    public KmlFont(Font font) {
        this.font = font;
        this.magnification = 1.0f;
    }

    public KmlFont() {
        this(new Font(java.awt.Font.MONOSPACED, Font.BOLD, 14));
    }

    public KmlFont(String fontName) {
        this(new Font(fontName, Font.PLAIN, 10));

    }

    public KmlFont(String fontName, float fontSize) {
        this(new Font(fontName, Font.PLAIN, (int) fontSize));
    }

    public KmlFont(String fontName, float fontSize, Style[] styles) {
        this(new Font(fontName, toAwtStyle(styles), (int) fontSize));
    }

    public KmlFont(File fontFile, float fontSize, Style[] styles)
            throws FontFormatException, IOException {
        this(Font.createFont(Font.TRUETYPE_FONT, fontFile).deriveFont(fontSize)
                .deriveFont(toAwtStyle(styles)));
    }

    @Override
    public String getFontName() {
        return this.font.getFontName();
    }

    @Override
    public float getFontSize() {
        return this.font.getSize2D();
    }

    @Override
    public Style[] getStyle() {
        return toVizStyles(font.getStyle());
    }

    @Override
    public void dispose() {

    }

    @Override
    public IFont deriveWithSize(float size) {
        return new KmlFont(font.deriveFont(size));
    }

    @Override
    public void setMagnification(float magnification) {
        setMagnification(magnification, true);
    }

    @Override
    public void setMagnification(float magnification, boolean scaleFont) {
        if (scaleFont) {
            this.font = font.deriveFont(font.getSize2D() * magnification);
        } else {
            this.magnification = magnification;
        }
    }

    @Override
    public float getMagnification() {
        return magnification;
    }

    @Override
    public void setSmoothing(boolean smooth) {
        this.smoothing = smooth;
    }

    @Override
    public boolean getSmoothing() {
        return smoothing;
    }

    @Override
    public boolean isScaleFont() {
        return scaleFont;
    }

    @Override
    public void setScaleFont(boolean scaleFont) {
        this.scaleFont = scaleFont;
    }

    public Font getFont() {
        return font;
    }

    public void setFont(Font font) {
        this.font = font;
    }

    private static int toAwtStyle(Style[] styles) {
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

    private static Style[] toVizStyles(int style) {
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
