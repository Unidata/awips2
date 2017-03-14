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

import com.raytheon.uf.viz.core.drawables.AbstractAWTFont;
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
 * Jun 1, 2012             bsteffen    Initial creation
 * Jul 24, 2013       2189 mschenke    Refactored to share common awt font code
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlFont extends AbstractAWTFont implements IFont {

    private float magnification;

    public KmlFont(Font font) {
        super(font);
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

    public KmlFont(File fontFile, FontType fontType, float fontSize,
            Style[] styles) throws FontFormatException, IOException {
        this(Font.createFont(toAwtFontType(fontType), fontFile)
                .deriveFont(fontSize).deriveFont(toAwtStyle(styles)));
    }

    @Override
    public float getFontSize() {
        return this.font.getSize2D();
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

    public Font getFont() {
        return font;
    }

    public void setFont(Font font) {
        this.font = font;
    }

}
