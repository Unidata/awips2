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

package com.raytheon.viz.core.gl.internal;

import java.io.File;

import com.raytheon.uf.viz.core.drawables.AbstractAWTFont;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.viz.core.gl.IGLFont;
import com.sun.opengl.util.j2d.TextRenderer;

/**
 * 
 * Implements fonts for OpenGL
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2007             chammack    Initial Creation.
 * Jul 24, 2013       2189 mschenke    Refactored to share common awt font code
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GLFont extends AbstractAWTFont implements IGLFont {

    private boolean disposed = false;

    private float fontSize;

    private float currentFontSize;

    private TextRenderer textRenderer;

    private File fontFile;

    private FontType fontType;

    private float magnification = 1.0f;

    public GLFont(File font, FontType type, float fontSize, Style[] styles) {
        super(font, type, fontSize, styles);
        this.fontFile = font;
        this.fontType = type;
        this.currentFontSize = this.fontSize = fontSize;
        this.textRenderer = TextRendererCache.getRenderer(this.font);
    }

    public GLFont(String fontName, float fontSize, Style[] styles) {
        super(fontName, fontSize, styles);
        this.currentFontSize = this.fontSize = fontSize;
        this.textRenderer = TextRendererCache.getRenderer(this.font);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IFont#dispose()
     */
    public void dispose() {
        disposeInternal();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IFont#getFontSize()
     */
    public float getFontSize() {
        return currentFontSize;
    }

    public TextRenderer getTextRenderer() {
        return textRenderer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IFont#deriveWithSize(float)
     */
    @Override
    public IFont deriveWithSize(float size) {
        GLFont newFont = null;
        if (this.fontFile != null) {
            // File based construction
            newFont = new GLFont(this.fontFile, fontType, size, getStyle());
        } else {
            newFont = new GLFont(getFontName(), size, getStyle());
        }

        return newFont;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#setMagnification(float)
     */
    @Override
    public void setMagnification(float magnification) {
        setMagnification(magnification, true);
    }

    @Override
    public void setMagnification(float magnification, boolean scaleFont) {
        float newSize = currentFontSize;
        if (!scaleFont) {
            newSize = fontSize;
        } else if (this.magnification != magnification) {
            newSize = fontSize * magnification;
        } else if (fontSize == currentFontSize) {
            newSize = fontSize * magnification;
        }
        this.magnification = magnification;
        if (newSize != currentFontSize) {
            dispose();
            currentFontSize = newSize;
            font = font.deriveFont(newSize);
            this.textRenderer = TextRendererCache.getRenderer(this.font);
            disposed = false;
        }

    }

    @Override
    public float getMagnification() {
        if (fontSize != currentFontSize) {
            return 1.0f;
        }
        return magnification;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IGLFont#disposeInternal()
     */
    @Override
    public void disposeInternal() {
        if (!disposed) {
            if (this.textRenderer != null) {

                TextRendererCache.releaseRenderer(this.font);
                this.textRenderer = null;
                disposed = true;

            }
        }
    }

}
