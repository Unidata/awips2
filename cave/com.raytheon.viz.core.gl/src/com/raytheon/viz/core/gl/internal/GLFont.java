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

import java.awt.Font;
import java.io.File;

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
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GLFont implements IGLFont {

    private boolean disposed = false;

    private String fontName;

    private float fontSize;

    private float currentFontSize;

    private Style[] styles;

    private Font font;

    private TextRenderer textRenderer;

    private boolean smoothing = true;

    private File fontFile;

    private float magnification = 1.0f;

    private boolean scaleFont = true;

    public GLFont() {
        ;
    }

    public GLFont(File font, float fontSize, Style[] styles) {
        try {
            this.fontName = font.getName();
            this.font = Font.createFont(Font.TRUETYPE_FONT, font).deriveFont(
                    fontSize);
            this.currentFontSize = this.fontSize = fontSize;
            this.styles = styles;

            if (styles != null && styles.length > 0) {
                for (Style style : styles) {
                    if (style == Style.BOLD) {
                        this.font = this.font.deriveFont(Font.BOLD);
                    } else if (style == Style.ITALIC) {
                        this.font = this.font.deriveFont(Font.ITALIC);
                    }
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
            throw new IllegalArgumentException("Bad font file", e);
        }
        this.textRenderer = TextRendererCache.getRenderer(this.font);
    }

    public GLFont(String fontName, float fontSize, Style[] styles) {
        this.fontName = fontName;
        this.currentFontSize = this.fontSize = fontSize;
        this.styles = styles;

        int style = Font.PLAIN;

        if (styles != null) {
            for (Style s : styles) {
                if (s == IFont.Style.BOLD) {
                    style = (Font.BOLD | style);
                } else if (s == IFont.Style.ITALIC) {
                    style = (Font.ITALIC | style);
                }

            }
        }

        this.font = new Font(this.fontName, style, (int) fontSize);
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
     * @see com.raytheon.viz.core.drawables.IFont#getFontName()
     */
    public String getFontName() {
        return this.fontName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IFont#getFontSize()
     */
    public float getFontSize() {
        return currentFontSize;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IFont#getStyle()
     */
    public Style[] getStyle() {
        return this.styles;
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
            newFont = new GLFont(this.fontFile, size, styles);
        } else {
            newFont = new GLFont(this.fontName, size, styles);
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
     * @see com.raytheon.uf.viz.core.drawables.IFont#getSmoothing()
     */
    @Override
    public boolean getSmoothing() {
        return smoothing;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#setSmoothing(boolean)
     */
    @Override
    public void setSmoothing(boolean smoothing) {
        this.smoothing = smoothing;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#isScaleFont()
     */
    @Override
    public boolean isScaleFont() {
        return scaleFont;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#setScaleFont(boolean)
     */
    @Override
    public void setScaleFont(boolean scaleFont) {
        this.scaleFont = scaleFont;
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
