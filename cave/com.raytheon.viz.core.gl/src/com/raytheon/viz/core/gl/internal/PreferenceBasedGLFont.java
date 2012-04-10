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

import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.viz.core.gl.IGLFont;
import com.sun.opengl.util.j2d.TextRenderer;

/**
 * GL font that is set by preferences page, listens for updates
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PreferenceBasedGLFont implements IGLFont, IPropertyChangeListener {

    private IGLFont preferenceFont;

    private FontRegistry registry;

    private String propertyName;

    public PreferenceBasedGLFont(IGLFont font, String propName,
            FontRegistry registry) {
        this.preferenceFont = font;
        this.propertyName = propName;
        this.registry = registry;
        registry.addListener(this);
    }

    public int hashCode() {
        return preferenceFont.hashCode();
    }

    public void dispose() {
        preferenceFont.dispose();
    }

    public boolean equals(Object obj) {
        return preferenceFont.equals(obj);
    }

    public String getFontName() {
        return preferenceFont.getFontName();
    }

    public float getFontSize() {
        return preferenceFont.getFontSize();
    }

    public Style[] getStyle() {
        return preferenceFont.getStyle();
    }

    public TextRenderer getTextRenderer() {
        return preferenceFont.getTextRenderer();
    }

    public IFont deriveWithSize(float size) {
        return preferenceFont.deriveWithSize(size);
    }

    public void setMagnification(float magnification) {
        preferenceFont.setMagnification(magnification);
    }

    public void setMagnification(float magnification, boolean scaleFont) {
        preferenceFont.setMagnification(magnification, scaleFont);
    }

    public float getMagnification() {
        return preferenceFont.getMagnification();
    }

    public boolean getSmoothing() {
        return preferenceFont.getSmoothing();
    }

    public void setSmoothing(boolean smoothing) {
        preferenceFont.setSmoothing(smoothing);
    }

    public boolean isScaleFont() {
        return preferenceFont.isScaleFont();
    }

    public void setScaleFont(boolean scaleFont) {
        preferenceFont.setScaleFont(scaleFont);
    }

    public String toString() {
        return preferenceFont.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (propertyName.equals(event.getProperty())) {
            preferenceFont.disposeInternal();
            preferenceFont = FontFactory.getInstance().getFont(propertyName);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IGLFont#disposeInternal()
     */
    @Override
    public void disposeInternal() {
        preferenceFont.disposeInternal();
        registry.removeListener(this);
    }

}
