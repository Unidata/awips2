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
package com.raytheon.uf.viz.remote.graphics.objects;

import java.io.File;
import java.io.IOException;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.remote.graphics.Activator;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.fonts.CreateFontEvent;
import com.raytheon.uf.viz.remote.graphics.events.fonts.UpdateFontDataEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingFont extends DispatchingObject<IFont> implements IFont {

    private float lastMagnification;

    private boolean lastSmoothing;

    private boolean lastScaleFont;

    /**
     * @param wrappedObject
     * @param dispatcher
     */
    public DispatchingFont(IFont wrappedObject, Dispatcher dispatcher) {
        super(wrappedObject, dispatcher);
        this.lastMagnification = wrappedObject.getMagnification();
        this.lastSmoothing = wrappedObject.getSmoothing();
        this.lastScaleFont = wrappedObject.isScaleFont();
        sendCreateFontEvent(getFontName(), getFontSize(), getStyle(),
                getMagnification(), getSmoothing(), isScaleFont(), null);
    }

    public DispatchingFont(IFont wrappedObject, Dispatcher dispatcher,
            File fontFile) {
        super(wrappedObject, dispatcher);
        this.lastMagnification = wrappedObject.getMagnification();
        this.lastSmoothing = wrappedObject.getSmoothing();
        this.lastScaleFont = wrappedObject.isScaleFont();
        sendCreateFontEvent(getFontName(), getFontSize(), getStyle(),
                getMagnification(), getSmoothing(), isScaleFont(), fontFile);
    }

    private void sendCreateFontEvent(String fontName, float fontSize,
            Style[] fontStyle, float magnification, boolean smooth,
            boolean scaleFont, File fontData) {
        CreateFontEvent event = RemoteGraphicsEventFactory.createEvent(
                CreateFontEvent.class, this);
        event.setFontName(fontName);
        event.setFontSize(fontSize);
        event.setFontStyle(fontStyle);
        event.setMagnification(magnification);
        event.setSmoothing(smooth);
        event.setScaleFont(scaleFont);
        if (fontData != null && fontData.exists() && fontData.canRead()) {
            try {
                event.setFontData(FileUtil.file2bytes(fontData));
            } catch (IOException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
        dispatch(event);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#getFontName()
     */
    public String getFontName() {
        return wrappedObject.getFontName();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#getFontSize()
     */
    public float getFontSize() {
        return wrappedObject.getFontSize();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#getStyle()
     */
    public Style[] getStyle() {
        return wrappedObject.getStyle();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IFont#dispose()
     */
    public void dispose() {
        wrappedObject.dispose();
        dispatch(RemoteGraphicsEventFactory.createEvent(
                DisposeObjectEvent.class, this));
    }

    /**
     * @param size
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#deriveWithSize(float)
     */
    public IFont deriveWithSize(float size) {
        return new DispatchingFont(wrappedObject.deriveWithSize(size),
                getDispatcher());
    }

    /**
     * @param magnification
     * @see com.raytheon.uf.viz.core.drawables.IFont#setMagnification(float)
     */
    public void setMagnification(float magnification) {
        wrappedObject.setMagnification(magnification);
        if (lastMagnification != magnification) {
            lastMagnification = magnification;
            UpdateFontDataEvent event = RemoteGraphicsEventFactory.createEvent(
                    UpdateFontDataEvent.class, this);
            event.setMagnification(magnification);
            dispatch(event);
        }
    }

    /**
     * @param magnification
     * @param scaleFont
     * @see com.raytheon.uf.viz.core.drawables.IFont#setMagnification(float,
     *      boolean)
     */
    public void setMagnification(float magnification, boolean scaleFont) {
        wrappedObject.setMagnification(magnification, scaleFont);
        if (lastMagnification != magnification) {
            lastMagnification = magnification;
            UpdateFontDataEvent event = RemoteGraphicsEventFactory.createEvent(
                    UpdateFontDataEvent.class, this);
            event.setScaleFont(scaleFont);
            event.setMagnification(magnification);
            dispatch(event);
        }
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#getMagnification()
     */
    public float getMagnification() {
        return wrappedObject.getMagnification();
    }

    /**
     * @param smooth
     * @see com.raytheon.uf.viz.core.drawables.IFont#setSmoothing(boolean)
     */
    public void setSmoothing(boolean smooth) {
        wrappedObject.setSmoothing(smooth);
        if (lastSmoothing != smooth) {
            lastSmoothing = smooth;
            UpdateFontDataEvent event = RemoteGraphicsEventFactory.createEvent(
                    UpdateFontDataEvent.class, this);
            event.setSmoothing(smooth);
            dispatch(event);
        }
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#getSmoothing()
     */
    public boolean getSmoothing() {
        return wrappedObject.getSmoothing();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IFont#isScaleFont()
     */
    public boolean isScaleFont() {
        return wrappedObject.isScaleFont();
    }

    /**
     * @param scaleFont
     * @see com.raytheon.uf.viz.core.drawables.IFont#setScaleFont(boolean)
     */
    public void setScaleFont(boolean scaleFont) {
        wrappedObject.setScaleFont(scaleFont);
        if (lastScaleFont != scaleFont) {
            lastScaleFont = scaleFont;
            UpdateFontDataEvent event = RemoteGraphicsEventFactory.createEvent(
                    UpdateFontDataEvent.class, this);
            event.setScaleFont(scaleFont);
            dispatch(event);
        }
    }

}
