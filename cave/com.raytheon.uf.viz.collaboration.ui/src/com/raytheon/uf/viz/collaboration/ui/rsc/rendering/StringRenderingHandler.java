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
package com.raytheon.uf.viz.collaboration.ui.rsc.rendering;

import java.io.File;
import java.io.IOException;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.fonts.CreateFontEvent;
import com.raytheon.uf.viz.remote.graphics.events.fonts.UpdateFontDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.strings.DrawStringEvent;
import com.raytheon.uf.viz.remote.graphics.events.strings.DrawStringsEvent;

/**
 * Rendering handler for strings and fonts
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class StringRenderingHandler extends CollaborationRenderingHandler {

    @Subscribe
    public void drawStrings(DrawStringsEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        DrawStringEvent[] events = event.getObjects();
        DrawableString[] strings = new DrawableString[events.length];
        for (int i = 0; i < events.length; ++i) {
            strings[i] = events[i].getDrawableString();
            if (events[i].getFontId() > -1) {
                strings[i].font = dataManager.getRenderableObject(
                        events[i].getFontId(), IFont.class);
            }
        }
        try {
            target.drawStrings(strings);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void createFont(CreateFontEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        int fontId = event.getObjectId();
        IFont font = null;
        if (event.getFontData() != null) {
            try {
                File fontFile = File.createTempFile(event.getFontName(), null);
                FileUtil.bytes2File(event.getFontData(), fontFile);
                font = target.initializeFont(fontFile, event.getFontSize(),
                        event.getFontStyle());
            } catch (IOException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        "Error creating font from file", e);
            }
        } else {
            font = target.initializeFont(event.getFontName(),
                    event.getFontSize(), event.getFontStyle());
        }
        font.setMagnification(event.getMagnification());
        font.setSmoothing(event.isSmoothing());
        font.setScaleFont(event.isScaleFont());
        dataManager.putRenderableObject(fontId, font);
    }

    @Subscribe
    public void updateFont(UpdateFontDataEvent event) {
        IFont font = dataManager.getRenderableObject(event.getObjectId(),
                IFont.class);
        if (font != null) {
            if (event.getScaleOnMagnify() != null) {
                font.setMagnification(event.getMagnification(),
                        event.getScaleOnMagnify());
            } else {
                font.setMagnification(event.getMagnification());
            }
            font.setSmoothing(event.getSmoothing());
            font.setScaleFont(event.getScaleFont());
        }
    }

    @Subscribe
    public void disposeFont(IFont font) {
        font.dispose();
    }

}
