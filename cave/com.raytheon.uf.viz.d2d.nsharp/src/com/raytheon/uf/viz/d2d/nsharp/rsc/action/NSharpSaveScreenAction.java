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

package com.raytheon.uf.viz.d2d.nsharp.rsc.action;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.image.export.handler.ExportImageHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Save the current screen to a file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 26, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class NSharpSaveScreenAction extends ExportImageHandler {

    @Override
    protected BufferedImage captureCurrentFrames(AbstractEditor editor) {
        return editor.getActiveDisplayPane().getTarget().screenshot();
    }

    @Override
    protected List<BufferedImage> captureAllFrames(AbstractEditor editor)
            throws VizException {
        if(!(editor instanceof NsharpEditor)){
            return super.captureAllFrames(editor);
        }
        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();
        int startIndex = 0;
        int endIndex = handler .getFrameCount(); // Chin NsharpFrameIndexUtil.getFrameCount(handler);
        return captureFrames(editor, startIndex, endIndex);
    }

    @Override
    protected List<BufferedImage> captureFrames(AbstractEditor editor,
            int startIndex, int endIndex) throws VizException {
        if(!(editor instanceof NsharpEditor)){
            return super.captureFrames(editor, startIndex, endIndex);
        }
        IDisplayPane pane = editor.getActiveDisplayPane();
        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();
        // save the frame we are on;
        int startingIndex = handler.getCurrentIndex(); // Chin NsharpFrameIndexUtil.getCurrentIndex(handler);
        List<BufferedImage> images = new ArrayList<BufferedImage>();
        LoopProperties loopProperties = ((AbstractEditor) editor)
                .getLoopProperties();
        renderPane(pane, loopProperties);
        for (int i = startIndex; i < endIndex; i++) {
        	//Chin NsharpFrameIndexUtil.setCurrentIndex(handler, i);
        	if(handler.setCurrentIndex(i)== false)
        		continue;
            pane.refresh();
            renderPane(pane, loopProperties);
            images.add(captureCurrentFrames(editor));
        }
        handler.setCurrentIndex(startingIndex); // Chin NsharpFrameIndexUtil.setCurrentIndex(handler, startingIndex);
        pane.refresh();
        renderPane(pane, loopProperties);
        return images;
    }

}
