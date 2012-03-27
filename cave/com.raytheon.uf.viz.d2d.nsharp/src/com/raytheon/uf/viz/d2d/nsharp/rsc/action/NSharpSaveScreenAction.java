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

import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.actions.SaveScreenAction;
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
public class NSharpSaveScreenAction extends SaveScreenAction {

    @Override
    protected BufferedImage captureCurrentFrames(AbstractEditor editor) {
        return editor.getActiveDisplayPane().getTarget().screenshot();
    }

    @Override
    protected List<BufferedImage> captureAllFrames(AbstractEditor editor)
            throws VizException {
        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();
        if (!(desc instanceof NsharpSkewTDescriptor)) {
            return super.captureAllFrames(editor);
        }
        NsharpSkewTDescriptor ndesc = (NsharpSkewTDescriptor) desc;
        int startIndex = 0;
        int endIndex = ndesc.getSkewtResource().getDataTimelineList().size();
        return captureFrames(editor, startIndex, endIndex);
    }

    @Override
    protected List<BufferedImage> captureFrames(AbstractEditor editor,
            int startIndex, int endIndex) throws VizException {
        IDisplayPane pane = editor.getActiveDisplayPane();
        IDescriptor desc = pane.getDescriptor();
        if (!(desc instanceof NsharpSkewTDescriptor)) {
            return super.captureFrames(editor, startIndex, endIndex);
        }
        NsharpSkewTDescriptor ndesc = (NsharpSkewTDescriptor) desc;
        // save the frame we are on;
        String picked = ndesc.getSkewtResource().getPickedStnInfoStr();
        List<BufferedImage> images = new ArrayList<BufferedImage>();

        desc.getFrameCoordinator().changeFrame(FrameChangeOperation.FIRST,
                FrameChangeMode.TIME_AND_SPACE);
        pane.refresh();
        renderPane(pane, editor.getLoopProperties());
        for (int i = 0; i < endIndex; i++) {
            if (i >= startIndex) {
                images.add(captureCurrentFrames(editor));
            }
            if (i < endIndex - 1) {
                desc.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.NEXT,
                        FrameChangeMode.TIME_AND_SPACE);
                pane.refresh();
                renderPane(pane, editor.getLoopProperties());
            }
        }
        while (!ndesc.getSkewtResource().getPickedStnInfoStr().equals(picked)) {
            desc.getFrameCoordinator().changeFrame(FrameChangeOperation.NEXT,
                    FrameChangeMode.TIME_AND_SPACE);
            pane.refresh();
            renderPane(pane, editor.getLoopProperties());
        }
        return images;
    }

}
