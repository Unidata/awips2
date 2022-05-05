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

import java.awt.image.BufferedImage;
import java.util.LinkedHashMap;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.image.export.handler.ExportImageHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

/**
 * Save the current screen to a file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Unknown                bsteffen  Initial creation
 * Unknown                cchen     Modifications
 * Dec 08, 2014  16713    jgerth    Incorporate date and time
 * Jun 13, 2018  7318     bsteffen  Fix frame indexing.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class NSharpSaveScreenAction extends ExportImageHandler {

    @Override
    protected LinkedHashMap<DataTime, BufferedImage> captureCurrentFrames(
            AbstractEditor editor) {
        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();
        LinkedHashMap<DataTime, BufferedImage> dtbiHash = new LinkedHashMap<>();
        DataTime[] dataTimes = handler.getSkewtPaneRsc().getDescriptor()
                .getFramesInfo().getFrameTimes();
        if (dataTimes == null || dataTimes.length == 0) {
            dtbiHash.put(buildFakeTime(0),
                    editor.getActiveDisplayPane().getTarget().screenshot());
        } else {
            dtbiHash.put(dataTimes[handler.getCurrentTimeElementListIndex()],
                    editor.getActiveDisplayPane().getTarget().screenshot());
        }
        return dtbiHash;
    }

    @Override
    protected LinkedHashMap<DataTime, BufferedImage> captureAllFrames(
            AbstractEditor editor) throws VizException {
        if (!(editor instanceof NsharpEditor)) {
            return super.captureAllFrames(editor);
        }
        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();
        int startIndex = 0;
        int endIndex = handler.getTimeElementListSize();
        return captureFrames(editor, startIndex, endIndex);
    }

    @Override
    protected LinkedHashMap<DataTime, BufferedImage> captureFrames(
            AbstractEditor editor, int startIndex, int endIndex)
            throws VizException {
        if (!(editor instanceof NsharpEditor)) {
            return super.captureFrames(editor, startIndex, endIndex);
        }
        LinkedHashMap<DataTime, BufferedImage> dtbiHash = new LinkedHashMap<>();
        IDisplayPane pane = editor.getActiveDisplayPane();
        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();
        // save the frame we are on;
        int startingIndex = handler.getCurrentTimeElementListIndex();
        LoopProperties loopProperties = editor.getLoopProperties();
        renderPane(pane, loopProperties);
        DataTime[] dataTimes = handler.getSkewtPaneRsc().getDescriptor()
                .getFramesInfo().getFrameTimes();
        for (int i = startIndex; i < endIndex; i++) {
            setIndex(handler, i);
            pane.refresh();
            renderPane(pane, loopProperties);
            dtbiHash.put(dataTimes[i], pane.getTarget().screenshot());
        }
        setIndex(handler, startingIndex);
        pane.refresh();
        renderPane(pane, loopProperties);
        return dtbiHash;
    }

    /**
     * NSharp does not easily support directly moving to a specific time frame,
     * so this method will use frame changes to get to the specified index.
     * 
     * @param handler
     *            the nsharp handler
     * @param index
     *            the desired index
     */
    private void setIndex(NsharpResourceHandler handler, int index) {
        int current = handler.getCurrentTimeElementListIndex();
        while (index < current) {
            handler.setSteppingTimeLine(FrameChangeOperation.NEXT,
                    FrameChangeMode.TIME_ONLY);
            current -= 1;
        }
        while (index > current) {
            handler.setSteppingTimeLine(FrameChangeOperation.PREVIOUS,
                    FrameChangeMode.TIME_ONLY);
            current += 1;
        }
    }

}
