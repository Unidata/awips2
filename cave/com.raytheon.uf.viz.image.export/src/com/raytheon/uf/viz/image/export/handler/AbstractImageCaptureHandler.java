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
package com.raytheon.uf.viz.image.export.handler;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * 
 * Abstract handler that provides the ability to grab frame(s) as buffered
 * iamges from the active editor. Subclasses are expected to implement
 * {@link #execute(org.eclipse.core.commands.ExecutionEvent)} and use the
 * capture...Frames methods to implement export capability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 20, 2014  2312     bsteffen    Move to image export plugin.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractImageCaptureHandler extends AbstractHandler {

    protected BufferedImage captureCurrentFrames(AbstractEditor editor) {
        return editor.screenshot();
    }

    protected List<BufferedImage> captureAllFrames(AbstractEditor editor)
            throws VizException {
        int startIndex = 0;
        int endIndex = editor.getActiveDisplayPane().getDescriptor()
                .getFramesInfo().getFrameCount();
        if (endIndex == 0) {
            endIndex = 1;
        }
        return captureFrames(editor, startIndex, endIndex);
    }

    protected List<BufferedImage> captureFrames(AbstractEditor editor,
            int startIndex, int endIndex) throws VizException {
        if (startIndex < 0) {
            startIndex = 0;
        }
        List<BufferedImage> images = new ArrayList<BufferedImage>(endIndex
                - startIndex);
        int origIndex = editor.getActiveDisplayPane().getDescriptor()
                .getFramesInfo().getFrameIndex();
        for (int i = startIndex; i < endIndex; i++) {
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                setFrameIndex(pane.getDescriptor(), i);
                pane.refresh();
                renderPane(pane, editor.getLoopProperties());
            }
            images.add(editor.screenshot());
        }
        for (IDisplayPane pane : editor.getDisplayPanes()) {
            setFrameIndex(pane.getDescriptor(), origIndex);
            pane.refresh();
        }
        return images;
    }

    private void setFrameIndex(IDescriptor desc, int index) {
        FramesInfo fi = desc.getFramesInfo();
        if (fi.getFrameTimes() == null || fi.getFrameCount() <= 1) {
            return;
        }
        index = index % fi.getFrameCount();
        if (index < 0) {
            index += fi.getFrameCount();
        }
        fi = new FramesInfo(fi.getFrameTimes(), index, fi.getTimeMap());
        desc.setFramesInfo(fi);
    }

    protected void renderPane(IDisplayPane pane, LoopProperties loopProperties)
            throws VizException {
        IGraphicsTarget target = pane.getTarget();
        IRenderableDisplay display = pane.getRenderableDisplay();

        float alpha = 1.0f;
        float zoomLevel = (float) pane.getZoomLevel();
        IView view = display.getView();
        Rectangle canvasBounds = pane.getBounds();
        boolean isZooming = false;
        FramesInfo framesInfo = pane.getDescriptor().getFramesInfo();
        PaintProperties paintProps = new PaintProperties(alpha, zoomLevel,
                view, canvasBounds, isZooming, framesInfo);
        paintProps.setLoopProperties(loopProperties);

        // paint in a loop until any async tasks are done
        while (target.isNeedsRefresh()) {
            target.beginFrame(display.getView(), true);
            display.paint(target, paintProps);
            target.endFrame();
        }
    }

    @Override
    public void setEnabled(Object obj) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container == null) {
            setBaseEnabled(false);
        } else {
            setBaseEnabled(true);
        }
    }

}
