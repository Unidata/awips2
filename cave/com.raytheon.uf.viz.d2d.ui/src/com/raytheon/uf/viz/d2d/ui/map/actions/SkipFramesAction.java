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
package com.raytheon.uf.viz.d2d.ui.map.actions;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SkipFramesAction extends AbstractRightClickAction {
    public static enum SkipFrameMode {
        THIS_FRAME("Skip This Frame"), PREVIOUS_FRAMES("Skip Previous Frames"), SUBSEQ_FRAMES(
                "Skip Subsequent Frames"), RESTORE("Restore Skipped Frames");

        private String name;

        private SkipFrameMode(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return this.name;
        }
    }

    private SkipFrameMode mode;

    public SkipFramesAction(SkipFrameMode mode) {
        this.mode = mode;
    }

    @Override
    public void run() {
        IDescriptor descriptor = null;
        for (IDisplayPane pane : this.container.getDisplayPanes()) {
            descriptor = pane.getDescriptor();
            FramesInfo info = descriptor.getFramesInfo();
            DataTime[] times = info.getFrameTimes();
            int currIndex = info.getFrameIndex();
            int frameCount = info.getFrameCount();
            switch (this.mode) {
            case THIS_FRAME: {
                times[currIndex].setVisible(false);
                descriptor.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.NEXT,
                        FrameChangeMode.TIME_AND_SPACE);
                break;
            }
            case PREVIOUS_FRAMES: {
                int i = currIndex - 1;
                for (; i >= 0; --i) {
                    times[i].setVisible(false);
                }
                break;
            }
            case SUBSEQ_FRAMES: {
                int i = currIndex + 1;
                for (; i < frameCount; ++i) {
                    times[i].setVisible(false);
                }
                break;
            }
            case RESTORE: {
                for (int i = 0; i < frameCount; ++i) {
                    times[i].setVisible(true);
                }
                break;
            }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return mode.toString();
    }
}
