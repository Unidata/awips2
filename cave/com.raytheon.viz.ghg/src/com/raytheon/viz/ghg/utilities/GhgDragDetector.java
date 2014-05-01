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
/**
 * 
 */
package com.raytheon.viz.ghg.utilities;

import com.raytheon.viz.ui.input.InputAdapter;

/**
 * A class created to capture MB1 drags for GHGLauncher before PanHandler eats
 * the event. The basic idea is that this input handler is put into the chain
 * where it will receive mouseDownMove events before PanHandler. It sets an
 * internal flag to remember the drag. When
 * GhgLauncher.theMouseListener.handleMouseUp() is called, it checks and clears
 * the flag and can skip its "click" behavior if a the action was a drag. Note
 * that since this object will receive mouseUp events before
 * GhgLauncher.theMouseListener, it cannot clear its own wasDrag flag.
 * 
 * @author wdougherty
 * 
 */
public class GhgDragDetector extends InputAdapter {
    private boolean drag;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        if (mouseButton == 1) {
            setDrag(true);
        }
        return false;
    }

    /**
     * @return the wasDrag
     */
    public boolean isDrag() {
        return drag;
    }

    /**
     * @param drag
     *            the drag to set
     */
    public void setDrag(boolean drag) {
        this.drag = drag;
    }

}
