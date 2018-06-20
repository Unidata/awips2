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
package com.raytheon.viz.gfe.temporaleditor.mousehandler;

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.temporaleditor.AbstractTemporalEditorBar;

/**
 * MouseHandler to resize temporal editor bars.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * May 28, 2009 #2159      Richard Peter Initial Creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class LabelMouseHandler extends MouseHandler {
    // TOP bar, drag up expands, drag down shrinks
    public static final int TOP_ORIENTATION = 0;

    // BOTTOM bar, drag up shrinks, drag down expands
    public static final int BOTTOM_ORIENTATION = 1;

    int lastHeightProcessed;

    private int orientation = 0;

    private AbstractTemporalEditorBar teBar;

    public LabelMouseHandler(AbstractTemporalEditorBar teBar, int orientation) {
        this.teBar = teBar;
        this.orientation = orientation;
    }

    @Override
    public void dragStart(MouseEvent e) {
        super.dragStart(e);
        lastHeightProcessed = getDragAnchor().y;
    }

    @Override
    public void dragMove(MouseEvent e) {
        super.dragMove(e);
        int delta;

        if (orientation == TOP_ORIENTATION) {
            delta = lastHeightProcessed - e.y;
        } else {
            delta = e.y - getDragAnchor().y;
        }

        Composite container = teBar.getContainer();
        GridData gridData = (GridData) container.getLayoutData();
        int height = gridData.heightHint;

        // if delta negative
        if (height + delta > 0) {
            height += delta;
        } else {
            height = 0;
        }

        gridData.heightHint = height;
        container.setLayoutData(gridData);
        container.getParent().layout();
        lastHeightProcessed = e.y;
    }

    @Override
    public void dragEnd(MouseEvent e) {
        super.dragEnd(e);
        teBar.getTemporalEditor().resize();
    }
}