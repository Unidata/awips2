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

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Switch to a single panel display. The currently active display becomes the
 * large pane. the other panes are disposed.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Aug 19, 2009             bgonzale    Initial Creation.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1
 */
public class RotatePanelLayoutMenuAction extends AbstractRightClickAction {

    IDisplayPane paneWithFocus;

    /**
     * Default constructor.
     */
    public RotatePanelLayoutMenuAction() {
        super("Rotate Panels");
    }

    public void setPaneInFocus(IDisplayPane pane) {
        this.paneWithFocus = pane;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        RotatePanelsHandler.rotateToNextPane((IMultiPaneEditor) getContainer(),
                paneWithFocus);
    }

    @Override
    public boolean isHidden() {
        return container instanceof IMultiPaneEditor == false;
    }

}
