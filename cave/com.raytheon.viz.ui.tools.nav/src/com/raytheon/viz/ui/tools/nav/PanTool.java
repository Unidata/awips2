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

package com.raytheon.viz.ui.tools.nav;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * Activate panning support in the editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 12/28/07                 chammack    Modified handler to be accessible 
 *                                      statically, using IDisplayContainer 
 *                                      rather than editor specific
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PanTool extends AbstractModalTool {

    /** The mouse handler */
    private PanHandler panHandler;

    @Override
    protected void activateTool() {
        if (panHandler == null) {
            panHandler = new PanHandler(this.editor);
        } else {
            panHandler.setContainer(editor);
        }
        editor.registerMouseHandler(panHandler, InputPriority.LOWEST);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
        if (editor != null)
            editor.unregisterMouseHandler(panHandler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#setEditor(com.raytheon.viz.ui.
     * editor.AbstractEditor)
     */
    @Override
    public void setEditor(IDisplayPaneContainer editor) {
        super.setEditor(editor);
        if (panHandler == null) {
            panHandler = new PanHandler(editor);
        }
        panHandler.setContainer(editor);
    }

}
