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

import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * Activate zoom box support in the editor
 * 
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * Oct 21, 2008   #1450     randerso    Fixed to support multipane editors
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ZoomTool extends AbstractModalTool {

    /** The mouse handler */
    private ZoomHandler currentHandler;

    @Override
    protected void activateTool() {
        currentHandler = ZoomHandler.getInstance(editor);
        editor.registerMouseHandler(currentHandler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
        if (editor != null) {
            editor.unregisterMouseHandler(currentHandler);
        }
    }

}
