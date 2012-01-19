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

package com.raytheon.viz.drawing;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Draw a weather symbol on the map
 * 
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 *   Date         Ticket#     Engineer    Description
 *   ------------ ----------  ----------- --------------------------
 *   Oct 26, 2006         66  chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class WeatherSymbolTool extends AbstractDrawingTool {

    protected int symbolID;

    protected IInputHandler mouseHandler;

    @Override
    protected void activateTool() {
        super.activateTool();
        String id = this.commandId;
        int idx = id.indexOf(":");
        symbolID = Integer.parseInt(id.substring(idx + 1));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.drawing.AbstractDrawingTool#getMouseHandler()
     */
    @Override
    public IInputHandler getMouseHandler() {

        if (mouseHandler == null) {
            mouseHandler = new SymbolDrawingHandler();
        }
        return mouseHandler;
    }

    public class SymbolDrawingHandler extends InputAdapter {

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        public boolean handleMouseUp(int x, int y, int button) {
            if (button != 1)
                return false;
            Coordinate c = editor.translateClick(x, y);
            if (c == null)
                return true;

            theDrawingLayer.addSymbol(c, symbolID, true, null);
            editor.refresh();
            return true;
        }

    }

}
