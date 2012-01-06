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
package com.raytheon.viz.hydrocommon.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 20, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class Zoom extends AbstractTool {

    /** The zoom ratio */
    private static final double ZOOM_RATIO = 0.9;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        String dir = arg0.getParameter("direction");

        IDisplayPane[] panes = editor.getDisplayPanes();
        for (IDisplayPane p : panes) {
            double zoomLevel = 1.0;

            if (dir.equalsIgnoreCase("in")) {
                zoomLevel *= ZOOM_RATIO;
            } else if (dir.equalsIgnoreCase("out")) {
                zoomLevel /= ZOOM_RATIO;
            }

            p.getRenderableDisplay().zoom(zoomLevel);
            p.refresh();
        }
        return null;
    }
}
