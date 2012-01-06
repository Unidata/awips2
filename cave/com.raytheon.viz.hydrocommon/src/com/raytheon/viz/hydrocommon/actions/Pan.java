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
import org.eclipse.swt.graphics.Rectangle;

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

public class Pan extends AbstractTool {

    /** The percentage of the screen to pan */
    private static final double PAN_DELTA = 0.25;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        String dir = arg0.getParameter("direction");

        IDisplayPane[] panes = editor.getDisplayPanes();
        for (IDisplayPane p : panes) {
            Rectangle bounds = p.getBounds();
            double[] start = new double[] { bounds.x, bounds.y };
            double[] end = new double[] { bounds.x, bounds.y };

            if (dir.contains("U")) {
                end[1] -= bounds.height * PAN_DELTA;
            }

            if (dir.contains("D")) {
                end[1] += bounds.height * PAN_DELTA;
            }

            if (dir.contains("R")) {
                end[0] += bounds.width * PAN_DELTA;
            }

            if (dir.contains("L")) {
                end[0] -= bounds.width * PAN_DELTA;
            }
            p.shiftExtent(start, end);
        }
        return null;
    }
}
