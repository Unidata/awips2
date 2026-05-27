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

package com.raytheon.uf.viz.d2d.nsharp.rsc.action;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

/**
 * 
 * Execute NSHARP from Tools in D2D
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date           Ticket#  Engineer  Description
 * -------------  -------- --------- ----------------------------
 * Apr 03,2020    73172    smanoj    Initial creation
 * 
 * </pre>
 * 
 * @author smanoj
 */

public class D2DNsharpViewAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException { 
        // The viewID string is in the XML file for NSHARP extension point.
        String viewid = "com.raytheon.uf.viz.d2d.nsharp.display.D2DNSharpToolPaletteWindow";

        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(viewid);

        try {
            if (vpart == null) {
                vpart = wpage.showView(viewid);
            } else {
                if (!wpage.isPartVisible(vpart))
                    vpart = wpage.showView(viewid);
            }
        } catch (Exception e) {
            throw new ExecutionException(
                    "Error occurred while viewing NSHARP Palette Window. ", e);
        }

        return null;
    }

}