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
package com.raytheon.uf.viz.profiler.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.profiler.ui.ProfilerDisplay;
import com.raytheon.viz.ui.editor.EditorInput;

/**
 * This delegate gets the editor and sets up the upper air stations resource and
 * mouse handlers for data selection.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 16 Apr 2009             dhladky     Initial Coding
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class OpenProfilerEditor extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) {
        try {
            ProfilerDisplay renderableDisplay = new ProfilerDisplay();

            EditorInput cont = new EditorInput(renderableDisplay);
            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().openEditor(cont,
                            "com.raytheon.uf.viz.profiler.ui.ProfilerEditor", false);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return null;
    }
}

