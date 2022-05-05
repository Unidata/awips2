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
package com.raytheon.uf.viz.auto.transition;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Action which opens the {@link AutoTransitionDialog}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 09, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ConfigureAutoTransitionAction extends AbstractRightClickAction {

    private AutoTransitionResourceData getResourceData() {
        if (selectedRsc.getResource() instanceof AutoTransitionResource) {
            return ((AutoTransitionResource) selectedRsc.getResource())
                    .getResourceData();
        }
        return null;
    }

    @Override
    public boolean isHidden() {
        return !getResourceData().isAutomaticSelection();
    }

    @Override
    public String getText() {
        return "Configure Automatic Transition";
    }

    @Override
    public void run() {
        AutoTransitionResource resource = (AutoTransitionResource) selectedRsc
                .getResource();
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        new AutoTransitionDialog(shell, resource).open();
    }

}
