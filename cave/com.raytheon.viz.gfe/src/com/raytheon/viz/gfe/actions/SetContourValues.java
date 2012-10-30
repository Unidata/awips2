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
package com.raytheon.viz.gfe.actions;

import java.util.Set;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.EditorType;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisualizationType;
import com.raytheon.viz.gfe.dialogs.ContourDialog;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Right click action to bring up the Contour dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2009            randerso     Initial creation
 * Oct 30, 2012 1298       rferrel     Changes for non-blocking SetContourValues.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SetContourValues extends AbstractRightClickAction {
    private ContourDialog dialog;

    /**
     * Constructor
     */
    public SetContourValues() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dialog = new ContourDialog(shell,
                    ((GFEResource) getSelectedRsc()).getParm());
            dialog.open();
        } else {
            dialog.bringToTop();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Set Contour Values...";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.cmenu.AbstractRightClickAction#isHidden()
     */
    @Override
    public boolean isHidden() {
        Parm parm = ((GFEResource) getSelectedRsc()).getParm();
        VisMode visMode = parm.getDisplayAttributes().getVisMode();
        Set<VisualizationType> visTypes = parm.getDisplayAttributes()
                .getVisualizationType(EditorType.SPATIAL, visMode);

        return !visTypes.contains(VisualizationType.CONTOUR);
    }

}
