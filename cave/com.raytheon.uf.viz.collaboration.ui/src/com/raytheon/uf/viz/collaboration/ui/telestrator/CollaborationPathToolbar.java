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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.drawing.PathToolbar;
import com.raytheon.uf.viz.drawing.events.DrawingEventBus;
import com.raytheon.uf.viz.drawing.tools.PathDrawingTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationPathToolbar extends PathToolbar {

    /**
     * @param parentShell
     */
    protected CollaborationPathToolbar(Shell parentShell) {
        super(parentShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.PathToolbar#startTool()
     */
    @Override
    protected void startTool() {
        PathDrawingTool tool = new CollaborationPathDrawingTool();
        tool.activate();
    }

    public static PathToolbar getToolbar() {
        if (pathToolbar == null) {
            pathToolbar = new CollaborationPathToolbar(new Shell(
                    Display.getCurrent()));
            DrawingEventBus.register(PathToolbar.getToolbar());
        }
        return pathToolbar;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.PathToolbar#initializeComponents(org.eclipse
     * .swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // allows for subclasses to add more items to the toolbar, in this case
        // allowing the user to turn off other collaborator drawings
        super.initializeComponents(shell);
        // ToolItem allowOthersItem = new ToolItem(toolbar, SWT.CHECK);
        // allowOthersItem.setText("Leader Only");
        // allowOthersItem.setImage(IconUtil.getImageDescriptor(
        // Activator.getDefault().getBundle(), "multiple_draw.gif")
        // .createImage());
        // allowOthersItem.setSelection(true);
        // allowOthersItem.addSelectionListener(new SelectionAdapter() {
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        // AbstractEditor editor = EditorUtil
        // .getActiveEditorAs(AbstractEditor.class);
        // IDescriptor desc = editor.getActiveDisplayPane()
        // .getDescriptor();
        // for (ResourcePair pair : desc.getResourceList()) {
        // if (pair.getResource() instanceof CollaborationDrawingLayer) {
        // System.out.println("collaborating");
        // }
        // }
        // }
        // });
    }

}
