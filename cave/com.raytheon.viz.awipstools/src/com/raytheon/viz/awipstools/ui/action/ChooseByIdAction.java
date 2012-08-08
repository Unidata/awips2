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
package com.raytheon.viz.awipstools.ui.action;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.d2d.ui.map.actions.NewMapEditor;
import com.raytheon.viz.awipstools.ui.dialog.ChooseByIdDialog;
import com.raytheon.viz.awipstools.ui.layer.HomeToolLayer;
import com.raytheon.viz.awipstools.ui.layer.InteractiveBaselinesLayer;
import com.raytheon.viz.awipstools.ui.layer.PointsToolLayer;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.map.AbstractMapTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06Dec2007    #576        Eric Babin Initial Creation
 * 31Jul2012    #875       rferrel     Added checks for disposed dialgos.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ChooseByIdAction extends AbstractMapTool {

    private ChooseByIdDialog chooseByIdDialog;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        if (editor.getActiveDisplayPane().getDescriptor() instanceof MapDescriptor) {
            if (chooseByIdDialog == null || chooseByIdDialog.isDisposed()) {
                chooseByIdDialog = new ChooseByIdDialog(PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow().getShell());

                chooseByIdDialog.setHomeResource(getResource(
                        HomeToolLayer.class, HomeToolAction.class));
                chooseByIdDialog.setPointsResource(getResource(
                        PointsToolLayer.class, PointsToolAction.class));
                chooseByIdDialog.setBaslinesResource(getResource(
                        InteractiveBaselinesLayer.class,
                        BaselinesToolAction.class));
                chooseByIdDialog.setDescriptor(editor.getActiveDisplayPane()
                        .getDescriptor());

                chooseByIdDialog.open();
                chooseByIdDialog = null;
            } else {
                chooseByIdDialog.setDescriptor(editor.getActiveDisplayPane()
                        .getDescriptor());

                // find and activate the dialog
                for (Shell s : chooseByIdDialog.getParent().getShells()) {
                    if (s.getText().equals(ChooseByIdDialog.DIALOG_TITLE)) {
                        s.setVisible(true);
                        s.setActive();
                    }
                }
            }
        } else {
            // If a map editor is open, activate and use. Otherwise, create one.
            AbstractRenderableDisplay display = null;
            String editorId = DescriptorMap.getEditorId(MapDescriptor.class
                    .getName());

            IEditorPart editorPart = EditorUtil.findEditor(editorId);
            if (editorPart == null) {
                try {
                    new NewMapEditor().execute(null);
                } catch (ExecutionException e) {
                    throw new RuntimeException(e);
                }
                editorPart = EditorUtil.findEditor(editorId);
            }
            AbstractEditor editor = (AbstractEditor) editorPart;
            display = (AbstractRenderableDisplay) editor.getActiveDisplayPane()
                    .getRenderableDisplay().createNewDisplay();
            try {
                display.setDescriptor(new MapDescriptor());
            } catch (VizException e) {
                throw new RuntimeException(e);
            }

            AbstractEditor mapEditor = UiUtil.createOrOpenEditor(
                    VizMapEditor.EDITOR_ID, display);

            if (chooseByIdDialog == null || chooseByIdDialog.isDisposed()) {
                chooseByIdDialog = new ChooseByIdDialog(PlatformUI
                        .getWorkbench().getActiveWorkbenchWindow().getShell());
                chooseByIdDialog.setHomeResource(getResource(
                        HomeToolLayer.class, HomeToolAction.class));
                chooseByIdDialog.setPointsResource(getResource(
                        PointsToolLayer.class, PointsToolAction.class));
                chooseByIdDialog.setBaslinesResource(getResource(
                        InteractiveBaselinesLayer.class,
                        BaselinesToolAction.class));
                chooseByIdDialog.setDescriptor(mapEditor.getActiveDisplayPane()
                        .getDescriptor());

                chooseByIdDialog.open();
                chooseByIdDialog = null;
            } else {
                chooseByIdDialog.setDescriptor(mapEditor.getActiveDisplayPane()
                        .getDescriptor());

                // find and activate the dialog
                for (Shell s : chooseByIdDialog.getParent().getShells()) {
                    if (s.getText().equals(ChooseByIdDialog.DIALOG_TITLE)) {
                        s.setVisible(true);
                        s.setActive();
                    }
                }
            }
        }
        return null;
    }

}
