package gov.noaa.gsd.viz.ensemble.control;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.EditorReference;
import org.eclipse.ui.part.EditorPart;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * This is the part listener for the editors associated with Ensemble Tool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014    5056      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class EnsembleEditorPartListener implements IPartListener2 {

    private List<IDisplayPaneContainer> editors = null;

    public EnsembleEditorPartListener() {
        editors = new ArrayList<>();
    }

    public void addEditor(IDisplayPaneContainer e) {
        editors.add(e);
    }

    public void removeEditor(IDisplayPaneContainer e) {
        editors.remove(e);
    }

    @Override
    public void partActivated(IWorkbenchPartReference partRef) {

        if (containsPart(partRef)) {
            IEditorPart editorPart = (EditorPart) partRef.getPart(false);

            if (editorPart instanceof IDisplayPaneContainer) {
                IDisplayPaneContainer editor = (IDisplayPaneContainer) editorPart;
                EnsembleTool.getInstance().setEditor(editor);
                EnsembleTool.getInstance().refreshTool(true);
            }
        }
    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {

            EditorPart editorPart = (EditorPart) partRef.getPart(false);
            if (editorPart instanceof AbstractEditor) {
                VizApp.runSync(new Runnable() {
                    public void run() {

                        if (!PlatformUI.getWorkbench().isClosing()) {
                            EnsembleTool.getInstance()
                                    .verifyCloseActiveToolLayer();
                        }

                    }
                });
            }
        }
    }

    @SuppressWarnings("restriction")
    @Override
    public void partOpened(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {
            /* ignore */
        }
        /*
         * Did another editor open? If so, if there is not already a tool layer
         * associated with it the tell the manager about the new editor.
         */
        else if (partRef instanceof EditorReference) {
            EditorPart openedEditor = (EditorPart) ((EditorReference) partRef)
                    .getPart(false);
            if (openedEditor instanceof IDisplayPaneContainer) {
                IDisplayPaneContainer editor = (IDisplayPaneContainer) openedEditor;
                EnsembleTool.getInstance().setEditor(editor);
                if (!EnsembleTool.getInstance().hasToolLayer(editor)) {
                    EnsembleTool.getInstance().createToolLayer(editor);
                }
                EnsembleTool.getInstance().refreshTool(true);
            }
        }
    }

    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        if (containsPart(partRef)) {
            // TODO
        }
    }

    private boolean containsPart(IWorkbenchPartReference ref) {
        boolean containsPart = false;
        IWorkbenchPart part = ref.getPart(false);
        if (part != null) {

        }
        for (IDisplayPaneContainer ep : editors) {
            if (part.equals(ep)) {
                containsPart = true;
                break;
            }
        }
        return containsPart;
    }
}
