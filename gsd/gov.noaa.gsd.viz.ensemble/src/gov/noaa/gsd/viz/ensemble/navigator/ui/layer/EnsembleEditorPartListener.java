package gov.noaa.gsd.viz.ensemble.navigator.ui.layer;

import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

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

    private AbstractEditor editor = null;

    public EnsembleEditorPartListener(AbstractEditor editor) {
        this.editor = editor;
    }

    @Override
    public void partActivated(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            EnsembleToolManager.getInstance().switchToEditor(editor, false);
        }
    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            EnsembleToolManager.getInstance().unloadActiveToolLayer();
        }
    }

    @Override
    public void partOpened(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    private boolean isThisPart(IWorkbenchPartReference ref) {
        IWorkbenchPart part = ref.getPart(false);
        return part != null && part.equals(editor);
    }

}
