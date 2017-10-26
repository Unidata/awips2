package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;

import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

/**
 * The part listener for the EnsembleToolViewer class (ViewPart). The most
 * critical reason for this listener is to turn on/off the CAVE tool layer
 * editability flag (i.e. whether the tool layer has control of user
 * interactivity or not) when the view state changes (becomes
 * activated/deactivated).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014    5056      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class EnsembleToolViewerPartListener implements IPartListener2 {

    private EnsembleToolViewer ensembleToolViewer = null;

    public EnsembleToolViewerPartListener(EnsembleToolViewer fsv) {
        ensembleToolViewer = fsv;
    }

    @Override
    public void partActivated(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // ensembleToolViewer.setFocus();`
        }
    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            /* TODO */
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partHidden(org.eclipse.ui.
     * IWorkbenchPartReference)
     * 
     * Whenever the ViewPart becomes hidden, then turn off tool layer
     * editability.
     */
    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            /* TODO */
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partVisible(org.eclipse.ui.
     * IWorkbenchPartReference)
     * 
     * Whenever the ViewPart becomes visible, then, if the user preference is
     * set for it, turn on tool layer editability.
     */
    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            if (EnsembleTool.getInstance().isToolEditable()
                    && EnsembleTool.getInstance()
                            .isMakeEditableOnRestorePreference()) {
                EnsembleTool.getInstance().setEditable(true);
            }
        }
    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            /* TODO */
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            /* TODO */
        }
    }

    @Override
    public void partOpened(IWorkbenchPartReference partRef) {

        if (isThisPart(partRef)) {
            /* TODO */
        }
    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            /* TODO */
        }
    }

    private boolean isThisPart(IWorkbenchPartReference ref) {
        IWorkbenchPart part = ref.getPart(false);
        return part != null && part.equals(ensembleToolViewer);
    }

}
