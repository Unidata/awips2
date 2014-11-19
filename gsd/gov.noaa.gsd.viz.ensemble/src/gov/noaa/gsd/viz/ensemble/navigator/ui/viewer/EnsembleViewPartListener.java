package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer;

import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolManager;

import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.VizApp;

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
public class EnsembleViewPartListener implements IPartListener2 {

    public static boolean isHidden = false;

    private EnsembleToolViewer ensembleToolViewer = null;

    public EnsembleViewPartListener(EnsembleToolViewer fsv) {
        ensembleToolViewer = fsv;
    }

    @Override
    public void partActivated(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            EnsembleToolManager.getInstance().setEditable(true);
        }
    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            VizApp.runAsync(new Runnable() {
                public void run() {
                    try {
                        Thread.sleep(10);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                    if (isHidden) {
                        EnsembleToolManager.getInstance().setEditable(false);
                    }
                }
            });
        }
    }

    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            isHidden = true;
        }
    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            isHidden = false;
        }
    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {

            isHidden = false;

            VizApp.runAsync(new Runnable() {
                public void run() {
                    if (!PlatformUI.getWorkbench().isClosing()) {
                        EnsembleToolManager.getInstance().close();
                    }
                }
            });
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        if (isThisPart(partRef)) {
            // TODO
        }
    }

    @Override
    public void partOpened(IWorkbenchPartReference partRef) {
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
        return part != null && part.equals(ensembleToolViewer);
    }

}
