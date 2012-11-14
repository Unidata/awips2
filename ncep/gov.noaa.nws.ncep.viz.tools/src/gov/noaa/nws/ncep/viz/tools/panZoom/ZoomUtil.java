package gov.noaa.nws.ncep.viz.tools.panZoom;

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.ISourceProviderService;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;

public class ZoomUtil {

    private static final IInputHandler suspendZoom = new SuspendZoomHandler();

    private static ISelectedPanesChangedListener paneListener = new ZoomManager();

    /**
     * suspend Zoom capabilities
     */
    public static void suspendZoom(IDisplayPaneContainer editor) {

        if (editor != null) {
            disableZoomWheel(editor);
            disableZoomTools();

            if (editor.getDisplayPanes().length > 1
                    && editor instanceof AbstractEditor) {
                AbstractEditor ed = (AbstractEditor) editor;
                ((EditorInput) ed.getEditorInput()).getPaneManager()
                        .addSelectedPaneChangedListener(paneListener);
            }
        }

    }

    /**
     * resume Zoom capabilities
     */
    public static void allowZoom(IDisplayPaneContainer editor) {

        if (editor != null) {
            enableZoomWheel(editor);
            enableZoomTools();
        }
    }

    public static void disableZoom(IDisplayPaneContainer editor) {
        disableZoomTools();
        disableZoomWheel(editor);
    }

    public static void enableZoom(IDisplayPaneContainer editor) {
        enableZoomTools();
        enableZoomWheel(editor);
    }

    public static void disableZoomTools() {

        IWorkbenchWindow workbenchWindow = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        ISourceProviderService spService = (ISourceProviderService) workbenchWindow
                .getService(ISourceProviderService.class);
        ZoomStateSourceProvider zoomStateSourceProvider = (ZoomStateSourceProvider) spService
                .getSourceProvider(ZoomStateSourceProvider.ZOOM_STATE);
        zoomStateSourceProvider.setZoomSuspended(true);
    }

    public static void enableZoomTools() {

        IWorkbenchWindow workbenchWindow = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        ISourceProviderService spService = (ISourceProviderService) workbenchWindow
                .getService(ISourceProviderService.class);
        ZoomStateSourceProvider zoomStateSourceProvider = (ZoomStateSourceProvider) spService
                .getSourceProvider(ZoomStateSourceProvider.ZOOM_STATE);
        zoomStateSourceProvider.setZoomSuspended(false);

    }

    private static void disableZoomWheel(IDisplayPaneContainer editor) {
        editor.registerMouseHandler(suspendZoom, InputPriority.RESOURCE);
    }

    private static void enableZoomWheel(IDisplayPaneContainer editor) {
        editor.unregisterMouseHandler(suspendZoom);
    }

}
