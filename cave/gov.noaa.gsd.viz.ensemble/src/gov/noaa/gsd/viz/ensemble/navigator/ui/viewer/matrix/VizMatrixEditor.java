package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.viz.core.maps.display.VizMapEditor;

/**
 * 
 * This class is for all intents and purposes equivalent to the parent class
 * VizMapEditor. It is needed as a separate class for proper key bindings (i.e.
 * by type) and RTTI.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2017   41520     polster     Initial creation
 *
 * </pre>
 *
 * @author polster
 */
public class VizMatrixEditor extends VizMapEditor {

    public static final String EDITOR_ID = "gov.noaa.gsd.viz.ensemble.navigator.matrix.editor";

    public VizMatrixEditor() {
        super();
    }

    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        super.init(site, input);
        setTabTitle(MatrixNavigatorComposite.MATRIX_EDITOR_TAB_TITLE);
    }

}
