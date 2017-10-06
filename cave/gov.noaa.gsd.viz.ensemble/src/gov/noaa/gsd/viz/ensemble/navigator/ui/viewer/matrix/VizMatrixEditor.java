package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.maps.scales.MapScales.PartId;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager.ManagedMapScale;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.VizMultiPaneEditor;

/**
 * Defines the matrix editor. This editor is used to contain resources that are
 * loaded as model families, in order to allow the user to compare elements
 * (field/plane pairs) between sources.
 * 
 * Currently, this class is functionally equivalent to the
 * <code>VizMapEditor</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 * Nov 11, 2015     12302       polster     Initial version
 * </pre>
 * 
 * @author polster
 * 
 */
public class VizMatrixEditor extends VizMultiPaneEditor {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VizMatrixEditor.class);

    public static final String EDITOR_ID = "gov.noaa.gsd.viz.ensemble.ui.map.MatrixEditor";

    public static enum MatrixNavigationOperation {
        UP_MODEL_SOURCE, DOWN_MODEL_SOURCE, LEFT_FRAME, RIGHT_FRAME
    };

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.AbstractEditor#init(org.eclipse.ui.IEditorSite
     * , org.eclipse.ui.IEditorInput)
     */
    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        super.init(site, input);
        setTabTitle("Matrix");
    }

    public static AbstractEditor create() {
        AbstractEditor editor = null;

        ManagedMapScale editorScale = null;
        for (ManagedMapScale scale : MapScalesManager.getInstance().getScales()) {
            for (PartId partId : scale.getPartIds()) {
                if (partId.isView() == false) {
                    editorScale = scale;
                }
            }
        }

        if (editorScale != null) {
            try {
                Bundle b = editorScale.getScaleBundle();
                editor = UiUtil.createEditor(EDITOR_ID, b.getDisplays());
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to load bundle for scale " + editorScale
                                + " to screen", e);
            }
        } else {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to find an editor based map scale");
        }
        return editor;
    }
}
