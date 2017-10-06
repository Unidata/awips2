package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * This tool handles frame and data navigation requests including step
 * left/right (accessed with the left and right arrow keys) and choose model
 * source (accessed via the up and down arrows).
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Feb 23 2016    13211       polster     Initial Creation.
 * 
 * </pre>
 * 
 * @author polster
 * @version 1
 */
public class MatrixNavigationTool extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        this.editor = EditorUtil.getActiveVizContainer();
        if (editor != null && editor instanceof VizMatrixEditor) {

            String operationStr = arg0.getParameter("operation");

            VizMatrixEditor.MatrixNavigationOperation operation = VizMatrixEditor.MatrixNavigationOperation
                    .valueOf(operationStr);
            EnsembleTool.getInstance().matrixNavigationRequest(operation);

            editor.refresh();
        }

        return null;

    }

}
