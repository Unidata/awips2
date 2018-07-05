package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.tools.AbstractTool;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;

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
 *    Feb 23 2016    13211      polster     Initial creation
 *    Dec 01, 2017   41520      polster     Cleaned up comments
 * 
 * </pre>
 * 
 * @author polster
 * @version 1
 */
public class MatrixNavigationTool extends AbstractTool {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        this.editor = EditorUtil.getActiveVizContainer();

        if (editor != null && EnsembleTool.isMatrixEditor(editor)) {

            String operationStr = arg0.getParameter("operation");

            EnsembleTool.MatrixNavigationOperation operation = EnsembleTool.MatrixNavigationOperation
                    .valueOf(operationStr);
            EnsembleTool.getInstance().matrixNavigationRequest(operation);

            editor.refresh();

        }

        return null;

    }

}
