package gov.noaa.nws.ncep.viz.ui.display;

import org.eclipse.ui.PartInitException;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.editor.EditorInput;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/09/13      #972      Greg Hull   Created to derive from AbstractNcEditor and
 *                                     replace NCMapEditor
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class NatlCntrsEditor extends AbstractNcEditor {
    
    @Override
    protected void validateEditorInput(EditorInput input)
            throws PartInitException {
        super.validateEditorInput(input);
        
        // TODO : implement this to validate the 
        if( input.getPaneManager() != null && 
        	input.getPaneManager() instanceof NCPaneManager == false) {
        	
            throw new PartInitException("Expected pane manager of type: "
                    + NCPaneManager.class);
        }
        
        // Renderable displays are validated in the base class
    }
}
