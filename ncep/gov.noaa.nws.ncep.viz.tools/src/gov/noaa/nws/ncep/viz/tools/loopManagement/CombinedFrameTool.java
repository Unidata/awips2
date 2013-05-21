package gov.noaa.nws.ncep.viz.tools.loopManagement;


import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * CombinedFrameTool
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 26, 2007             chammack    Initial Creation.
 *    Oct 10, 2009    169      Greg Hull   Check for time synced multipanes
 *    Sept 28, 2010   317      Xilin Guo   Create Loop Management and copy it from dwellRate. Remove dwellRate
 *    07/15/11                 C Chen      fix looping buttons not coordinated issue. Clean up code.
 *    02/12/13        972      G. Hull     call NcEditorUtil.refreshGUIElements in place of AbstractNcEditor
 *                                          
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class CombinedFrameTool extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        
    	
        String operationStr = arg0.getParameter("operation");
        String modeStr = arg0.getParameter("mode");
        
        this.editor.getLoopProperties().setLooping(false);
        IDescriptor.FrameChangeMode mode = IDescriptor.FrameChangeMode
                .valueOf(modeStr);
        IDescriptor.FrameChangeOperation operation = IDescriptor.FrameChangeOperation
                .valueOf(operationStr);
        if(editor!= null) { //Chin 
        	IDisplayPane[] panes = this.editor.getDisplayPanes();
            
        	for( IDisplayPane pane : panes ) {
        		IDescriptor desc = (IDescriptor) pane.getRenderableDisplay().getDescriptor();
        		desc.changeFrame(operation, mode);
        	}
        }
        editor.refresh();
        
        NcEditorUtil.refreshGUIElements( (AbstractEditor) editor );

        return null;

    }
}
