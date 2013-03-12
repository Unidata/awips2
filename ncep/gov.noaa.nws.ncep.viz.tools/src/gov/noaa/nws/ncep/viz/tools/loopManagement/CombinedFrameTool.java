package gov.noaa.nws.ncep.viz.tools.loopManagement;


import gov.noaa.nws.ncep.viz.common.AbstractNcEditor;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.xy.VizXyEditor;
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
 *    02/13/13      958        S. Gurung   Added temporary code (for solar image display) to refresh GUI elements for editor of type VizXyEditor 
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
        if(editor != null && editor instanceof AbstractNcEditor){
        	AbstractNcEditor e = (AbstractNcEditor)editor;
        	e.refreshGUIElements();
        }
        /* temporary code (added to make the step forward/backward buttons work for solar image display) */
    	else if (editor != null && editor instanceof VizXyEditor) {
    		refreshGUIElements((AbstractEditor)editor);
    	}
        return null;

    }
    
    public void refreshGUIElements(AbstractEditor editor) {
        ICommandService service = (ICommandService) editor.getSite().getService(
                ICommandService.class);

        String[] guiUpdateElementCommands = {
                // "gov.noaa.nws.ncep.viz.tools.pan",
                "gov.noaa.nws.ncep.viz.ui.options.SyncPanes",
                "gov.noaa.nws.ncep.viz.ui.actions.loopBackward",
                "gov.noaa.nws.ncep.viz.ui.actions.loopForward",
                "gov.noaa.nws.ncep.viz.ui.actions.rock",
                "gov.noaa.nws.ncep.viz.ui.actions.frameTool",
                "gov.noaa.nws.ncep.viz.ui.autoUpdate",
                "gov.noaa.nws.ncep.viz.ui.actions.hideFrames" };
        // Update the GUI elements on the menus and toolbars
        for (String toolbarID : guiUpdateElementCommands) {
            service.refreshElements(toolbarID, null);
        }
    }
}
