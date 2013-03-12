package gov.noaa.nws.ncep.viz.tools.loopManagement;

import gov.noaa.nws.ncep.viz.common.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.menus.UIElement;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.datastructure.LoopProperties.LoopMode;
import com.raytheon.uf.viz.xy.VizXyEditor;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Activate backward looping
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/20/09      108        M. Li    	Initial Creation.
 * 09/28/10		 317        X. Guo    	Copy it from dwellRate and 
 *                                      add codes to check Loop Stop.
 * 03/07/11      migration  G. Hull     use NCLoopProperties                                    
 * 07/15/11                 C Chen      fix looping buttons not coordinated issue. Clean up code.
 * 02/13/13      958        S. Gurung   Added temporary code (for solar image display) to refresh GUI elements for editor of type VizXyEditor 
 * 
 * </pre>
 * 
 * @author mli
 * @version 1
 */
public class LoopBackwardAction extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        
        if( !(editor.getLoopProperties() instanceof NCLoopProperties) ) {
        	System.out.println("sanity check: editor LoopProperties is not NCLoopProperties");
        	return null;
        }
        
        NCLoopProperties loopProperties = (NCLoopProperties) editor.getLoopProperties();
        boolean newState;
        
        // Stop backward looping
        if (loopProperties.isLooping() && loopProperties.getMode() == LoopMode.Backward) {
        	newState = false;
        }
        // Activate backward looping
        else {
        	if (loopProperties.getRevFrameTime() == LoopProperties.MIN_DWELL_TIME && 
        			loopProperties.getFwdFrameTime() > 0) {
        		loopProperties.setRevFrameTime(loopProperties.getFwdFrameTime());
        	}
        	
        	// Disable forward looping
        	loopProperties.setFwdFrameTime(LoopProperties.MIN_DWELL_TIME);
        	
        	loopProperties.setMode(LoopMode.Backward);
        	newState = true;
        }

        /* Chin, never goes in to this loop at all
    	if ( (! newState) && (!loopProperties.getLoopStopCurrent())){
 	       IDescriptor.FrameChangeMode mode = IDescriptor.FrameChangeMode.valueOf("TIME_ONLY");
 	       IDescriptor.FrameChangeOperation operation = IDescriptor.FrameChangeOperation.valueOf("LAST");
     	   IDisplayPane[] panes = this.editor.getDisplayPanes();
         
     	   for( IDisplayPane pane : panes ) {
     		    IDescriptor desc = (IDescriptor) pane.getRenderableDisplay().getDescriptor();
     		    desc.changeFrame(operation, mode);
     	   }
     	  editor.refresh();
    	}   */     
        loopProperties.setLooping(newState);
        editor.setLoopProperties(loopProperties);
    	this.setEnabled(newState);
        
    	if(editor != null && editor instanceof AbstractNcEditor){
        	AbstractNcEditor e = (AbstractNcEditor)editor;
        	e.refreshGUIElements();
        }
    	/* temporary code (added to make the looping work for solar image display) */
    	else if (editor != null && editor instanceof VizXyEditor) {
    		refreshGUIElements((AbstractEditor)editor);
    	}
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#updateElement(org.eclipse.ui.menus
     * .UIElement, java.util.Map)
     */
    @Override
    public void updateElement(UIElement element, Map parameters) {
        AbstractEditor editor = EditorUtil.getActiveEditorAs(AbstractEditor.class);
        if (editor != null) {
            this.editor = editor;
        	LoopProperties loopProperties = this.editor.getLoopProperties();
            element.setChecked(loopProperties.isLooping() && loopProperties.getMode() == LoopMode.Backward);
        }
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
