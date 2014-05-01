package gov.noaa.nws.ncep.viz.tools.loopManagement;

import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.datastructure.LoopProperties.LoopMode;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Activate forward looping.
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
 * 02/12/13        972      G. Hull     call NcEditorUtil.refreshGUIElements in place of AbstractNcEditor
 * 
 * </pre>
 * 
 * @author mli
 * @version 1
 */
public class LoopForwardAction extends AbstractTool {

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
        //System.out.println("loop forward entered: current mode = "+ loopProperties.getMode().toString());
        // Stop forward looping
        if (loopProperties.isLooping() && loopProperties.getMode() == LoopMode.Forward) {
        	newState = false;
        }
        // Activate forward looping
        else {
        	if (loopProperties.getFwdFrameTime() == LoopProperties.MIN_DWELL_TIME &&
        			loopProperties.getRevFrameTime() > 0) {
        		loopProperties.setFwdFrameTime(loopProperties.getRevFrameTime());
        	}
        	
        	// Disable Backward looping
        	loopProperties.setRevFrameTime(LoopProperties.MIN_DWELL_TIME);
        	
        	loopProperties.setMode(LoopMode.Forward);
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
    	}*/
    	
        loopProperties.setLooping(newState);
        editor.setLoopProperties(loopProperties);
    	this.setEnabled(newState);

    	NcEditorUtil.refreshGUIElements( (AbstractEditor) editor );
    	
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
    	//System.out.println("LoopForwardAction updateElement entered");
        AbstractEditor editor = EditorUtil.getActiveEditorAs(AbstractEditor.class);
        if (editor != null) {
            this.editor = editor;
            LoopProperties loopProperties = this.editor.getLoopProperties();
            element.setChecked(loopProperties.isLooping() && loopProperties.getMode() == LoopMode.Forward);
        }
    }

}

