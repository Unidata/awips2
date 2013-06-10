
package gov.noaa.nws.ncep.viz.ui.display;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * Adds convenience methods for editor to avoid casting
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------	----------- --------------------------
 *  09/29/09     #169        Greg Hull    Initial Creation.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public abstract class AbstractNcModalTool extends AbstractModalTool {

    public AbstractNcModalTool() {
        super();
    }

    protected AbstractEditor mapEditor=null;

    protected void activateTool(ExecutionEvent event) {
        if( NcDisplayMngr.isNatlCntrsEditor( (AbstractEditor)editor ) ) {
        	this.mapEditor = (AbstractEditor)super.editor;
        }
        else {
        	System.out.println("Non-NCMap editor in AbstractNcModalTool??");
        	mapEditor = null;
        }
    }
    
}
