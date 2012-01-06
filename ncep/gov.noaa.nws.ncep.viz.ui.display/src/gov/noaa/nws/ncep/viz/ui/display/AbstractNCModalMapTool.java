
package gov.noaa.nws.ncep.viz.ui.display;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

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
public abstract class AbstractNCModalMapTool extends AbstractModalTool {

    public AbstractNCModalMapTool() {
        super();
    }

    protected NCMapEditor mapEditor=null;

    protected void activateTool(ExecutionEvent event) {
        if( editor instanceof NCMapEditor ) {
        	this.mapEditor = (NCMapEditor)super.editor;
        }
        else {
        	//System.out.println("Non-NCMap editor in AbstractNCModalMapTool??");
        	mapEditor = null;
        }
    }
    
}
