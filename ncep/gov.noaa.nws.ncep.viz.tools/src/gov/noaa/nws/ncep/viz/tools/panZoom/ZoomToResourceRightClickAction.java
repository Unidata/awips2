package gov.noaa.nws.ncep.viz.tools.panZoom;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *                                       Created
 *   05/15/13   #862       G. Hull       Modify to use for new AreaProvider/AreaName
 *   
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

import gov.noaa.nws.ncep.viz.common.area.AreaName;
import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.tools.panZoom.ZoomToAction.ZoomType;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.editor.AbstractEditor;

public class ZoomToResourceRightClickAction extends AbstractRightClickAction {

	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.zoomTo";
	
	@Override
	public void run() {
		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
 
		ICommandService service = (ICommandService)currEditor
						.getSite().getService(ICommandService.class);
		Command cmd = service.getCommand(commandId);

		if( cmd != null ) {
			try {		        
				Map<String, String> cmdParams = new HashMap<String, String>();
				cmdParams.put("zoomType", ZoomType.AREA_PROVIDER.toString() );
		        
				IAreaProviderCapable areaProv = (IAreaProviderCapable)getSelectedRsc();
				AreaName areaName = new AreaName( areaProv.getSourceProvider(), areaProv.getAreaName() );
				cmdParams.put("zoomLevel", areaName.toString() );
			
				ExecutionEvent exec = new ExecutionEvent(cmd, cmdParams, null, null);
				cmd.executeWithChecks(exec);
			} 
			catch (Exception ex) {
				ex.printStackTrace();
				System.out.println("Error executing cmd to zoomTo Resource: "+ commandId );
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	public String getText() {
		return "Zoom To Size of Image";
	}
	
//	@Override
//	public void setSelectedRsc(ResourcePair selectedRsc) {
//		super.setSelectedRsc(selectedRsc);
//	}

}
