package gov.noaa.nws.ncep.viz.tools.panZoom;


import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.tools.panZoom.ZoomToAction.ZoomType;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

public class ZoomToResourceRightClickAction extends AbstractRightClickAction {

	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.zoomTo";
	
	@Override
	public void run() {
		System.out.println("Running AreaFromResourceRightClickAction");
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();

		ICommandService service = (ICommandService)currEditor
						.getSite().getService(ICommandService.class);
		Command cmd = service.getCommand(commandId);

		if( cmd != null ) {
			try {		        
				Map<String, String> cmdParams = new HashMap<String, String>();
				cmdParams.put("zoomType", ZoomType.RESOURCE_DEFINED.toString() );
		        
//				if( !(getSelectedRsc().getResourceData() instanceof IGridGeometryProvider) ) {
//					System.out.println("???Selected resource is not capable of defining its Area");
//					return;
//				}
				
				cmdParams.put("zoomLevel", 
						((INatlCntrsResourceData)getSelectedRsc().getResourceData()).getResourceName().toString() );
			
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
