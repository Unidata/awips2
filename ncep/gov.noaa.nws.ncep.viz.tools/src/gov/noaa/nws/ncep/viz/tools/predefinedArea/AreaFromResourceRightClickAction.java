package gov.noaa.nws.ncep.viz.tools.predefinedArea;


import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.ui.display.IGridGeometryProvider;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea.AreaSource;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

public class AreaFromResourceRightClickAction extends AbstractRightClickAction {

	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea";
	
	@Override
	public void run() {
		System.out.println("Running AreaFromResourceRightClickAction");
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();


		ICommandService service = (ICommandService)currEditor
						.getSite().getService(ICommandService.class);
		Command cmd = service.getCommand(commandId);

		if( cmd != null ) {
			try {
				//HashMap<String, Object> params = new HashMap<String, Object>();
		        
				Map<String, String> cmdParams = new HashMap<String, String>();
		        
				if( !(getSelectedRsc().getResourceData() instanceof IGridGeometryProvider) ) {
					System.out.println("???Selected resource is not capable of defining its Area");
					return;
				}
				
				cmdParams.put("areaName", 
						((INatlCntrsResourceData)getSelectedRsc().getResourceData()).getResourceName().toString() );
				cmdParams.put("areaType", AreaSource.RESOURCE_DEFINED.toString() );
			
				ExecutionEvent exec = new ExecutionEvent(cmd, cmdParams, null, null);
				cmd.executeWithChecks(exec);
			} 
			catch (Exception ex) {
				ex.printStackTrace();
				System.out.println("Error executing cmd to change the area: "+ commandId );
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	public String getText() {
		return "Change Area to Fit Image";
	}
	
//	@Override
//	public void setSelectedRsc(ResourcePair selectedRsc) {
//		super.setSelectedRsc(selectedRsc);
//	}

}
