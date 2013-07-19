package gov.noaa.nws.ncep.viz.tools.predefinedArea;


import gov.noaa.nws.ncep.viz.common.area.IAreaProviderCapable;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
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

public class AreaFromResourceRightClickAction extends AbstractRightClickAction {

	private static String commandId = "gov.noaa.nws.ncep.viz.ui.actions.loadPredefinedArea";
	
	@Override
	public void run() {
		AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

		ICommandService service = (ICommandService)currEditor
						.getSite().getService(ICommandService.class);
		Command cmd = service.getCommand(commandId);

		if( cmd != null ) {
			try {
				Map<String, String> cmdParams = new HashMap<String, String>();
		        
				if( !(getSelectedRsc() instanceof IAreaProviderCapable) ) {
					System.out.println("???Selected resource is not capable of defining its Area");
					return;
				}
				IAreaProviderCapable areaProv = (IAreaProviderCapable)getSelectedRsc(); 
				cmdParams.put("areaName", areaProv.getAreaName() );
				cmdParams.put("areaSource", areaProv.getSourceProvider().toString() );
			
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
