package gov.noaa.nws.ncep.viz.tools.ncInventoryControl;

import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

public class NcInventoryControlAction extends AbstractHandler {
	NcInventoryControlDlg dlg = null;
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		dlg = null;
		
	//	if( dlg == null ) {
			dlg = new NcInventoryControlDlg( NcDisplayMngr.getCaveShell() );
	//	}

//		if( !dlg.isOpen() ) {
			dlg.open();			
//		}
		
		return null;
	}

}
