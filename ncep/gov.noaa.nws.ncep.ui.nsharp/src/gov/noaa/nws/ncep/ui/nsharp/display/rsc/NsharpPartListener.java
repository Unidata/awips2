package gov.noaa.nws.ncep.ui.nsharp.display.rsc;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;

public class NsharpPartListener implements IPartListener2 {
	private static NsharpPartListener instance=null;
	//private NsharpResourceHandler rscHandler;
	//Only add one listener for all Nsharp instance to share
	public static void addPartListener(){
		if(instance == null){
			instance = new NsharpPartListener();
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().addPartListener(instance);
		}

	}
	@Override
	public void partActivated(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		if(partRef.getPart(false) instanceof NsharpEditor){
			NsharpEditor ed = (NsharpEditor)partRef.getPart(false);
			System.out.println("NsharpPartActivated "+ed.toString()+ " editor's RscHdr="+ed.getRscHandler());//+" saved rscHandler="+rscHandler.toString());
			if(ed!=null && ed.getRscHandler()!=null){
				ed.getRscHandler().repopulateSndgData();
				ed.getRscHandler().resetRsc();
				ed.refresh();
			}
		}
	}

	

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartBroughtToTop rscHandler="+rscHandler.toString());

	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartCosed rscHandler="+rscHandler.toString());

	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartDeactivated rscHandler="+rscHandler.toString());

	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartOpened rscHandler="+rscHandler.toString());

	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartHidden rscHandler="+rscHandler.toString());

	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartVisible rscHandler="+rscHandler.toString());

	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		// TODO Auto-generated method stub
		//System.out.println("NsharppartInputChanged rscHandler="+rscHandler.toString());

	}

	//public void setRscHandler(NsharpResourceHandler rscHandler) {
	//	this.rscHandler = rscHandler;
	//}

}
