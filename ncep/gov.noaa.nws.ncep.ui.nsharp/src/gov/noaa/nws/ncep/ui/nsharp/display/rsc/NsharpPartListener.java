package gov.noaa.nws.ncep.ui.nsharp.display.rsc;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;

public class NsharpPartListener implements IPartListener2 {
	private static NsharpPartListener instance=null;
	//Only add one listener for all Nsharp instance to share
	public static void addPartListener(){
		if(instance == null){
			instance = new NsharpPartListener();
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().addPartListener(instance);
		}

	}
	public enum PartEvent {
		partActivated, partBroughtToTop,partClosed,partDeactivated,partOpened,partHidden, partInputChanged, partVisible
	};
	
	private void handleEvent(IWorkbenchPartReference partRef,PartEvent event){
		if(partRef.getPart(false) instanceof NsharpEditor){
			NsharpEditor ed = (NsharpEditor)partRef.getPart(false);
			if(ed!=null && ed.getRscHandler()!=null){
				//System.out.println(event+" rscHandler="+ed.getRscHandler().toString());//+" saved rscHandler="+rscHandler.toString());
				ed.getRscHandler().handleNsharpEditorPartEvent(event);
			}
		}
	}
	@Override
	public void partActivated(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partActivated);
	}



	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partBroughtToTop);
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partClosed);
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partDeactivated);
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partOpened);
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partHidden);
	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partVisible);
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		handleEvent(partRef,PartEvent.partInputChanged);
	}

}
