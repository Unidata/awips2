package gov.noaa.nws.ncep.ui.nsharp;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.viz.ui.tools.AbstractTool;

public class NsharpKeyHandler extends AbstractTool {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		NsharpEditor edtor =  NsharpEditor.getActiveNsharpEditor();
		if(edtor!= null&& event.getParameter("action") != null && !event.getParameter("action").isEmpty()){
			String actionToDo = new String(event.getParameter("action"));
			
			if( actionToDo.equals("arrowdown")){
				//System.out.println("Nsharp shift + arrow down ");
				if(edtor.getRscHandler()!= null)
					edtor.getRscHandler().setSteppingSndTypeList(IFrameCoordinator.FrameChangeOperation.PREVIOUS );
			}else if(actionToDo.equals("arrowup")){
				//System.out.println("Nsharp shift + arrow up");
				if(edtor.getRscHandler()!= null)
					edtor.getRscHandler().setSteppingSndTypeList(IFrameCoordinator.FrameChangeOperation.NEXT);
			}
			else 
				System.out.println("Nsharp key error");
		}
		return super.execute(event);
	}
	
}
