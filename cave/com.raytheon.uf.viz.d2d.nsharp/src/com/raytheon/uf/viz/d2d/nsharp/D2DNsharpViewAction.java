/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpViewAction
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 06/25/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package com.raytheon.uf.viz.d2d.nsharp;

 

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

public class D2DNsharpViewAction extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException { 
		/*
		 *  The viewID string is in the XML file for NSHARP extension point. 
		 */
		String viewid = "com.raytheon.uf.viz.d2d.nsharp.display.D2DNSharpPaletteWindow";
        IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        IViewPart vpart = wpage.findView( viewid);
        try {
            if ( vpart == null ){
                vpart = wpage.showView(viewid );
            } else {
                if ( ! wpage.isPartVisible(vpart) ) vpart = wpage.showView( viewid );
            }
        }
        catch (Exception e) {
        	e.printStackTrace();
        }
		
		return null;
	}

}
