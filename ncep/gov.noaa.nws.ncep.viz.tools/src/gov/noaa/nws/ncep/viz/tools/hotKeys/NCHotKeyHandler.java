/*
 * NCHotKeyHandler
 * 
 *Date Created (01 October 2010)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.tools.hotKeys;

import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.ui.UiUtil;

 

/**
 * Hot Key handler to switch between tabs in the National Centers Perspective
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 *     Date        Ticket#	    Engineer	    Description
 * -------------------------------------------------------------------
 * 01-Oct-2010   289        Archana     Initial Creation
 * 02/01/2011    399        Chin        Modified to be generic hot
 *                                      key handler for all editors
 *                                      
 * 
 *</pre> 
 * @author archana
 *  @version 1.0
 *
 */
  public class NCHotKeyHandler extends AbstractHandler{

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		if(event.getCommand() == null){
			return null;
		}
				
		String tabKeyValue = event.getParameter("keyNum");
		//System.out.println("Window id: "+ tabKeyValue);
		
		int funKeyNum = Integer.parseInt(tabKeyValue);
		if(tabKeyValue == null || tabKeyValue.isEmpty()|| funKeyNum< 1){
			return null;
		}
		
		IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
		
		for( IWorkbenchWindow window : windows ) {
			IWorkbenchPage pages[] = window.getPages();
			for( IWorkbenchPage page : pages ) {
				IEditorReference[] refs = page.getEditorReferences();
				for( IEditorReference r : refs ) {	
					//System.out.println("title= "+ r.getTitle() + " id="+ r.getId() );
					String edTitle = r.getTitle();
					if(edTitle.indexOf('-')>0){
						String edNumStr = edTitle.substring(0,edTitle.indexOf('-'));
						if(edNumStr != null){
							int edNum = Integer.parseInt(edNumStr);
							if( funKeyNum == edNum ) {
								IWorkbenchPage activePage = PlatformUI.getWorkbench()
								.getActiveWorkbenchWindow().getActivePage();
								activePage.bringToTop(r.getPart(true));
								if(activePage!=null)
									return null;
							}
						}
					}
				}
			}
		}
		return null;
		
		/*
		tabKeyValue = new String( tabKeyValue + "-xyz" );

		NCMapEditor  editorToActivate = NmapUiUtils.findDisplayByID( tabKeyValue );
		
		boolean findEditor = false;
		if (editorToActivate != null) {
			String displayNameOfEditorToActivate = editorToActivate
					.getDisplayName();
			IWorkbenchPage activePage = PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getActivePage();
			IEditorReference[] iEditorRefArray = activePage
					.getEditorReferences();
			for (IEditorReference thisEditorRef : iEditorRefArray) {
				if (thisEditorRef.getTitle().compareTo(
						displayNameOfEditorToActivate) == 0) {
					activePage.bringToTop(thisEditorRef.getPart(true));
					findEditor = true;
				}
			}
		} 
		//try if it is NsharpEditor
		if(findEditor == false){
			IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
			String nskewt= "NsharpSkewt";
			for( IWorkbenchWindow window : windows ) {
				IWorkbenchPage pages[] = window.getPages();
				for( IWorkbenchPage page : pages ) {
					IEditorReference[] refs = page.getEditorReferences();
					for( IEditorReference r : refs ) {	
						if(r.getTitle().equals(nskewt)){
							IWorkbenchPage activePage = PlatformUI.getWorkbench()
							.getActiveWorkbenchWindow().getActivePage();
							activePage.bringToTop(r.getPart(true));
							//System.out.println( "title =  "+ r.getTitle() );
						}
					}
				}
			}
		}
		return null;*/
	}

 }