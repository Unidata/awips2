/*
 * gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteAction
 * 
 * 25 November 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.palette;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.WorkbenchPage;

import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

public class PgenPaletteAction extends AbstractHandler {

	@SuppressWarnings("restriction")
	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException { 
		
		/*
		 *  The viewID string is in the XML file for PGEN extension point. 
		 */

		AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();
//		if( editor instanceof NCMapEditor ) {//&& ((NCMapEditor) editor).getApplicationName().equals("NA") ) {
		if( editor instanceof AbstractEditor ) {//&& ((NCMapEditor) editor).getApplicationName().equals("NA") ) {
			IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

			IViewPart vpart = wpage.findView( PgenUtil.VIEW_ID );


			try {

				if ( vpart == null ){

					vpart = wpage.showView( PgenUtil.VIEW_ID );
					IViewReference pgenViewRef =   wpage.findViewReference(PgenUtil.VIEW_ID);
					if(pgenViewRef != null&& wpage instanceof WorkbenchPage ){
						( (WorkbenchPage) wpage ).detachView(pgenViewRef);
					}
				}
				else {

					if ( ! wpage.isPartVisible(vpart) ){ 
						vpart = wpage.showView( PgenUtil.VIEW_ID );
						IViewReference pgenViewRef =   wpage.findViewReference(PgenUtil.VIEW_ID);
						if(pgenViewRef != null&& wpage instanceof WorkbenchPage ){
							( (WorkbenchPage) wpage ).detachView(pgenViewRef);
						}
					}

				}
			}
			catch (Exception e) {

				e.printStackTrace();

			}
		} 
		else {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(); 
			MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING
					| SWT.OK);

			mb.setMessage( "Pgen is not supported in this editor. Please select a mapEditor for Pgen to use first!");
			mb.open();
		}
		return null;
	}

}
