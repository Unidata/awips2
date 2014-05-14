package gov.noaa.nws.ncep.viz.resourceManager.ui.saveBundle;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.SaveRbdDialog;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Action to save bundle (RBD) from outside the RBD Manager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/13/14		   --		M.James/UPC Initial creation
 * </pre>
 * 
 * @author mjames
 * @version 1
 */
public class SaveBundleAction extends AbstractHandler {

	//private RscBundleDisplayMngr rbdMngr;
	 // used to initialize the Save Dialog
    private String savedSpfGroup = "default";
    private String savedSpfName  = null;
    private String rbd_name_txt = null;
    private Shell shell;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		
		shell = NcDisplayMngr.getCaveShell();

		final AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
		
		if( editor == null ) {
    		MessageDialog errDlg = new MessageDialog( 
    				NcDisplayMngr.getCaveShell(), 
    				"Error", null, 
    				"Can't save data from this type of display",
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		errDlg.open();
    		return null;
		}
		
		IRenderableDisplay display = 
				   (IRenderableDisplay)NcEditorUtil.getSelectedPanes(editor)[0].getRenderableDisplay();
			
		final AbstractTimeMatcher tm = display.getDescriptor().getTimeMatcher();
		
		final NCTimeMatcher timeMatcher = (NCTimeMatcher)tm;
		
		final NcDisplayType dispType = NcEditorUtil.getNcDisplayType( editor );
		
		SaveRbdDialog saveDlg = new SaveRbdDialog( shell,
				"default", savedSpfName, rbd_name_txt, false, false ); 
		saveDlg.open();
		savedSpfName  = saveDlg.getSeldSpfName();
		rbd_name_txt = saveDlg.getSeldRbdName();

		if( saveDlg.getRbdOkay() ) {
			RscBundleDisplayMngr rbdMngr = new RscBundleDisplayMngr( 
	    			new NcPaneLayout(6,6), dispType );
			AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
			try {
				AbstractRBD<?> rbdBndl = AbstractRBD.createRbdFromEditor(currEditor);
				rbdBndl = AbstractRBD.clone( rbdBndl );
				rbdBndl.setRbdName(rbd_name_txt);
				SpfsManager.getInstance().saveRbdToSpf( savedSpfGroup, savedSpfName, rbdBndl, false, false );
			} catch (VizException e1) {
				e1.printStackTrace();
			}
			
			editor.refresh();
			NcEditorUtil.refreshGUIElements(editor);

			VizApp.runSync(new Runnable() {
				public void run() {
					String msg = null;
					msg = new String("bundle "+ rbd_name_txt + " saved to "+ savedSpfName +".");
					MessageBox mb = new MessageBox( shell, SWT.OK );         								
					mb.setText( "Bundle Saved" );
					mb.setMessage( msg );
					mb.open();
				}
			});
		  
		}
		
		return null;
	}
}




