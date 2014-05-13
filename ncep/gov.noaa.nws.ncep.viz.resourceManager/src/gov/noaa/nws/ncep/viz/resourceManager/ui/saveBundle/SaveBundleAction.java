package gov.noaa.nws.ncep.viz.resourceManager.ui.saveBundle;

import java.util.HashMap;

import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resourceManager.ui.ResourceManagerDialog;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.CreateRbdControl;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.ResourceSelectionDialog;
import gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd.SaveRbdDialog;
import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceFactory.ResourceSelection;
import gov.noaa.nws.ncep.viz.resources.manager.RscBundleDisplayMngr.PaneSelectionData;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneID;
import gov.noaa.nws.ncep.viz.ui.display.NcPaneLayout;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 
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




