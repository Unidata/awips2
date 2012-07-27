/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.NsharpUnloadDialog
 * 
 * This java class performs the NSHARP NsharpLoadDialog functions.
 * This code is developed by NCEP-SIB for AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/21/2011	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

public class NsharpUnloadDialog extends Dialog {
	private static NsharpUnloadDialog INSTANCE = null;
	private   org.eclipse.swt.widgets.List sndTimeList;
	private  List<String> selectedTimeList = new ArrayList<String>(); 
	protected Composite top;
	protected NsharpUnloadDialog(Shell parentShell) {
		super(parentShell);
	}
	public static NsharpUnloadDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
			INSTANCE = new NsharpUnloadDialog( parShell );	//System.out.println("new unload dialog INSTANCE created");
			
		}
		return INSTANCE;
		
	}
	
	@Override
	public int open() {
		return super.open();
	}
	@Override
	public boolean close() {
		
		if(sndTimeList!=null){
			sndTimeList.removeListener(SWT.Selection, sndTimeList.getListeners(SWT.Selection)[0]);
			sndTimeList.dispose();
			sndTimeList = null;
		}
		if(selectedTimeList!= null){
			selectedTimeList.clear();
			selectedTimeList = null;
		}
		INSTANCE = null;
		return super.close();
	}

	@Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Unload" );
	}
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.CANCEL_ID,
				IDialogConstants.CANCEL_LABEL, false);
		
	}


	@Override
	protected Control createDialogArea(Composite parent) {
		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;

		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		createDiaContents(top);

		return top;
	}


	private void createDiaContents(Composite parent) {
		//create file widget list 
		Group sndTimeListGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		//sndTimeListGp.setText("Loaded Sounding Times:");
		sndTimeList = new org.eclipse.swt.widgets.List(sndTimeListGp, SWT.BORDER | SWT.MULTI| SWT.V_SCROLL  );
		sndTimeList.setBounds(0,0, 2*NsharpConstants.listWidth, NsharpConstants.listHeight * 8);
		createSndList();
        //create a selection listener to handle user's selection on list		
		sndTimeList.addListener ( SWT.Selection, new Listener () {
        	private String selectedSndTime=null;	
    		public void handleEvent (Event e) {   			
    			if (sndTimeList.getSelectionCount() > 0 ) {  	
    				selectedTimeList.clear();
    				for(int i=0; i < sndTimeList.getSelectionCount(); i++) {
    					selectedSndTime = sndTimeList.getSelection()[i];
    					selectedTimeList.add(selectedSndTime);
    				}
    				
    			}
    		}
    	});
		
		Group buttonGp = new Group(parent,SWT.SHADOW_OUT);
		buttonGp.setLayout( new GridLayout(2, false) );

		Button unloadAllBtn = new Button(buttonGp, SWT.PUSH);
		unloadAllBtn.setText("   Unload All   ");
		unloadAllBtn.setEnabled( true );
		unloadAllBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {   
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				MessageBox messageBox = new MessageBox(shell, SWT.ICON_WARNING | SWT.YES | SWT.NO);	        
				messageBox.setText("Warning");
				messageBox.setMessage("Unloading ALL sounding data?");
				int response = messageBox.open();
				if(response == SWT.YES) {
					NsharpResourceHandler rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();		
					rsc.deleteRscAll();
					selectedTimeList.clear();
					close();
				}
						
			}

		} );
		Button unloadSelectedBtn = new Button(buttonGp, SWT.PUSH);
		unloadSelectedBtn.setText("Unload Selected");
		unloadSelectedBtn.setEnabled( true );
		unloadSelectedBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				//System.out.println("Unload Selected");
				NsharpResourceHandler rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();				
				if(selectedTimeList.size() == sndTimeList.getItemCount()){
					rsc.deleteRscAll();
				}else {
					rsc.deleteRsc(selectedTimeList);
				}
				selectedTimeList.clear();
				close();
			}          		            	 	
		} );
	}
	private boolean checkLoadedRsc() {
		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor == null) {
            return false;
        }
        NsharpResourceHandler rsc = editor.getRscHandler();
        if (rsc == null) {
            return false;
        }
        return true;
    }
	private void createSndList(){
		if(checkLoadedRsc()== false)
			return;
		//after checking, rsc is not null guaranteed.
		NsharpResourceHandler rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();
		//List<ElementStateProperty>  timeLineElementList = rsc.getDataTimelineList();
		List<List<NsharpSoundingElementStateProperty>> stnTmTable = rsc.getStnTimeTable();
		//for(ElementStateProperty elm: timeLineElementList){
		//	sndTimeList.add(elm.getElementDescription());
		//}
		for(List<NsharpSoundingElementStateProperty> stnTmList: stnTmTable){
			for(NsharpSoundingElementStateProperty tm: stnTmList){
				if(tm.getElementState() != NsharpConstants.State.NOTAVAIL){
					sndTimeList.add(tm.getElementDescription());
				}
			}
		}
	}
}
