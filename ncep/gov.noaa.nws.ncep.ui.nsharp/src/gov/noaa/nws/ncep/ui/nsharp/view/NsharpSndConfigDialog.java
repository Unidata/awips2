package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpSndConfigDialog
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpOperationElement;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import java.util.ArrayList;
import java.util.List;

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

public class NsharpSndConfigDialog extends Dialog {
	private static NsharpSndConfigDialog INSTANCE = null;
	private   org.eclipse.swt.widgets.List sndList;
	private  List<String> selectedsndList = new ArrayList<String>(); 
	protected Composite top;
	private MessageBox mb;
	protected NsharpSndConfigDialog(Shell parentShell) {
		super(parentShell);
		mb = new MessageBox(parentShell, SWT.ICON_WARNING
				| SWT.OK );
		mb.setMessage( "Current sounding type can't be deactivated!");
	}
	public static NsharpSndConfigDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
			INSTANCE = new NsharpSndConfigDialog( parShell );
			
		}
		return INSTANCE;
		
	}
	
	@Override
	public int open() {
		return super.open();
	}
	@Override
	public boolean close() {
		
		if(sndList!=null){
			sndList.removeListener(SWT.Selection, sndList.getListeners(SWT.Selection)[0]);
			sndList.dispose();
			sndList = null;
		}
		if(selectedsndList!= null){
			selectedsndList.clear();
			selectedsndList = null;
		}
		INSTANCE = null;
		return super.close();
	}

	@Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Sounding Configuration" );
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
		Group sndListGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		sndList = new org.eclipse.swt.widgets.List(sndListGp, SWT.BORDER | SWT.MULTI| SWT.V_SCROLL  );
		sndList.setBounds(0,0, 2*NsharpConstants.listWidth, NsharpConstants.listHeight * 8);
		createsndList();
        //create a selection listener to handle user's selection on list		
		sndList.addListener ( SWT.Selection, new Listener () {
        	private String selectedSndType=null;	
    		public void handleEvent (Event e) {   			
    			if (sndList.getSelectionCount() > 0 ) {  	
    				selectedsndList.clear();
    				for(int i=0; i < sndList.getSelectionCount(); i++) {
    					selectedSndType = sndList.getSelection()[i];
    					if(selectedSndType.contains("Active-Current") == true){
    						sndList.deselect(sndList.indexOf(selectedSndType));
    						mb.open();
    						break;
    					}
    					//remove "--InActive" or "--Active" from string
    					selectedSndType= selectedSndType.substring(0, selectedSndType.indexOf('-'));
    					selectedsndList.add(selectedSndType);
    				}
    				
    			}
    		}
    	});
		
		Group buttonGp = new Group(parent,SWT.SHADOW_OUT);
		buttonGp.setLayout( new GridLayout(2, false) );

		Button activateBtn = new Button(buttonGp, SWT.PUSH);
		activateBtn.setText("Activate");
		activateBtn.setEnabled( true );
		activateBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {   
				NsharpResourceHandler rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();			
				rsc.handleSndTypeActConfig(selectedsndList, NsharpConstants.ActState.ACTIVE);
				selectedsndList.clear();
				close();
			}

		} );
		Button deactivateBtn = new Button(buttonGp, SWT.PUSH);
		deactivateBtn.setText("DeActivate");
		deactivateBtn.setEnabled( true );
		deactivateBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				//System.out.println("Unload Selected");
				NsharpResourceHandler rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();				
				rsc.handleSndTypeActConfig(selectedsndList, NsharpConstants.ActState.INACTIVE);
				selectedsndList.clear();
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
	private void createsndList(){
		if(checkLoadedRsc()== false)
			return;
		//after checking, rsc is not null guaranteed.
		NsharpResourceHandler rsc = NsharpEditor.getActiveNsharpEditor().getRscHandler();
		List<NsharpOperationElement>  sndTypeList = rsc.getSndElementList();
		int curStnIndex = rsc.getCurrentSndElementListIndex();
		for(NsharpOperationElement snd: sndTypeList){
			String s;
			if(snd.getActionState() == NsharpConstants.ActState.INACTIVE)
				s = "--(InActive)";
			else{
				if(sndTypeList.indexOf(snd)== curStnIndex)
					s="--(Active-Current)";
				else
					s="--(Active)";
			}
			sndList.add(snd.getElementDescription() +s);
		}
	}
}
