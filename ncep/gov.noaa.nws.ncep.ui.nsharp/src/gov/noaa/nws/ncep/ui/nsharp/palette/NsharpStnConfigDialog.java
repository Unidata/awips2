package gov.noaa.nws.ncep.ui.nsharp.palette;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpStnConfigDialog
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
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpStationStateProperty;

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
import org.eclipse.swt.widgets.Shell;

public class NsharpStnConfigDialog extends Dialog {
	private static NsharpStnConfigDialog INSTANCE = null;
	private   org.eclipse.swt.widgets.List stnList;
	private  List<String> selectedStnList = new ArrayList<String>(); 
	protected Composite top;
	protected NsharpStnConfigDialog(Shell parentShell) {
		super(parentShell);
	}
	public static NsharpStnConfigDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
			INSTANCE = new NsharpStnConfigDialog( parShell );
			
		}
		return INSTANCE;
		
	}
	
	@Override
	public int open() {
		return super.open();
	}
	@Override
	public boolean close() {
		
		if(stnList!=null){
			stnList.removeListener(SWT.Selection, stnList.getListeners(SWT.Selection)[0]);
			stnList.dispose();
			stnList = null;
		}
		if(selectedStnList!= null){
			selectedStnList.clear();
			selectedStnList = null;
		}
		INSTANCE = null;
		return super.close();
	}

	@Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Station Configuration" );
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
		Group stnListGp = new Group(parent,SWT.SHADOW_ETCHED_IN);
		stnList = new org.eclipse.swt.widgets.List(stnListGp, SWT.BORDER | SWT.MULTI| SWT.V_SCROLL  );
		stnList.setBounds(0,0, 2*NsharpConstants.listWidth, NsharpConstants.listHeight * 8);
		createStnList();
        //create a selection listener to handle user's selection on list		
		stnList.addListener ( SWT.Selection, new Listener () {
        	private String selectedSndTime=null;	
    		public void handleEvent (Event e) {   			
    			if (stnList.getSelectionCount() > 0 ) {  	
    				selectedStnList.clear();
    				for(int i=0; i < stnList.getSelectionCount(); i++) {
    					selectedSndTime = stnList.getSelection()[i];
    					//remove "--InActive" or "--Active" from string
    					selectedSndTime= selectedSndTime.substring(0, selectedSndTime.indexOf('-'));
    					selectedStnList.add(selectedSndTime);
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
				NsharpSkewTResource rsc = NsharpSkewTEditor.getActiveNsharpEditor().getNsharpSkewTDescriptor().getSkewtResource();			
				rsc.handleStationActConfig(selectedStnList, NsharpSkewTResource.State.ACTIVE);
				selectedStnList.clear();
				close();
			}

		} );
		Button deactivateBtn = new Button(buttonGp, SWT.PUSH);
		deactivateBtn.setText("DeActivate");
		deactivateBtn.setEnabled( true );
		deactivateBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				//System.out.println("Unload Selected");
				NsharpSkewTResource rsc = NsharpSkewTEditor.getActiveNsharpEditor().getNsharpSkewTDescriptor().getSkewtResource();				
				rsc.handleStationActConfig(selectedStnList, NsharpSkewTResource.State.INACTIVE);
				selectedStnList.clear();
				close();
			}          		            	 	
		} );
	}
	private boolean checkLoadedRsc() {
        NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
        if (editor == null) {
            return false;
        }
        NsharpSkewTResource rsc = editor.getNsharpSkewTDescriptor()
                .getSkewtResource();
        if (rsc == null) {
            return false;
        }
        return true;
    }
	private void createStnList(){
		if(checkLoadedRsc()== false)
			return;
		//after checking, rsc is not null guaranteed.
		NsharpSkewTResource rsc = NsharpSkewTEditor.getActiveNsharpEditor().getNsharpSkewTDescriptor().getSkewtResource();
		List<NsharpStationStateProperty>  stnStList = rsc.getStnStateList();
		for(NsharpStationStateProperty stn: stnStList){
			String s;
			if(stn.getStnState() == NsharpSkewTResource.State.INACTIVE)
				s = "--(InActive)";
			else
				s="--(Active)";
			stnList.add(stn.getStnDescription() +s);
		}
	}
}
