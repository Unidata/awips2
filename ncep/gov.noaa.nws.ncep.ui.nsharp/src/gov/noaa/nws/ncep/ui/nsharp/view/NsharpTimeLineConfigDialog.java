package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpTimeLineConfigDialog
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

public class NsharpTimeLineConfigDialog extends Dialog {
	private static NsharpTimeLineConfigDialog INSTANCE = null;
	private   org.eclipse.swt.widgets.List timeLineList;
	private  List<String> selectedTimeList = new ArrayList<String>(); 
	protected Composite top;
	private MessageBox mb;
	protected NsharpTimeLineConfigDialog(Shell parentShell) {
		super(parentShell);
		mb = new MessageBox(parentShell, SWT.ICON_WARNING
				| SWT.OK );
		mb.setMessage( "Current time line can't be deactivated!");
	}
	public static NsharpTimeLineConfigDialog getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
			INSTANCE = new NsharpTimeLineConfigDialog( parShell );	//System.out.println("new unload dialog INSTANCE created");
			
		}
		return INSTANCE;
		
	}
	
	@Override
	public int open() {
		return super.open();
	}
	@Override
	public boolean close() {
		
		if(timeLineList!=null){
			timeLineList.removeListener(SWT.Selection, timeLineList.getListeners(SWT.Selection)[0]);
			timeLineList.dispose();
			timeLineList = null;
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
        shell.setText( "Time Line Configuration" );
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
		timeLineList = new org.eclipse.swt.widgets.List(sndTimeListGp, SWT.BORDER | SWT.MULTI| SWT.V_SCROLL  );
		timeLineList.setBounds(0,0, 2*NsharpConstants.listWidth, NsharpConstants.listHeight * 4);
		createSndList();
        //create a selection listener to handle user's selection on list		
		timeLineList.addListener ( SWT.Selection, new Listener () {
        	private String selectedSndTime=null;	
    		public void handleEvent (Event e) {   			
    			if (timeLineList.getSelectionCount() > 0 ) {  	
    				selectedTimeList.clear();
    				for(int i=0; i < timeLineList.getSelectionCount(); i++) {
    					selectedSndTime = timeLineList.getSelection()[i]; 
    					if(selectedSndTime.contains("Active-Current") == true){
    						timeLineList.deselect(timeLineList.indexOf(selectedSndTime));
    						mb.open();
    						break;
    					}
    					//remove "--InActive" or "--Active"
    					selectedSndTime= selectedSndTime.substring(0, selectedSndTime.indexOf("--"));
    					selectedTimeList.add(selectedSndTime);
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
				rsc.handleTimeLineActConfig(selectedTimeList, NsharpConstants.ActState.ACTIVE);
				
				selectedTimeList.clear();
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
				rsc.handleTimeLineActConfig(selectedTimeList, NsharpConstants.ActState.INACTIVE);
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
		List<NsharpOperationElement>  tlList = rsc.getTimeElementList();
		int curTmIndex = rsc.getCurrentTimeElementListIndex();
		for(NsharpOperationElement tl: tlList){
			String s;
			if(tl.getActionState() == NsharpConstants.ActState.INACTIVE)
				s = "--(InActive)";
			else {
				if(tlList.indexOf(tl) == curTmIndex)
					s="--(Active-Current)";
				else
					s="--(Active)";
			}
			timeLineList.add(tl.getElementDescription() +s);
		}
	}
}
