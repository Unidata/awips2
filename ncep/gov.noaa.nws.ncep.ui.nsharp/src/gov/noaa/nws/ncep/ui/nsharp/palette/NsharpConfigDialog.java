package gov.noaa.nws.ncep.ui.nsharp.palette;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpConfigDialog
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/21/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */


import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;


public class NsharpConfigDialog extends Dialog {
	private Button parameterBtn, dataDisplayBtn, timeLineBtn, stnBtn;
	private static NsharpConfigDialog thisDialog=null;
	private static NsharpParametersSelectionConfigDialog parameterSelDialog = null;
	private static NsharpDataDisplayConfigDialog dataDislpayDialog = null;
	private static NsharpTimeLineConfigDialog timelineDialog = null;
	private static NsharpStnConfigDialog stnDialog = null;
	public NsharpConfigDialog(Shell parentShell) {
		super(parentShell);
		// TODO Auto-generated constructor stub
	}

	public NsharpConfigDialog(IShellProvider parentShell) {
		super(parentShell);
		// TODO Auto-generated constructor stub
	}
	public static NsharpConfigDialog getInstance( Shell parShell){

		if ( thisDialog == null ){
			thisDialog = new NsharpConfigDialog( parShell );
		}

		return thisDialog;

	}
	
	public void createDialogContents(Composite parent){
		parameterBtn = new Button(parent, SWT.PUSH);
		parameterBtn.setText("Parameters Selection");
		parameterBtn.setEnabled( true );
		//graphBtn.setSize(btnWidth,pushbtnHeight);
		parameterBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           				
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				parameterSelDialog = NsharpParametersSelectionConfigDialog.getInstance(shell);
				if ( parameterSelDialog != null ) {
					timeLineBtn.setEnabled( false );
					dataDisplayBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					parameterSelDialog.open();
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					stnBtn.setEnabled( true );
				}	
			}          		            	 	
		} );
		dataDisplayBtn = new Button(parent, SWT.PUSH);
		dataDisplayBtn.setText("Data Display");
		dataDisplayBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		dataDisplayBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				dataDislpayDialog =  NsharpDataDisplayConfigDialog.getInstance(shell);
				if ( dataDislpayDialog != null ) {
					timeLineBtn.setEnabled( false );
					parameterBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					dataDislpayDialog.open();
					parameterBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					stnBtn.setEnabled( true );
				}	
			}          		            	 	
		} );
		timeLineBtn = new Button(parent, SWT.PUSH);
		timeLineBtn.setText("Time Line");
		timeLineBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		timeLineBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				timelineDialog =  NsharpTimeLineConfigDialog.getInstance(shell);
				if ( timelineDialog != null ) {
					dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					timelineDialog.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					stnBtn.setEnabled( true );
				}	
			}          		            	 	
		} );
		stnBtn = new Button(parent, SWT.PUSH);
		stnBtn.setText("Station");
		stnBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		stnBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				stnDialog =  NsharpStnConfigDialog.getInstance(shell);
				if ( stnDialog != null ) {
					dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					timeLineBtn.setEnabled( false );
					stnDialog.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
				}	
			}          		            	 	
		} );
	}
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		createDialogContents(top);

		return top;
	}   

	@Override
	public void createButtonsForButtonBar(Composite parent) {
		Button closeBtn = createButton(parent, IDialogConstants.CLOSE_ID, IDialogConstants.CLOSE_LABEL,
				true);
		closeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				//System.out.println("OK listener is called");
				//NsharpSkewTResource skewtRsc = NsharpSkewTEditor.getActiveNsharpEditor().getNsharpSkewTDescriptor().getSkewtResource();
				close();
			}          		            	 	
		} );  

	}
	@Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Nsharp Configuration" );
        shell.setSize(250, 250);
    }
	@Override
    public int open( ) {
        if ( this.getShell() == null ){
			this.create();
		}
   	    this.getShell().setLocation(this.getShell().getParent().getLocation().x+1100,
   	    		this.getShell().getParent().getLocation().y+200);
   	    return super.open();
    	
    }
}
