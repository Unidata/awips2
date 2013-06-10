package gov.noaa.nws.ncep.ui.nsharp.view;
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
	private Button parameterBtn, dataDisplayBtn,dataPageBtn, timeLineBtn, stnBtn, paneCfgBtn, mdlCfgBtn;
	private static NsharpConfigDialog thisDialog=null;
	private static NsharpParametersSelectionConfigDialog parameterSelDialog = null;
	private static NsharpDataDisplayConfigDialog dataDislpayDialog = null;
	private static NsharpDataPageConfigDialog dataPageDialog = null;
	private static NsharpTimeLineConfigDialog timelineDialog = null;
	private static NsharpStnConfigDialog stnDialog = null;
	private static NsharpPaneConfigDialog paneCfgDialog = null;
	private static NsharpGribModelTypeConfigDialog mdlCfgDialog = null;
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
					parameterSelDialog.open();
					/*timeLineBtn.setEnabled( false );
					dataDisplayBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					paneCfgBtn.setEnabled( false );
					mdlCfgBtn.setEnabled( false );
					parameterSelDialog.open();
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					stnBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					paneCfgBtn.setEnabled( true );
					mdlCfgBtn.setEnabled( true );*/
				}	
			}          		            	 	
		} );
		dataDisplayBtn = new Button(parent, SWT.PUSH);
		dataDisplayBtn.setText("Data Display Configuration");
		dataDisplayBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		dataDisplayBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				dataDislpayDialog =  NsharpDataDisplayConfigDialog.getInstance(shell);
				if ( dataDislpayDialog != null ) {
					dataDislpayDialog.open();
					/*timeLineBtn.setEnabled( false );
					parameterBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					paneCfgBtn.setEnabled( false );
					mdlCfgBtn.setEnabled( false );
					dataDislpayDialog.open();
					parameterBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					stnBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					paneCfgBtn.setEnabled( true );
					mdlCfgBtn.setEnabled( true );*/
				}	
			}          		            	 	
		} );
		
		dataPageBtn = new Button(parent, SWT.PUSH);
		dataPageBtn.setText("Data Page Configuration");
		dataPageBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		dataPageBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				dataPageDialog =  NsharpDataPageConfigDialog.getInstance(shell);
				if ( dataPageDialog != null ) {
					/*timeLineBtn.setEnabled( false );
					parameterBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					dataDisplayBtn.setEnabled(false);
					paneCfgBtn.setEnabled( false );
					mdlCfgBtn.setEnabled( false );
					dataPageDialog.open();
					parameterBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					stnBtn.setEnabled( true );
					dataDisplayBtn.setEnabled(true);
					paneCfgBtn.setEnabled( true );
					mdlCfgBtn.setEnabled( true );*/
					dataPageDialog.open();
				}	
			}          		            	 	
		} );
		timeLineBtn = new Button(parent, SWT.PUSH);
		timeLineBtn.setText("Time Line Activation");
		timeLineBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		timeLineBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				timelineDialog =  NsharpTimeLineConfigDialog.getInstance(shell);
				if ( timelineDialog != null ) {
					timelineDialog.open();
					/*dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					stnBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					paneCfgBtn.setEnabled( false );
					mdlCfgBtn.setEnabled( false );
					timelineDialog.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					stnBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					paneCfgBtn.setEnabled( true );
					mdlCfgBtn.setEnabled( true );*/
				}	
			}          		            	 	
		} );
		stnBtn = new Button(parent, SWT.PUSH);
		stnBtn.setText("Station Activation");
		stnBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		stnBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				stnDialog =  NsharpStnConfigDialog.getInstance(shell);
				if ( stnDialog != null ) {
					stnDialog.open();
					/*dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					timeLineBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					paneCfgBtn.setEnabled( false );
					mdlCfgBtn.setEnabled( false );
					stnDialog.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					paneCfgBtn.setEnabled( true );
					mdlCfgBtn.setEnabled( true );*/
				}	
			}          		            	 	
		} );
		
		paneCfgBtn = new Button(parent,  SWT.PUSH);
		paneCfgBtn.setText("Display Pane Configuration");
		paneCfgBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		paneCfgBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				paneCfgDialog =  NsharpPaneConfigDialog.getInstance(shell);
				if ( paneCfgDialog != null ) {
					paneCfgDialog.open();
				}
					/*dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					timeLineBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					stnBtn.setEnabled( false );
					mdlCfgBtn.setEnabled( false );
					paneCfgDialog.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					stnBtn.setEnabled( true );
					mdlCfgBtn.setEnabled( true );
					*/
			}          		            	 	
		} );
		
		mdlCfgBtn = new Button(parent,  SWT.PUSH);
		mdlCfgBtn.setText("Grid Model Type Configuration");
		mdlCfgBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		mdlCfgBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				mdlCfgDialog =  NsharpGribModelTypeConfigDialog.getInstance(shell);
				if ( mdlCfgDialog != null ) {
					mdlCfgDialog.open();
					/*dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					timeLineBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					stnBtn.setEnabled( false );
					paneCfgBtn.setEnabled( false );
					mdlCfgDialog.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					stnBtn.setEnabled( true );
					paneCfgBtn.setEnabled(true);*/
				}	
			}          		            	 	
		} );
		
		Button windbarbCfgBtn = new Button(parent,  SWT.PUSH);
		windbarbCfgBtn.setText("Wind Barb Configuration");
		windbarbCfgBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		windbarbCfgBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				NsharpWindBarbConfigDialog windBarbDlg= NsharpWindBarbConfigDialog.getInstance(shell);
				
				if ( windBarbDlg != null ) {
					windBarbDlg.open();
					/*dataDisplayBtn.setEnabled(false);
					parameterBtn.setEnabled(false);
					timeLineBtn.setEnabled( false );
					dataPageBtn.setEnabled( false );
					stnBtn.setEnabled( false );
					paneCfgBtn.setEnabled( false );
					windBarbDlg.open();
					parameterBtn.setEnabled(true);
					dataDisplayBtn.setEnabled(true);
					timeLineBtn.setEnabled( true );
					dataPageBtn.setEnabled( true );
					stnBtn.setEnabled( true );
					paneCfgBtn.setEnabled(true);*/
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
        shell.setSize(250, 370);
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
