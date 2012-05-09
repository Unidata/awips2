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
	private Button graphBtn, lineBtn;
	private static NsharpConfigDialog thisDialog=null;
	private static NsharpGraphConfigDialog graphDialog = null;
	private static NsharpLineConfigDialog lineDialog = null;
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
		
		graphBtn = new Button(parent, SWT.PUSH);
		graphBtn.setText("Parameters Selection");
		graphBtn.setEnabled( true );
		//graphBtn.setSize(btnWidth,pushbtnHeight);
		graphBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	
				graphDialog = NsharpGraphConfigDialog.getInstance(shell);

				if ( graphDialog != null ) {
					
					lineBtn.setEnabled(false);
					graphDialog.open();
					lineBtn.setEnabled(true);
				}	
			}          		            	 	
		} );
		lineBtn = new Button(parent, SWT.PUSH);
		lineBtn.setText("Data Display");
		lineBtn.setEnabled( true );
		//lineBtn.setSize(btnWidth,pushbtnHeight);
		lineBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  	

				lineDialog =  NsharpLineConfigDialog.getInstance(shell);

				if ( lineDialog != null ) {
					
					graphBtn.setEnabled(false);
					lineDialog.open();
					graphBtn.setEnabled(true);
				}	
			}          		            	 	
		} );
	}
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, false);
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
        shell.setSize(250, 200);
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
