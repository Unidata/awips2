package gov.noaa.nws.ncep.viz.localization.ui.dialog;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.ISourceProviderService;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import gov.noaa.nws.ncep.viz.localization.admin.AdminLoginStateSourceProvider;
import gov.noaa.nws.ncep.viz.localization.service.IUserAuthenticationService;
import gov.noaa.nws.ncep.viz.localization.service.UserAuthenticationServiceImpl;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * September 21, 2010              M. Gao	   Init User Login Dislog
 * 
 * </pre>
 * 
 * @author mgao
 * @version 1.0
 */

public class AdminLoginDialog extends CaveJFACEDialog {

	private String dialogTitle = "Admin User Login"; 

    /**
     * Dialog shell.
     */
    private Shell shell;
    
    private Composite dlgComp;

    private Text passwordText, userNameText;  //, deskText, siteText; 
    
    private Label passwordLabel, userNameLabel; //, deskLabel, siteLabel;
    
    private Composite userFieldComp; 
  
    private IUserAuthenticationService userAuthenticationService; 
    
    private boolean dialogOpen; 
//    private boolean cancelPressed; 

	/**
     * @param parentShell
     */
    public AdminLoginDialog(Shell parentShell) {
        super(parentShell);
        this.shell = parentShell; 
        
        userAuthenticationService = new UserAuthenticationServiceImpl(); 
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
    	setDialogTitleText(dialogTitle); 

    	dlgComp = (Composite) super.createDialogArea(parent);
    	dlgComp.setLayoutData( new GridData( 450, 200 ) );

        Group userInputGroup = new Group(dlgComp, SWT.BORDER);
        userInputGroup.setText("GEMPAK");   //setText("Graphic Area & Projection");
        Layout layout = new GridLayout(1, true);
        userInputGroup.setLayout(layout);
        userInputGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        userFieldComp = new Composite(userInputGroup, SWT.NONE);
        layout = new GridLayout(2, false);
        userFieldComp.setLayout(layout);

        int textWidth = 260; 
        int textHeight = 15; 
        int labelWidth = 75; 
        int labelHeight = 15; 

        userNameLabel = new Label(userFieldComp, SWT.NONE); 
        userNameLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        userNameLabel.setText("User Name:");
        userNameText = new Text(userFieldComp, SWT.BORDER);
        userNameText.setLayoutData( new GridData( textWidth, textHeight ) );
        
        passwordLabel = new Label(userFieldComp, SWT.NONE); 
        passwordLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        passwordLabel.setText("Password:");
        passwordText = new Text(userFieldComp, SWT.BORDER | SWT.PASSWORD);
        passwordText.setLayoutData( new GridData( textWidth, textHeight ) );

//        deskLabel = new Label(userFieldComp, SWT.NONE); 
//        deskLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
//        deskLabel.setText("Desk:");
//        deskText = new Text(userFieldComp, SWT.BORDER);
//        deskText.setLayoutData( new GridData( textWidth, textHeight ) );
//
//        siteLabel = new Label(userFieldComp, SWT.NONE); 
//        siteLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
//        siteLabel.setText("Site:");
//        siteText = new Text(userFieldComp, SWT.BORDER);
//        siteText.setLayoutData( new GridData( textWidth, textHeight ) );

        Composite applyButtonComp = new Composite(userInputGroup, SWT.NONE);
        layout = new GridLayout(2, false);
        applyButtonComp.setLayout(layout);

        applyDialogFont(dlgComp);
        return dlgComp;
    }

    
    /**
     * Helper method to set the dialog window title
     */
	private void setDialogTitleText(String dialogTitleText) {
		Shell shell = getShell(); 
		shell.setText(dialogTitleText); 
	}

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void okPressed() {
    	if(userNameText == null)
    		System.out.println("@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++++++++================================ userNameText is NULL!!!!!!!!!!!!!!!!!!1"); 
    	if(passwordText == null)
    		System.out.println("@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++++++++================================ passwordText is NULL!!!!!!!!!!!!!!!!!!1"); 
    	String userNameString = userNameText.getText(); 
    	String passwordString = passwordText.getText(); 
//    	String deskString = deskText.getText(); 
//    	String siteString = siteText.getText(); 
    	
    	if(!userAuthenticationService.isUserValid(userNameString, passwordString)) {
          MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
          mb.setMessage("Error: Invalid username and password, please try again.");
          mb.setText("Error");
          mb.open();
          return;
    	}

    	changeAdminLoginStatus(true); 
    	
//    	UserLoginStatusManager.getInstance().setUserLogin(true);
//    	UserLoginStatusManager.getInstance().setCurrentUserName(userNameString); 
//    	UserLoginStatusManager.getInstance().setCurrentDesk(deskString); 
//    	UserLoginStatusManager.getInstance().setCurrentSite(siteString); 
    	this.dialogOpen = true; 
//    	this.cancelPressed = false; 
    	
        super.okPressed();
    }

    @Override
	protected void createButtonsForButtonBar(Composite parent) {
		// create OK and Cancel buttons by default
		createButton(parent, IDialogConstants.OK_ID, "Submit",
				true);
//		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
//				true);
//		createButton(parent, IDialogConstants.CANCEL_ID,
//				IDialogConstants.CANCEL_LABEL, false);
	}

    protected void cancelPressed() {
    	this.dialogOpen = false; 
//    	this.cancelPressed = true; 
    	super.cancelPressed();  
    }
    
    /**
     * a helper method to check if the dialog is still open
     */
	public boolean isDialogOpen() {
		return dialogOpen;
	}

	public void setDialogOpen(boolean dialogOpen) {
		this.dialogOpen = dialogOpen;
	}

//    public boolean isOpen() {
//    	return dialogOpen; 
//    }
    
//	public boolean isCancelPressed() {
//		return cancelPressed;
//	}

	private void changeAdminLoginStatus(boolean _isLoggedIn) {
        IWorkbenchWindow workbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        ISourceProviderService service = (ISourceProviderService) workbenchWindow.getService(ISourceProviderService.class);

        AdminLoginStateSourceProvider adminLoginStateSourceProvider = (AdminLoginStateSourceProvider)service.getSourceProvider(AdminLoginStateSourceProvider.ADMIN_LOGIN_STATE);
        /*
         * update the source provider
         */
        adminLoginStateSourceProvider.setAdminLoggedInState(_isLoggedIn); 

	}

}
