package gov.noaa.nws.ncep.viz.localization.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import gov.noaa.nws.ncep.viz.localization.Activator;
import gov.noaa.nws.ncep.viz.localization.resource.synchronization.ResourceUploadingUtil;
import gov.noaa.nws.ncep.viz.localization.service.IUserAuthenticationService;
import gov.noaa.nws.ncep.viz.localization.service.UserAuthenticationServiceImpl;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * September 30, 2010              M. Gao	   Init User Login Dislog
 * 
 * </pre>
 * 
 * @author mgao
 * @version 1.0
 */

public class SyncFileToolDialog extends Dialog {
	
	private String dialogTitle = "File Synchronization Tool"; 

    /**
     * Dialog shell.
     */
    private Shell myShell;
    
    private Composite dialogComp;

    private Text passwordText, userNameText;  
    
    private Label passwordLabel, userNameLabel; 
    
    private Button submitButton, syncSiteFileOnlyButton, syncDeskFileOnlyButton, syncFileDoneButton, cancelButton; 
    
    private IUserAuthenticationService userAuthenticationService; 
    
	/**
     * @param parentShell
     */
    public SyncFileToolDialog(Shell parentShell) {
        super(parentShell);
        
        userAuthenticationService = new UserAuthenticationServiceImpl(); 
    }

    /**
     * closes the Synchronization File Tool Dialog
     */
    public void close() {
    	if ( myShell != null ) myShell.dispose();
    }

    /**
     * Open method to display the File Synchronization dialog.
     * 
     * @return Return object (can be null).
     */
    public Object open() {
        Shell parentShell = getParent();
        Display display = parentShell.getDisplay();

        /*
         * create myShell for the file synchronization dialog only
         */
        myShell = new Shell(parentShell, SWT.DIALOG_TRIM);
//        myShell.setSize(500, 280); 
        String titleString = constructDialogTitle(dialogTitle); 
        myShell.setText(titleString);

        /*
         * Start to Create the main layout for the shell.
         */
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        myShell.setLayout(mainLayout);
        myShell.setLocation(450, 450);

        /*
         * Initialize all of the controls and layouts
         */
        initializeComponents(myShell);
    
        myShell.pack();

        myShell.open();
        while (!myShell.isDisposed()) {
            if (!display.readAndDispatch()) {
            	display.sleep();
            }
        }

        return null;
    }
    
    private String constructDialogTitle(String dialogTitlePrefix) {
    	StringBuilder builder = new StringBuilder(dialogTitlePrefix.length() * 3);
    	String currentSite = Activator.getDefault().getCurrentSite(); 
    	String currentDesk = Activator.getDefault().getCurrentDesk(); 
//    	builder.append(dialogTitlePrefix)
//    		   .append("\n")
    	builder.append("Active Site: ")
    		   .append(currentSite)
    		   .append(",  Active Desk: ")
    		   .append(currentDesk); 
    	builder.trimToSize(); 
    	String finalTitleValue = builder.toString(); 
    	return finalTitleValue; 
    }
    
    protected void initializeComponents(Shell shell) {
        int textWidth = 260; 
        int textHeight = 15; 
        int labelWidth = 75; 
        int labelHeight = 15; 

    	createUsernameAndPasswordFields(shell, textWidth, textHeight, 
    			labelWidth, labelHeight); 
    	createSubmitButton(dialogComp, textHeight); 
    	createSyncExecutionButton(dialogComp, textHeight); 
    	createDoneAndCancelButton(dialogComp, textHeight); 
    }

    private void createUsernameAndPasswordFields(Shell shell, int textWidth, 
    		int textHeight, int labelWidth, int labelHeight) {
    	dialogComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, true);
        dialogComp.setLayout(gridLayout);
        
        /*
         * set up user name and password fields
         */
        Composite usernamePasswordComp = new Composite(dialogComp, SWT.NONE);
        GridLayout usernamePasswordGridLayout = new GridLayout(2, false);
        usernamePasswordComp.setLayout(usernamePasswordGridLayout);

        userNameLabel = new Label(usernamePasswordComp, SWT.NONE); 
        userNameLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        userNameLabel.setText("User Name:");
        userNameText = new Text(usernamePasswordComp, SWT.BORDER);
        userNameText.setLayoutData( new GridData( textWidth, textHeight ) );
        
        passwordLabel = new Label(usernamePasswordComp, SWT.NONE); 
        passwordLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        passwordLabel.setText("Password:");
        passwordText = new Text(usernamePasswordComp, SWT.BORDER | SWT.PASSWORD);
        passwordText.setLayoutData( new GridData( textWidth, textHeight ) );
    }
    
    private void createSubmitButton(Composite parentComposite, int textHeight) { 
    	Composite submitButtonComp = new Composite(parentComposite, SWT.NONE);
        Layout layout = new GridLayout(1, false);
        submitButtonComp.setLayout(layout); 
        submitButton = new Button(submitButtonComp, SWT.NONE); 
        submitButton.setText("Submit");
        submitButton.setLayoutData(new GridData(100, textHeight + 15)); 
        submitButtonSelectionListenerAction(submitButton); 
    }

    private void createSyncExecutionButton(Composite parentComposite, int textHeight) {
    	Composite syncExecutionButtonComp = new Composite(parentComposite, SWT.NONE);
        Layout layout = new GridLayout(1, false);
        syncExecutionButtonComp.setLayout(layout);

        syncSiteFileOnlyButton = new Button(syncExecutionButtonComp, SWT.NONE); 
    	String currentSite = Activator.getDefault().getCurrentSite(); 
        String siteButtonText = "Start File Sync for active site '" + currentSite + "'";  
        syncSiteFileOnlyButton.setText(siteButtonText); 
        syncSiteFileOnlyButton.setLayoutData( new GridData( 350, textHeight+15 ) );
        syncSiteFileOnlyButtonSelectionListenerAction(syncSiteFileOnlyButton); 
        syncSiteFileOnlyButton.setEnabled(false);  
        
        syncDeskFileOnlyButton = new Button(syncExecutionButtonComp, SWT.NONE); 
    	String currentDesk = Activator.getDefault().getCurrentDesk(); 
        String deskButtonText = "Start File Sync for active desk '" + currentDesk + "'";  
        syncDeskFileOnlyButton.setText(deskButtonText); 
        syncDeskFileOnlyButton.setLayoutData( new GridData( 350, textHeight+15 ) );
        syncDeskFileOnlyButtonSelectionListenerAction(syncDeskFileOnlyButton); 
        syncDeskFileOnlyButton.setEnabled(false);  
        
    }

    private void createDoneAndCancelButton(Composite parentComposite, int textHeight) {
    	Composite syncDoneAndCancelButtonComp = new Composite(parentComposite, SWT.NONE);
        Layout layout = new GridLayout(2, false);
        syncDoneAndCancelButtonComp.setLayout(layout);

        syncFileDoneButton = new Button(syncDoneAndCancelButtonComp, SWT.NONE); 
        syncFileDoneButton.setText("Done"); 
        syncFileDoneButton.setLayoutData( new GridData( 50, textHeight+15 ) );
        syncDoneAndCancelButtonSelectionListenerAction(syncFileDoneButton); 
        syncFileDoneButton.setEnabled(false);  
    	
        cancelButton = new Button(syncDoneAndCancelButtonComp, SWT.NONE); 
        cancelButton.setText("Cancel"); 
        cancelButton.setLayoutData( new GridData( 80, textHeight+15 ) );
        syncDoneAndCancelButtonSelectionListenerAction(cancelButton); 
   }

    /**
     * Add selection listener to submitButton
     */
    private void submitButtonSelectionListenerAction(Button submitButton) {
    	submitButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				doUserLogin(); 
			}
    	}); 
    }
    
	/**
	 * This method check username/password, if the login is correct, 
	 * the syncExecutionButton and close/done button will be enabled
	 */
	private void doUserLogin() {
		String userNameString = userNameText.getText(); 
		String passwordString = passwordText.getText(); 

		if(!userAuthenticationService.isUserValid(userNameString, passwordString)) {

			MessageBox messageBox = new MessageBox(new Shell(), SWT.ICON_ERROR | SWT.OK);
			messageBox.setMessage("Error: Invalid username and password, please try again.");
			messageBox.setText("Error");
			messageBox.open();
			return;
		}

		/*
		 * Login successes, now enable the two buttons below
		 */
		syncSiteFileOnlyButton.setEnabled(true);  
		syncDeskFileOnlyButton.setEnabled(true);  
		syncFileDoneButton.setEnabled(true);  
	}

    /**
     * Add selection listener to syncSiteFileOnlyButton
     */
    private void syncSiteFileOnlyButtonSelectionListenerAction(Button syncSiteFileOnlyButton) {
    	syncSiteFileOnlyButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				doUploadingSiteFileOnly(); 
			}
    	}); 
    }

    /**
     * Add selection listener to syncDeskFileOnlyButton
     */
    private void syncDeskFileOnlyButtonSelectionListenerAction(Button syncDeskFileOnlyButton) {
    	syncDeskFileOnlyButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				doUploadingDeskFileOnly(); 
			}
    	}); 
    }

	/**
	 * method to test synchronizing local files of the active site back to server side 
	 */
	private void doUploadingSiteFileOnly() {
		ResourceUploadingUtil uploadingHandler = ResourceUploadingUtil.getInstance(); 
		uploadingHandler.uploadActiveSiteOnly();  
	}

	/**
	 * method to test synchronizing local files of the active desk back to server side 
	 */
	private void doUploadingDeskFileOnly() {
		ResourceUploadingUtil uploadingHandler = ResourceUploadingUtil.getInstance(); 
		uploadingHandler.uploadActiveDeskOnly();  
	}

    /**
     * Add selection listener to syncDoneButton
     */
    private void syncDoneAndCancelButtonSelectionListenerAction(Button syncDoneOrCancelButton) {
    	syncDoneOrCancelButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				close(); 
			}
    	}); 
    }

    /**
     * a helper method to check if the dialog is still open
     */
	public boolean isDialogOpen() {
		return myShell != null && !myShell.isDisposed();
	}

}
