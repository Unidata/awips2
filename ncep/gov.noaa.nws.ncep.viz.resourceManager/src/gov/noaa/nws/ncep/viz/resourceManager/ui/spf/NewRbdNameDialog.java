package gov.noaa.nws.ncep.viz.resourceManager.ui.spf;

import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;

import java.io.File;

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


public class NewRbdNameDialog extends Dialog {

	private String dialogTitle = "New Rbd Name"; 

    /**
     * Dialog shell.
     */
    private Shell myShell;
    
    private Composite dialogComp;

    private Text newRbdNameText;   
    
    private Label newRbdNameLabel; 
    
    private Button submitButton; 
  
    private String spfGroup, spfName;// not changable, just for validation
    
    private String newRbdName; 
    
    public String getNewRbdName() {
		return newRbdName;
	}

	/**
     * @param parentShell
     */
    public NewRbdNameDialog(Shell parentShell, 
    		String gName, String sName, String origRbdName ) {
        super(parentShell);
        myShell = parentShell; 
        newRbdName = origRbdName;
        spfGroup = gName;
        spfName = sName;
    }

    /**
     * closes the New Rbd  Name Dialog
     */
    public void close() {
    	if ( myShell != null ) myShell.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    protected Object open() {
        Shell parentShell = getParent();
        Display display = parentShell.getDisplay();

        /*
         * create myShell for the NewRbdName dialog only
         */
        myShell = new Shell(parentShell, SWT.DIALOG_TRIM);

        myShell.setText(dialogTitle); 

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
    
    protected void initializeComponents(Shell shell) {
        int textWidth = 220; 
        int textHeight = 15; 
        int labelWidth = 120; 
        int labelHeight = 15; 

    	createNewRbdNameFields(shell, textWidth, textHeight, 
    			labelWidth, labelHeight); 
    	createSubmitButton(dialogComp, textHeight); 
    }
    	
    private void createNewRbdNameFields(Shell shell, int textWidth, 
    		int textHeight, int labelWidth, int labelHeight) {
    	dialogComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, true);
        dialogComp.setLayout(gridLayout);

        /*
         * set up new RBD  name fields
         */
        Composite newRbdNameComp = new Composite(dialogComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        newRbdNameComp.setLayout(gl);

        newRbdNameLabel = new Label(newRbdNameComp, SWT.NONE); 
        newRbdNameLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        newRbdNameLabel.setText("New RBD Name:");
        newRbdNameText = new Text(newRbdNameComp, SWT.BORDER);
        newRbdNameText.setLayoutData( new GridData( textWidth, textHeight ) );
 
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

    
    /**
     * Add selection listener to submitButton
     */
    private void submitButtonSelectionListenerAction(Button submitButton) {
    	submitButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				doRbdNameValidate(); 
			}
    	}); 
    }
    
    protected void doRbdNameValidate() {
    	String newRBDnameString = null; 
    	newRbdName = newRbdNameText.getText(); 
    	
    	if(!SpfsManager.getInstance().isValidRbdName( newRbdName ) ) {
            MessageBox mb = new MessageBox(myShell, SWT.ICON_ERROR);
            mb.setMessage("Invalid RBD name.");
            mb.setText("Error");
            mb.open();
            return;
      	} 
    	else { 
        	if( SpfsManager.getInstance().doesRbdExistInUserContext( spfGroup, spfName, newRbdName ) ) {        		
                MessageBox mb = new MessageBox(myShell, SWT.ICON_ERROR);
                mb.setMessage("The Rbd "+ newRBDnameString + " already used\nEnter a different RBD name.");
                mb.setText("Error");
                mb.open();
                newRbdName = null;
                return;
      		}

        	close(); 
    	}
    }
}
