package gov.noaa.nws.ncep.viz.resourceManager.ui.spf;

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


public class NewSpfFileNameDialog extends Dialog {

	private String dialogTitle = "New SPF File Name"; 

    /**
     * Dialog shell.
     */
    private Shell myShell;
    
    private Composite dialogComp;

    private Text newSpfFileNameText;   
    
    private Label newSpfFileNameLabel; 
    
    private Button submitButton; 
  
    private File parentDirFile; 
    
    private String selectedSpfFilename; 
    public String getSelectedSpfFilename() {
		return selectedSpfFilename;
	}
    

	/**
     * @param parentShell
     */
    public NewSpfFileNameDialog(Shell parentShell, File _parentDirFile) {
        super(parentShell);
        this.myShell = parentShell; 
        this.parentDirFile = _parentDirFile; 
    }

    /**
     * closes the New SPF File Name Dialog
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
         * create myShell for the NewSpfFileName dialog only
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

    	createNewSpfFilenameFields(shell, textWidth, textHeight, 
    			labelWidth, labelHeight); 
    	createSubmitButton(dialogComp, textHeight); 
    }
    	
    private void createNewSpfFilenameFields(Shell shell, int textWidth, 
    		int textHeight, int labelWidth, int labelHeight) {
    	dialogComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, true);
        dialogComp.setLayout(gridLayout);

        /*
         * set up new SPF file name fields
         */
        Composite newSpfFilenameComp = new Composite(dialogComp, SWT.NONE);
        GridLayout newSpfFilenameGridLayout = new GridLayout(2, false);
        newSpfFilenameComp.setLayout(newSpfFilenameGridLayout);

        newSpfFileNameLabel = new Label(newSpfFilenameComp, SWT.NONE); 
        newSpfFileNameLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        newSpfFileNameLabel.setText("New SPF File Name:");
        newSpfFileNameText = new Text(newSpfFilenameComp, SWT.BORDER);
        newSpfFileNameText.setLayoutData( new GridData( textWidth, textHeight ) );
 
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
				doSpfFilenameValidate(); 
			}
    	}); 
    }
    
    protected void doSpfFilenameValidate() {
    	String newSPFFilenameString = null; 
    	if(newSpfFileNameText != null)
    		newSPFFilenameString = newSpfFileNameText.getText(); 
    	
    	if(!isFilenameValid(newSPFFilenameString)) {
            MessageBox mb = new MessageBox(myShell, SWT.ICON_ERROR);
            mb.setMessage("Error: Invalid SPF file name, please try again.");
            mb.setText("Error");
            mb.open();
            return;
      	} else { 
        	String savedRbdFilename = newSPFFilenameString + ".xml"; 
      		if(isFilenameAlreadyExist(parentDirFile, savedRbdFilename)) {
                MessageBox mb = new MessageBox(myShell, SWT.ICON_ERROR);
                mb.setMessage("Error: The filename "+ newSPFFilenameString + " is already used, please try a different SPF file name");
                mb.setText("Error");
                mb.open();
                return;
      		}
      		selectedSpfFilename = savedRbdFilename; 
      		close(); 
    	}
    }

	private boolean isFilenameValid(String filename) {
		boolean isFilenameValid = true; 
		if(filename == null || filename.trim().length() == 0)
			isFilenameValid = false; 
		return isFilenameValid; 
	}
	
    private boolean isFilenameAlreadyExist(File _parentDirFile, String filename) {
    	boolean isFileExist = false; 
    	String[] existFilenameArray = _parentDirFile.list();
    	for(String eachFilename : existFilenameArray) {
    		if(eachFilename.equals(filename)) {
    			isFileExist = true; 
    			break; 
    		}
    	}
    	return isFileExist; 
    }

    
}
