package gov.noaa.nws.ncep.viz.resourceManager.ui.manageSpf;

import gov.noaa.nws.ncep.viz.resources.manager.AbstractRBD;
import gov.noaa.nws.ncep.viz.resources.manager.NcMapRBD;
import gov.noaa.nws.ncep.viz.resources.manager.SpfsManager;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *                                       Created
 * 02/22/2013     #972       G. Hull     AbstractRBD
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class RenameRbdDialog extends Dialog {

	private String dialogTitle = "Rename Rbd"; 
    private Shell myShell;    
    private Text newRbdNameText;
    
    private AbstractRBD<?> rbdToRename;
    
    public RenameRbdDialog(Shell parentShell, AbstractRBD<?> rbd ) {
        super(parentShell);
        myShell = parentShell; 
        rbdToRename = rbd;
    }

    protected Object open() {
        Shell parentShell = getParent();
        Display display = parentShell.getDisplay();

        myShell = new Shell(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL );

        myShell.setText(dialogTitle); 

        /*
         * Start to Create the main layout for the shell.
         */
        myShell.setLayout( new FormLayout() );
        myShell.setLocation( parentShell.getLocation().x+100, 
        					 parentShell.getLocation().y+300 ); 

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

        Composite topComp = new Composite( shell, SWT.NONE );
        topComp.setLayout(new FormLayout() );

        FormData fd = new FormData( 350, 160 );
        fd.top = new FormAttachment( 0, 0 );
        fd.left  = new FormAttachment( 0, 0 );
        fd.right = new FormAttachment( 100, 0 );
        fd.bottom = new FormAttachment( 100, 0 );
        topComp.setLayoutData( fd );
        
        newRbdNameText = new Text(topComp, SWT.BORDER );
        newRbdNameText.setText( rbdToRename.getRbdName() );
        newRbdNameText.setSelection(0, rbdToRename.getRbdName().length() );
        
        fd = new FormData();
        fd.top = new FormAttachment( 0, 50);
        fd.left  = new FormAttachment( 0, 25 );
        fd.right = new FormAttachment( 100, -25 );

        newRbdNameText.setLayoutData( fd );

        Label newRbdNameLabel = new Label(topComp, SWT.NONE); 
        newRbdNameLabel.setText("Enter New RBD Name:");

        fd = new FormData();
        fd.bottom = new FormAttachment( newRbdNameText, -3, SWT.TOP );
        fd.left  = new FormAttachment( newRbdNameText, 0, SWT.LEFT );
        newRbdNameLabel.setLayoutData( fd );

        Label sep = new Label( topComp, SWT.SEPARATOR | SWT.HORIZONTAL );
       	fd = new FormData();
        fd.top  = new FormAttachment( newRbdNameText, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 5 );
        fd.right  = new FormAttachment( 100, -5 );
        
        sep.setLayoutData( fd );

        final Button renameRbdBtn = new Button(topComp, SWT.NONE); 
        renameRbdBtn.setText(" Rename ");
        
        fd = new FormData();
        fd.bottom = new FormAttachment( 100, -10 );
        fd.right = new FormAttachment( 100, -20 );

        renameRbdBtn.setLayoutData( fd ); 
        
        Button cancelBtn = new Button(topComp, SWT.NONE); 
        cancelBtn.setText(" Cancel ");
        
        fd = new FormData();
        fd.top = new FormAttachment( renameRbdBtn, 0, SWT.TOP );
        fd.right = new FormAttachment( renameRbdBtn, -20, SWT.LEFT );

        cancelBtn.setLayoutData( fd ); 

        // TODO : should we allow spaces?
        newRbdNameText.addVerifyListener( new VerifyListener() {			
			@Override
			public void verifyText(VerifyEvent e) {
				if( e.text.contains( File.separator ) ) {
					e.doit = false;
				}
			}
		});
        
        newRbdNameText.addModifyListener( new ModifyListener() {			
			@Override
			public void modifyText(ModifyEvent e) {
				renameRbdBtn.setEnabled( 
							!rbdToRename.getRbdName().equals( newRbdNameText.getText().trim() ) );
			}
		});
        
        newRbdNameText.addSelectionListener(new SelectionAdapter() {			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				renameRbd();
			}
    	});

    	renameRbdBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				renameRbd();
			}
    	});

    	cancelBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
		    	if( myShell != null ) { 
		    		myShell.dispose();
		    	}
			}
    	});    	
    }
 
    private void renameRbd() {
    	String newRbdName = newRbdNameText.getText().trim(); 

    	if( !SpfsManager.getInstance().isValidRbdName( newRbdName ) ) {
    		MessageBox mb = new MessageBox(myShell, SWT.ICON_ERROR);
    		mb.setMessage("Invalid RBD name.");
    		mb.setText("Error");
    		mb.open();
    		return;
    	} 

    	rbdToRename.setRbdName( newRbdName );

    	if( myShell != null ) { 
    		myShell.dispose();
    	}
    }
}
