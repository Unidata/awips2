package gov.noaa.nws.ncep.viz.resources.attributes;

import java.io.File;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 *  an interface to edit resource attributes
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 29 May 2009   #115        Greg Hull    Initial Creation.
 * 19 Nov 2009	 #192		 M. Li		Added new constructor
 * 15 Dec 2009               Greg Hull  removed new constructor	
 * 04 Apr 2010   #259        Greg Hull  add dispose()
 * 27 Apr 2010   #245        Greg Hull  Added Apply Button
 * 22 Nov 2011   #495        Greg Hull  Make Application Modal when from ResourceManager.
 * 13 Mar 2012   #651        Archana    Updated the code to reuse editedRscAttrSet in case of a roll-back  
 * 28 March 2012 #651        S. Gurung  Removed changes made by Archana. 
 * 										Moved the changes to a new class AbstractEditResourceAttrsInteractiveDialog.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public abstract class AbstractEditResourceAttrsDialog extends Dialog { 
    protected Shell shell;
    protected String dlgTitle = null;
    
    protected INatlCntrsResourceData rscData;
    protected ResourceAttrSet editedRscAttrSet = null;
    
    protected boolean hasApplyBtn=false;
    protected boolean ok=false;
    
    public AbstractEditResourceAttrsDialog( Shell parentShell, 
    		INatlCntrsResourceData r, Boolean apply ) {
        super(parentShell);
        rscData = r;
        ResourceName rscName = rscData.getResourceName(); 
        this.dlgTitle = "Edit " + rscName.toString() + " Attributes";
        
        hasApplyBtn = apply;
//        editedRscAttrSet = new ResourceAttrSet( rscData.getRscAttrSet().getRscAttrSetName() );
    }

	public abstract Composite createDialog( Composite topComp );
     
     // initialize the GUI from editedRscAttrSet
     public abstract void initWidgets(); 
     
     public void createShell( ) {
    	int style = SWT.DIALOG_TRIM | SWT.RESIZE ;
    	if( !hasApplyBtn ) {
    		style |= SWT.APPLICATION_MODAL;
    	}
    	
    	shell = new Shell( getParent(), style );
    	shell.setText( dlgTitle );
    	//shell.setSize( 600, 800 ); // pack later

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	shell.setLayout(mainLayout);
    	
    	Composite topComp = new Composite( shell, SWT.NONE );
    	topComp.setLayout( new GridLayout() );
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        topComp.setLayoutData( gd );
        
    	Composite editComp = createDialog( topComp );
    	Label sep = new Label( shell, SWT.SEPARATOR | SWT.HORIZONTAL );
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        sep.setLayoutData( gd );

        
        Composite okCanComp = new Composite( shell, SWT.NONE );
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.horizontalAlignment = SWT.CENTER;
        okCanComp.setLayoutData( gd );
        
        okCanComp.setLayout( new GridLayout( (hasApplyBtn ? 3 : 2), true) );
        
        Button canBtn = new Button( okCanComp, SWT.PUSH );
        canBtn.setText(" Cancel ");

        canBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		ok=false;
        		shell.dispose();
        	}
		});

        if( hasApplyBtn ) {
        	Button applyBtn = new Button( okCanComp, SWT.PUSH );
        	applyBtn.setText(" Apply ");

        	applyBtn.addSelectionListener( new SelectionAdapter() {
        		public void widgetSelected(SelectionEvent e) {
            		rscData.setRscAttrSet( editedRscAttrSet );
            		rscData.setIsEdited( true );
            		NcDisplayMngr.getActiveNatlCntrsEditor().refresh();
        		}
        	});
        }
        
        Button okBtn = new Button( okCanComp, SWT.PUSH );
        okBtn.setText("    OK    ");

        okBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		ok=true;
        		// get the 
        		shell.dispose();
        	}
		});
    }

    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

    public Object open() {
    	Display display = getParent().getDisplay();
    	
    	// copy the attrSet
    	editedRscAttrSet = new ResourceAttrSet( rscData.getRscAttrSet() );
        
        createShell();

        initWidgets();
        
    	shell.pack();
    	shell.open();

    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}
    	// Uses Java Bean utils to set the attributes on the resource
    	if( ok ) {
    		
    		rscData.setRscAttrSet( editedRscAttrSet );
    		rscData.setIsEdited( true );
    	}
    	
    	dispose();
    	
    	return null;
    }    
    
    // allow to override
	protected void dispose() {
	}
}
