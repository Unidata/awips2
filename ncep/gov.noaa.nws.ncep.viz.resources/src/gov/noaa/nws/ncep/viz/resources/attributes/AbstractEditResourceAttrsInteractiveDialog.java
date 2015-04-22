package gov.noaa.nws.ncep.viz.resources.attributes;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 *  An interface to edit resource attributes interactively.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/29/2012  #651        S. Gurung    Initial Creation.
 * 06/21/2012  #569        G. Hull      call refreshGUIElements() to update the Fade Display
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */

public class AbstractEditResourceAttrsInteractiveDialog extends AbstractEditResourceAttrsDialog {

	protected boolean isRscAttrSetChanged = false;
    
    public boolean isRscAttrSetChanged() {
		return isRscAttrSetChanged;
	}

	public void setRscAttrSetChanged(boolean isRscAttrSetChanged) {
		this.isRscAttrSetChanged = isRscAttrSetChanged;
	}

	public AbstractEditResourceAttrsInteractiveDialog(Shell parentShell,
			INatlCntrsResourceData r, Boolean apply) {
		super(parentShell, r, apply);
		// TODO Auto-generated constructor stub
	}

	@Override
	public Composite createDialog(Composite topComp) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub
		
	}
	
	@Override
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
        		rscData.setRscAttrSet( new ResourceAttrSet(editedRscAttrSet));
        		
        		// This is to update the Fade Display with a possible change in brightness
        		// which could have occured even though we are cancelling.
        		NcEditorUtil.refreshGUIElements(NcDisplayMngr.getActiveNatlCntrsEditor());

        		ok=false;
        		shell.dispose();
        	}
		});

        /*if( hasApplyBtn ) {
        	Button applyBtn = new Button( okCanComp, SWT.PUSH );
        	applyBtn.setText(" Apply ");

        	applyBtn.addSelectionListener( new SelectionAdapter() {
        		public void widgetSelected(SelectionEvent e) {
        			setRscAttrSetChanged(true);
        			
            		rscData.setRscAttrSet( new ResourceAttrSet( rscData.getRscAttrSet() )); 
            		rscData.setIsEdited( true );
            		NcDisplayMngr.getActiveNatlCntrsEditor().refresh();
        		}
        	});
        }*/
        
        Button okBtn = new Button( okCanComp, SWT.PUSH );
        okBtn.setText("    OK    ");

        okBtn.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {

        		// This is to update the Fade Display with a possible change in brightness.
        		NcEditorUtil.refreshGUIElements(NcDisplayMngr.getActiveNatlCntrsEditor());
        		 
        		ok=true;
        		// get the 
        		shell.dispose();
        	}
		});
    }
	 
	@Override
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
    		editedRscAttrSet = new ResourceAttrSet( rscData.getRscAttrSet() );
    		rscData.setRscAttrSet( editedRscAttrSet );
    		rscData.setIsEdited( true );
    	}
    	
    	dispose();
    	
    	return null;
    }    
    

}
