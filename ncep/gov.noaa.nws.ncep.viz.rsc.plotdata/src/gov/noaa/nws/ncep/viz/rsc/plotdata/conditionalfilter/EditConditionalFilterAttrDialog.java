package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 *  UI for temporarily editing Conditional Filter attribute of a Point Data Resource
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial Creation.
 * 12/2012      #947       Greg Hull   Allow pre-load editing
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class EditConditionalFilterAttrDialog extends Dialog {

	protected Shell shell;
	protected String dlgTitle = "Edit Conditional Filter";
	protected boolean ok=false;

	private ConditionalFilter editedConditionalFilter = null;
	
	public EditConditionalFilterAttrDialog(Shell parentShell, ConditionalFilter cf ) {  
		super(parentShell);
		dlgTitle = "Edit Conditional Filter " + cf.getPlugin()+"/"+cf.getName();
		editedConditionalFilter = new ConditionalFilter( cf );
	}

	public void createShell( int x, int y ) {
	
		shell = new Shell( getParent(), SWT.DIALOG_TRIM | SWT.RESIZE );
		shell.setText( dlgTitle );
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		shell.setLayout(mainLayout);
		shell.setLocation(x, y);

		Composite editConditionalFilterComp = new EditConditionalFilterComposite( shell, SWT.NONE, editedConditionalFilter);

		GridData gd = new GridData();

		Composite okCanComp = new Composite( shell, SWT.NONE );
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		okCanComp.setLayoutData( gd );

		okCanComp.setLayout( new FormLayout() );

		Button canBtn = new Button( okCanComp, SWT.PUSH );
		canBtn.setText(" Cancel ");
		FormData fd = new FormData();
		fd.width = 80;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left  = new FormAttachment( 20, -40 );
		canBtn.setLayoutData( fd );

		canBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				ok=false;
				shell.dispose();
			}
		});

		Button applyBtn = new Button( okCanComp, SWT.PUSH );
		applyBtn.setText("  OK  ");
		fd = new FormData();
		fd.width = 80;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left = new FormAttachment( 40, -40 );		
		applyBtn.setLayoutData( fd );
//		if ("".equals(editedConditionalFilter.getName()))
//			applyBtn.setEnabled(false);

		applyBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				ok=true;
				shell.dispose();
			}
		});

		Button revertBtn = new Button( okCanComp, SWT.PUSH );
		revertBtn.setText("  Revert  ");
		fd = new FormData();
		fd.width = 90;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left = new FormAttachment( 60, -40 );		
		revertBtn.setLayoutData( fd );
		
		revertBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				
				editedConditionalFilter = ConditionalFilterMngr.getInstance().getConditionalFilter(
						editedConditionalFilter.getPlugin(), editedConditionalFilter.getName() );
				ok=true;
                shell.dispose();
			}
		});
		
		final Button helpBtn = new Button ( okCanComp, SWT.PUSH);
        helpBtn.setText(" Help... ");
        helpBtn.setToolTipText("Help for Conditional Filter");
        
        fd = new FormData();
		fd.width = 80;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left = new FormAttachment( 80, -40 );		
		helpBtn.setLayoutData( fd );
        
        helpBtn.addSelectionListener(new SelectionAdapter() {

			public void widgetSelected(SelectionEvent e) {
				
				ConditionalFilterHelpDialog condFilterDlg = new ConditionalFilterHelpDialog(shell.getShell());
				helpBtn.setEnabled(false);
				condFilterDlg.open();
				if (!helpBtn.isDisposed())
					helpBtn.setEnabled(true);
			}

		});

	}

	public void open() {
		open( getParent().getLocation().x +10, 
			  getParent().getLocation().y +10);
	}
	

	public Object open( int x, int y) {
		Display display = getParent().getDisplay();

		createShell(x,y);

		initWidgets();

		shell.pack();
		shell.open();

		while( !shell.isDisposed() ) {
			if( !display.readAndDispatch() ) {
				display.sleep();
			}
		}

		return ( ok ? editedConditionalFilter : null );
	}    

	public void initWidgets() {
	}                           
}

