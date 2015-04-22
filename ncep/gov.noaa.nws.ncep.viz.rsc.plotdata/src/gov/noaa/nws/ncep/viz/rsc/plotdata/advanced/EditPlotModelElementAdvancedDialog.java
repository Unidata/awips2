package gov.noaa.nws.ncep.viz.rsc.plotdata.advanced;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilterMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefn;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 *  UI for temporarily editing advanced settings Plot Model Elements of Point Data Resources
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/2012      #431       S. Gurung   Initial Creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class EditPlotModelElementAdvancedDialog extends Dialog {

	protected Shell shell;
	protected String dlgTitle = "Edit Advanced Options";
	protected boolean ok=false;

	protected ConditionalColorBar editedColorBar = null;
	protected PlotModelElement editedPlotModelElement = null;
	protected String[] allParamNamesArray = null;
	
	private Combo paramNameCombo = null;
	
	public EditPlotModelElementAdvancedDialog(Shell parentShell, PlotModelElement pme, PlotParameterDefns plotParamDefns) {  
		super(parentShell);
		dlgTitle = dlgTitle + " for " + pme.getParamName();
		editedPlotModelElement = pme;
		
		List<String> plotParamNames = new ArrayList<String>();
		for( PlotParameterDefn plotPrmDefn : plotParamDefns.getParameterDefns() ) { 
			if (!plotPrmDefn.getPlotMode().equalsIgnoreCase("table") && !plotPrmDefn.getPlotMode().equalsIgnoreCase("barb") && (plotPrmDefn.getPlotUnit() != null && !plotPrmDefn.getPlotUnit().isEmpty()) ) { 
				plotParamNames.add(plotPrmDefn.getPlotParamName());
			}
		}
		
		allParamNamesArray = new String[plotParamNames.size()];		
		for( int i=0; i<plotParamNames.size(); i++) {
			String plotParamName = plotParamNames.get(i);
			allParamNamesArray[i] = plotParamName;
		}
		//allParamNamesArray = plotParamDefns.getAllParameterNames( false, false );		
		Arrays.sort(allParamNamesArray);
	}

	public void createShell( int x, int y ) {
	
		shell = new Shell( getParent(), SWT.DIALOG_TRIM | SWT.RESIZE );
		shell.setText( dlgTitle );
		shell.setLocation(x, y);
		
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		shell.setLayout(mainLayout);
		
		Group mainGrp = new Group ( shell, SWT.SHADOW_NONE );
		mainGrp.setLayout(mainLayout);
		mainGrp.setText("Advanced Options for " + editedPlotModelElement.getParamName());
		
		Composite advancedComp = new Composite( mainGrp, SWT.NONE );
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		advancedComp.setLayoutData( gd );
		advancedComp.setLayout( new FormLayout() );
	
		Label condParamNameLbl = new Label( advancedComp, SWT.NONE );
		condParamNameLbl.setText( "Conditional Parameter" );
		FormData fd = new FormData();
		fd.top = new FormAttachment( 0, 15 );
		fd.bottom = new FormAttachment( 50, 10 );
		fd.left  = new FormAttachment( 9, -43 );		
		condParamNameLbl.setLayoutData(fd);
		
		paramNameCombo = new Combo(advancedComp, SWT.DROP_DOWN | SWT.READ_ONLY);
		paramNameCombo.setItems(allParamNamesArray);
		paramNameCombo.setText((editedPlotModelElement.getConditionalParameter()!=null? editedPlotModelElement.getConditionalParameter(): ""));
		fd = new FormData();
		fd.top = new FormAttachment( 0, 15 );
		fd.bottom = new FormAttachment( 60, 10 );
		fd.left = new FormAttachment(condParamNameLbl, 40, SWT.RIGHT );	
		paramNameCombo.setLayoutData(fd);		
		paramNameCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				updateConditionalParameter();
			}
		});
		
		Composite editConditionalcolorComp = new EditConditionalColorbarComposite( mainGrp, SWT.NONE, editedPlotModelElement);
			
		gd = new GridData();

		Composite okCanComp = new Composite( mainGrp, SWT.NONE );
		gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		gd.horizontalAlignment = SWT.FILL;
		okCanComp.setLayoutData( gd );

		okCanComp.setLayout( new FormLayout() );

		Button canBtn = new Button( okCanComp, SWT.PUSH );
		canBtn.setText(" Cancel ");
		fd = new FormData();
		fd.width = 80;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left  = new FormAttachment( 40, -70 );
		canBtn.setLayoutData( fd );

		canBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				ok=false;
				shell.dispose();
			}
		});

		Button okBtn = new Button( okCanComp, SWT.PUSH );
		okBtn.setText("  OK  ");
		fd = new FormData();
		fd.width = 80;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left = new FormAttachment( 40, 17 );		
		okBtn.setLayoutData( fd );
		
		okBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				
				if (paramNameCombo.getText() == null || "".equals(paramNameCombo.getText())) {
                	MessageDialog infoDlg = new MessageDialog(shell, "Message", null, 
                			"Please select a Conditional Parameter.",
                			MessageDialog.INFORMATION, 
                			new String[]{" OK "}, 0);
                	infoDlg.open();

                	return;
                }
			
				ok=true;
				shell.dispose();
			}
		});


		// create " Clear Advanced" button
		Button clearAdvancedBtn = new Button(okCanComp, SWT.NONE);        
        clearAdvancedBtn.setText("Clear Advanced"); 
        clearAdvancedBtn.setToolTipText("Clear Advanced Settings");
        fd = new FormData();
		fd.width = 120;
		fd.bottom = new FormAttachment( 100, -5 );
		fd.left = new FormAttachment( 40, 105 );		
		clearAdvancedBtn.setLayoutData( fd );
        if (!editedPlotModelElement.hasAdvancedSettings())
        	clearAdvancedBtn.setEnabled(false);
        
        clearAdvancedBtn.addSelectionListener(new SelectionListener() 
        {
        	public void widgetSelected(SelectionEvent event) 
        	{        		
        		clearAdvancedSettings();
        	}
        	public void widgetDefaultSelected(SelectionEvent event) 
        	{
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

		return ( ok ? editedPlotModelElement : null );
	}    

	public void initWidgets() {
	}   
	
	public void updateConditionalParameter() {
		editedPlotModelElement.setConditionalParameter(paramNameCombo.getText());
	}
	
	private void clearAdvancedSettings() {
		
		MessageDialog confirmDlg = new MessageDialog(shell, "Confirm Clear Advanced Settings", null, 
				"Are you sure you want to delete the advanced settings for plot parameter '"+editedPlotModelElement.getParamName()+"'?\n",
				MessageDialog.QUESTION, 
				new String[]{"Yes", "No"}, 0);
		confirmDlg.open();

		if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
			return;
		}

		editedPlotModelElement.setConditionalParameter(editedPlotModelElement.getParamName());
		editedPlotModelElement.setConditionalColorBar(new ConditionalColorBar());
		ok=true;
		shell.dispose();
	}
}

