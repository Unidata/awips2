package com.raytheon.viz.warnings.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.warnings.rsc.AbstractWWAResource;

/**
 * Dialog with options for displaying the outline, fill, text, and time, 
 * individually, for each warnings, watches, and advisories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer          Description
 * ------------ ---------- ----------------  --------------------------
 * Mar 15, 2022            srcarter@ucar     Initial creation
 * Mar 21, 2022			   srcarter@ucar	 Set the current values every time initializeComponents is called (also called from .Open)
 * Jun 24, 2022			   srcarter@ucar	 Move Watches to top, add section for Other/Statement, add 'enabled' functionality
 * Jun 28, 2022			   srcarter@ucar	 Add 'Sampling' options
 * 
 * </pre>
 * 
 * @author srcarter
 */

public class DrawingPropertiesDialog extends CaveSWTDialog {

	//gui components
	private Button warnOutlineChk;
	private Button warnFillChk;
	private Button warnTextChk;
	private Button warnTimeChk;
	private Button warnSampleChk;
	private Button watchOutlineChk;
	private Button watchFillChk;
	private Button watchTextChk;
	private Button watchTimeChk;
	private Button watchSampleChk;
	private Button advOutlineChk;
	private Button advFillChk;
	private Button advTextChk;
	private Button advTimeChk;
	private Button advSampleChk;
	private Button otherOutlineChk;
	private Button otherFillChk;
	private Button otherTextChk;
	private Button otherTimeChk;
	private Button otherSampleChk;
	
	/**
	 * The WWA Resource associated with this properties dialog
	 */
	private AbstractWWAResource myResource;
	
	
	/**
	 * Creates a dialog with drawing options for all the warning, watches and 
	 * advisories drawn in the resource
	 * @param parent  The parent gui component to associate this dialog with
	 * @param rsc  The WWA resource associated with this dialog
	 */
	protected DrawingPropertiesDialog(Shell parent, AbstractWWAResource rsc) {
		super(parent, SWT.RESIZE | SWT.CLOSE);
		myResource = rsc;
		setText("WWA Drawing Properties");
	}

	@Override
	// Create and initialize all gui components for controlling the drawing
	// displays for the WWAs. 
	protected void initializeComponents(Shell shell) {
	// --- Sub title ---
		Composite subComp = new Composite(shell, SWT.NONE);
		subComp.setLayout(new GridLayout(1, true));
		subComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));	
		Label layerName = new Label(subComp, SWT.NONE);
		layerName.setText(myResource.getResourceData().getName());
	
	// --- Watches ---
		Group watchComp = new Group(shell, SWT.NONE);
		watchComp.setText("Watches");
		watchComp.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
		watchComp.setLayout(new GridLayout(2, true));
		
		//outline and fill
		watchOutlineChk = createButton(watchComp, "Show Outline");
		watchFillChk = createButton(watchComp, "Thatched Fill");
		
		//text and time
		watchTextChk = createButton(watchComp, "Show Text");
		watchTimeChk = createButton(watchComp, "Show Time");
		
		//sample
		watchSampleChk = createButton(watchComp, "Show Sampling");
		
	// --- end Watches ---	
		
	// --- Warnings ---
		Group warnComp = new Group(shell, SWT.NONE);
		warnComp.setText("Warnings");
		warnComp.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
		warnComp.setLayout(new GridLayout(2, true));
		
		//outline and fill
		warnOutlineChk = createButton(warnComp, "Show Outline");
		warnFillChk = createButton(warnComp, "Thatched Fill");
		//text and time
		warnTextChk = createButton(warnComp, "Show Text");
		warnTimeChk = createButton(warnComp, "Show Time");
		//sample
		warnSampleChk = createButton(warnComp, "Show Sampling");
		
	// --- end Warnings ---	
		
	// --- Advisories ---
		Group advComp = new Group(shell, SWT.NONE);
		advComp.setText("Advisories");
		advComp.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
		advComp.setLayout(new GridLayout(2, true));
		
		//outline and fill
		advOutlineChk = createButton(advComp, "Show Outline");
		advFillChk = createButton(advComp, "Thatched Fill");
		
		//text and time
		advTextChk = createButton(advComp, "Show Text");
		advTimeChk = createButton(advComp, "Show Time");
		
		//sample
		advSampleChk = createButton(advComp, "Show Sampling");
		
	// --- end Advisories ---	
		
	// --- Other ---
		Group otherComp = new Group(shell, SWT.NONE);
		otherComp.setText("Statements/Other");
		otherComp.setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
		otherComp.setLayout(new GridLayout(2, true));
		
		//outline and fill
		otherOutlineChk = createButton(otherComp, "Show Outline");
		otherFillChk = createButton(otherComp, "Thatched Fill");
		
		//text and time
		otherTextChk = createButton(otherComp, "Show Text");
		otherTimeChk = createButton(otherComp, "Show Time");
		
		//sample
		otherSampleChk = createButton(otherComp, "Show Sampling");
		
	// --- end Other ---	
		
	// --- Bottom Buttons ---
		Composite btnComp = new Composite(shell, SWT.NONE);
		btnComp.setLayout(new GridLayout(1, true));
		btnComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));	
		
		// Reset Button
		Button resetBtn = new Button(btnComp, SWT.PUSH);
		resetBtn.setText("Reset Defaults");
		GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
		resetBtn.setLayoutData(gd);
		resetBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event){
				resetDefaults();
				updateDisplay();
			}
		});
		
		// Close Button
		Button closeBtn = new Button(btnComp, SWT.PUSH);
		closeBtn.setText("Close");
		gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
		gd.widthHint = 70;
		closeBtn.setLayoutData(gd);
		closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
            }
        });
	// --- end Buttons ---
		
		//set all the values
		setToCurrentValues();
		
		//set visibility from resource
		setWarningControlsEnabled(myResource.enableWarnDisplay());
		setWatchControlsEnabled(myResource.enableWatchDisplay());
		setAdvisoryControlsEnabled(myResource.enableAdvisoryDisplay());
		setOtherControlsEnabled(myResource.enableOtherDisplay());
	}
	
	private void setWarningControlsEnabled(boolean isEnabled){
		warnOutlineChk.setEnabled(isEnabled);
		warnFillChk.setEnabled(isEnabled);
		warnTextChk.setEnabled(isEnabled);
		warnTimeChk.setEnabled(isEnabled);
		warnSampleChk.setEnabled(isEnabled);
	}
	
	private void setWatchControlsEnabled(boolean isEnabled){
		watchOutlineChk.setEnabled(isEnabled);
		watchFillChk.setEnabled(isEnabled);
		watchTextChk.setEnabled(isEnabled);
		watchTimeChk.setEnabled(isEnabled);
		watchSampleChk.setEnabled(isEnabled);
	}
	
	private void setAdvisoryControlsEnabled(boolean isEnabled){
		advOutlineChk.setEnabled(isEnabled);
		advFillChk.setEnabled(isEnabled);
		advTextChk.setEnabled(isEnabled);
		advTimeChk.setEnabled(isEnabled);
		advSampleChk.setEnabled(isEnabled);
	}
	
	private void setOtherControlsEnabled(boolean isEnabled){
		otherOutlineChk.setEnabled(isEnabled);
		otherFillChk.setEnabled(isEnabled);
		otherTextChk.setEnabled(isEnabled);
		otherTimeChk.setEnabled(isEnabled);
		otherSampleChk.setEnabled(isEnabled);
	}
	
	/**
	 * Creates a checkbox button used for defining the display
	 * properties of a given option for the WWA resource. Adds
	 * the selection listener to the button so it functions 
	 * properly as well.
	 * @param parent  The parent gui component to associate this 
	 * 					button with
	 * @param title  The text displayed next to the checkbox
	 * @return  A Checkbox style button
	 */
	private Button createButton(Composite parent, String title){
		Button btn = new Button(parent, SWT.CHECK);
		btn.setText(title);
		btn.addSelectionListener(checkboxListener);
		return btn;
	}
	
	/**
	 * The listener used on all the display checkbox buttons
	 */
	private SelectionAdapter checkboxListener = new SelectionAdapter() {
		@Override
		public void widgetSelected(SelectionEvent event){
			updateDisplay();
		}
	};
	
	/**
	 * Updates all the display settings on the resource based on the
	 * current button selections in this dialog.  Then issues a 
	 * refresh on the resource so it updates the display.
	 */
	private void updateDisplay(){
		myResource.setWarnOutlineDisplay(warnOutlineChk.getSelection());
		myResource.setWarnFillDisplay(warnFillChk.getSelection());
		myResource.setWarnTextDisplay(warnTextChk.getSelection());
		myResource.setWarnTimeDisplay(warnTimeChk.getSelection());
		myResource.setWarnSampleDisplay(warnSampleChk.getSelection());
		
		myResource.setWatchOutlineDisplay(watchOutlineChk.getSelection());
		myResource.setWatchFillDisplay(watchFillChk.getSelection());
		myResource.setWatchTextDisplay(watchTextChk.getSelection());
		myResource.setWatchTimeDisplay(watchTimeChk.getSelection());
		myResource.setWatchSampleDisplay(watchSampleChk.getSelection());
		
		myResource.setAdvisoryOutlineDisplay(advOutlineChk.getSelection());
		myResource.setAdvisoryFillDisplay(advFillChk.getSelection());
		myResource.setAdvisoryTextDisplay(advTextChk.getSelection());
		myResource.setAdvisoryTimeDisplay(advTimeChk.getSelection());
		myResource.setAdvisorySampleDisplay(advSampleChk.getSelection());
		
		myResource.setOtherOutlineDisplay(otherOutlineChk.getSelection());
		myResource.setOtherFillDisplay(otherFillChk.getSelection());
		myResource.setOtherTextDisplay(otherTextChk.getSelection());
		myResource.setOtherTimeDisplay(otherTimeChk.getSelection());	
		myResource.setOtherSampleDisplay(otherSampleChk.getSelection());

		myResource.issueRefresh();
	}

	/**
	 * Reset all the button selections in this dialog to the default
	 * values defined in the @AbstractWWAResource class
	 */
	private void resetDefaults(){
		warnOutlineChk.setSelection(AbstractWWAResource.WARN_OUTLINE_DEFAULT);
		warnFillChk.setSelection(AbstractWWAResource.WARN_FILL_DEFAULT);
		warnTextChk.setSelection(AbstractWWAResource.WARN_TEXT_DEFAULT);
		warnTimeChk.setSelection(AbstractWWAResource.WARN_TIME_DEFAULT);
		warnSampleChk.setSelection(true);
		
		watchOutlineChk.setSelection(AbstractWWAResource.WATCH_OUTLINE_DEFAULT);
		watchFillChk.setSelection(AbstractWWAResource.WATCH_FILL_DEFAULT);
		watchTextChk.setSelection(AbstractWWAResource.WATCH_TEXT_DEFAULT);
		watchTimeChk.setSelection(AbstractWWAResource.WATCH_TIME_DEFAULT);
		watchSampleChk.setSelection(true);
		
		advOutlineChk.setSelection(AbstractWWAResource.ADV_OUTLINE_DEFAULT);
		advFillChk.setSelection(AbstractWWAResource.ADV_FILL_DEFAULT);
		advTextChk.setSelection(AbstractWWAResource.ADV_TEXT_DEFAULT);
		advTimeChk.setSelection(AbstractWWAResource.ADV_TIME_DEFAULT);
		advSampleChk.setSelection(true);
		
		otherOutlineChk.setSelection(AbstractWWAResource.OTHER_OUTLINE_DEFAULT);
		otherFillChk.setSelection(AbstractWWAResource.OTHER_FILL_DEFAULT);
		otherTextChk.setSelection(AbstractWWAResource.OTHER_TEXT_DEFAULT);
		otherTimeChk.setSelection(AbstractWWAResource.OTHER_TIME_DEFAULT);	
		otherSampleChk.setSelection(true);
	}
		
	/**
	 * Set all the GUI checkboxes to the current boolean values from 
	 * the associated resource
	 */
	protected void setToCurrentValues(){
		warnOutlineChk.setSelection(myResource.showWarnOutline());
		warnFillChk.setSelection(myResource.showWarnFill());
		warnTextChk.setSelection(myResource.showWarnText());
		warnTimeChk.setSelection(myResource.showWarnTime());
		warnSampleChk.setSelection(myResource.showWarnSampling());
		
		watchOutlineChk.setSelection(myResource.showWatchOutline());
		watchFillChk.setSelection(myResource.showWatchFill());
		watchTextChk.setSelection(myResource.showWatchText());
		watchTimeChk.setSelection(myResource.showWatchTime());
		watchSampleChk.setSelection(myResource.showWatchSampling());
		
		advOutlineChk.setSelection(myResource.showAdvisoryOutline());
		advFillChk.setSelection(myResource.showAdvisoryFill());
		advTextChk.setSelection(myResource.showAdvisoryText());
		advTimeChk.setSelection(myResource.showAdvisoryTime());
		advSampleChk.setSelection(myResource.showAdvisorySampling());
		
		otherOutlineChk.setSelection(myResource.showOtherOutline());
		otherFillChk.setSelection(myResource.showOtherFill());
		otherTextChk.setSelection(myResource.showOtherText());
		otherTimeChk.setSelection(myResource.showOtherTime());
		otherSampleChk.setSelection(myResource.showOtherSampling());
	}
}
