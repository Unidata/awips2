/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.radarapps.rmr;

import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.rcm.rmr.MultipleRequest;

public class RequestDialog extends Dialog {

	private static final int MINIMUM_INTERVAL_IN_SECONDS = 4 * 60;

    private MultipleRequest mr;
	
	private Text nameText, repeatText, durationText;
	private Button singleButton;
	private Button multipleButton;
	private Combo repeatCombo, durationCombo;
    private Label validationMessageLabel;
	List<MultipleRequest> allRequests;
	
	public RequestDialog(Shell parentShell, MultipleRequest mr, 
			List<MultipleRequest> allRequests) {
		super(parentShell);
		setShellStyle((getShellStyle() & ~SWT.APPLICATION_MODAL) | SWT.PRIMARY_MODAL);
		this.mr = mr;
		this.allRequests = allRequests;
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Edit Request");
	}

	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite c = (Composite) super.createDialogArea(parent);
		((GridLayout) c.getLayout()).numColumns = 1;

		GridLayout gl = new GridLayout(1, false);
		c.setLayout(gl);
		Composite r;
		Label l;
		
		r = new Composite(c, SWT.NONE);
		gl = new GridLayout(2, false);
		r.setLayout(gl);
		
		l = new Label(r, SWT.LEFT);
		l.setText("Name: ");
		
		SelectionAdapter rtsa = new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				setSingleMode(singleButton.getSelection());
				validate();
			}
		};
		ModifyListener ml = new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				validate();
			}
		};
		SelectionAdapter sa = new SelectionAdapter() {
		    public void widgetSelected(SelectionEvent e) {
                validate();
            }
		};
		
		nameText = new Text(r, SWT.SINGLE | SWT.BORDER);		
		GridData gd = new GridData();
		gd.widthHint = convertWidthInCharsToPixels(40);
		nameText.setLayoutData(gd);
		nameText.addModifyListener(ml);
		
		Composite g = new Composite(c, SWT.NONE);
		RowLayout rl = new RowLayout(SWT.VERTICAL);
		g.setLayout(rl);
		
		nameText.setText(mr.getName());
		
		singleButton = new Button(g, SWT.RADIO);
		singleButton.setText("Single Request"); 
		singleButton.addSelectionListener(rtsa);
		multipleButton = new Button(g, SWT.RADIO);
		multipleButton.setText("Multiple Request"); 
		multipleButton.addSelectionListener(rtsa);
		
		Group sec = new Group(c, SWT.DEFAULT);
		sec.setText("Timing");
		g = sec;
		
		gl = new GridLayout(3, false);
		g.setLayout(gl);
		l = new Label(g, SWT.LEFT);
		l.setText("Repeat every");
		repeatText = new Text(g, SWT.SINGLE | SWT.BORDER);
		repeatText.addModifyListener(ml);
		gd = new GridData();
		gd.widthHint = convertWidthInCharsToPixels(4);
		repeatText.setLayoutData(gd);
		
		repeatCombo = new Combo(g, SWT.READ_ONLY);
		repeatCombo.add("minute(s)");
		repeatCombo.add("hours(s)");
		repeatCombo.addSelectionListener(sa);
		
		l = new Label(g, SWT.LEFT);
		l.setText("Duration:");
		durationText= new Text(g, SWT.SINGLE | SWT.BORDER);
		durationText.addModifyListener(ml);
		gd = new GridData();
		gd.widthHint = convertWidthInCharsToPixels(4);
		durationText.setLayoutData(gd);
		
		durationCombo= new Combo(g, SWT.READ_ONLY);
		durationCombo.add("minute(s)");
		durationCombo.add("hours(s)");
		durationCombo.addSelectionListener(sa);
	
		singleButton.setSelection(mr.isSingle());
		multipleButton.setSelection(! mr.isSingle());
		setSingleMode(mr.isSingle());
		
		validationMessageLabel = new Label(c, SWT.LEFT);
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gd.verticalIndent = 10;
		validationMessageLabel.setLayoutData(gd);
		
		return c;
	}
	
    @Override
    public void create() {
        super.create();
        validate();
    }
	
	private void setSingleMode(boolean isSingle) {
		if (isSingle) {
			repeatText.setText("");
			durationText.setText("");
		} else {
			setTimeFromSeconds(mr.getInterval(), repeatText, repeatCombo);
			setTimeFromSeconds(mr.getDuration(), durationText, durationCombo);
		}
		boolean enb = ! isSingle;
		repeatText.setEnabled(enb);
		repeatCombo.setEnabled(enb);
		durationText.setEnabled(enb);
		durationCombo.setEnabled(enb);
	}
	
	private void setTimeFromSeconds(int seconds, Text text,
			Combo combo) {
		int v;
		if (seconds % 3600 == 0) {
			v = seconds / 3600;
			combo.select(1);
		} else {
			v = seconds / 60;
			combo.select(0);
		}
		text.setText(Integer.toString(v));
	}

	private int getTimeInSeconds(Text text, Combo combo) {
		int i = Integer.parseInt(text.getText());
		if (combo.getSelectionIndex() == 1)
			return i * 3600;
		else
			return i * 60;
	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (checkValid(true) == null)
				super.buttonPressed(buttonId);
		} else
			super.buttonPressed(buttonId);
	}

	private void validate() {
	    String validationMessage = checkValid(false); 
		Button b = getButton(IDialogConstants.OK_ID);
		if (b != null)
			b.setEnabled(validationMessage == null);
		if (validationMessageLabel != null)
		    validationMessageLabel.setText(validationMessage != null ? 
		            validationMessage : "");
	}
	
	private String checkValid(boolean commit) {
		String name = nameText.getText();
		int interval;
		int duration;
		
		if (name.length() < 1)
			return "A name is requred.";
		
		if (! name.equals(mr.getName())) {
			for (MultipleRequest mr : allRequests)
				if (mr != this.mr && name.equals(mr.getName()))
					return "A request with this name already exists.";
		}

		if (singleButton == null)
		    return "UI not initialized.";
		
		if (singleButton.getSelection()) {
			interval = 0;
			duration = 0;
		} else {
			try {
				interval = getTimeInSeconds(repeatText, repeatCombo);
				duration = getTimeInSeconds(durationText, durationCombo);
			} catch (RuntimeException e) {
				return "Invalid duration/interval.";
			}
			if (duration < interval)
			    return "Duration is less than interval.";
			if (interval < MINIMUM_INTERVAL_IN_SECONDS || 
			        duration < MINIMUM_INTERVAL_IN_SECONDS)
				return "Interval must be at least four minutes.";
		}
		
		if (commit) {
			mr.setName(name);
			mr.setInterval(interval);
			mr.setDuration(duration);
		}

		return null;
	}

}
