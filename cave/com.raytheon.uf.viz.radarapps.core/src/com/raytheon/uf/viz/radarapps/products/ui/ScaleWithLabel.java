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
package com.raytheon.uf.viz.radarapps.products.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.viz.ui.widgets.MinimumSizeComposite;

// TODO: make label a spinner control for precise control
public class ScaleWithLabel extends Composite {
	private MinimumSizeComposite msc;
	private Scale scale;
	private Label label;
	private double labelScale = 1.0;
	private String labelSuffix;
	
	public ScaleWithLabel(Composite parent, int style) {
		super(parent, SWT.NONE);
		GridLayout gl = new GridLayout(1, false);
		gl.marginWidth = gl.marginHeight = 0;
		gl.marginLeft = gl.marginRight = gl.marginTop = gl.marginBottom = 0;
		gl.horizontalSpacing = gl.verticalSpacing = 0;
		
		GridData gd;
		
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		setLayout(gl);
		label = new Label(this, SWT.CENTER);
		label.setText("   ");
		label.setLayoutData(gd);
		
		gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
		msc = new MinimumSizeComposite(this);
		msc.setLayoutData(gd);
		scale = new Scale(msc, style);
		msc.setControl(scale);
		scale.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				updateValue();
			}
		});
		updateValue();
		
		setScaleLength(100);
	}
	
	public Scale getScale() {
		return scale;
	}

	public Label getLabel() {
		return label;
	}

	public void updateValue() {
		if (! label.isDisposed() && ! scale.isDisposed()) {
			String s;
			if (scale.isEnabled()) {
				//label.setText(Integer.toString(scale.getSelection()));
				if (labelScale < 1)
					s = String.format("%.1f%s", 
							scale.getSelection() * labelScale,
							labelSuffix != null ? labelSuffix : "");
				else
					s = String.format("%d%s",
							scale.getSelection(),
							labelSuffix != null ? labelSuffix : "");
			} else
				s = "";
			label.setText(s);
		}
	}
	
	public void setScaleLength(int length) {
		msc.setMinimumSize(length, SWT.DEFAULT);
	}

	public void setLabelScale(double d) {
		labelScale = d;
		updateValue();
	}

	public void setLabelSuffix(String string) {
		labelSuffix = string;
		updateValue();
	}
	
	public void setSelection(int selection) {
		scale.setSelection(selection);
		updateValue();
	}

	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		scale.setEnabled(enabled);
		updateValue();
	}	
}
