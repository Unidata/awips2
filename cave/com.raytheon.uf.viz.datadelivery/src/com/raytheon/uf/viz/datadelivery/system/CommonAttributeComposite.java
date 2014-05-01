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
package com.raytheon.uf.viz.datadelivery.system;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

/**
 * Composite holding the common data type attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2013   2386     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CommonAttributeComposite extends Composite {
    /** Parameter spinner */
    private Spinner parameterSpinner;

    /** Spatial spinner */
    private Spinner spatialSpinner;

    /** ChangeApplpier callback */
    private final IChangeApplier callback;

    /** Apply all check box */
    private Button applyAllBtn;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Composite
     * @param style
     *            Style bits
     * @param callback
     *            The change apply callback
     */
    public CommonAttributeComposite(Composite parent, int style,
            IChangeApplier callback) {
        super(parent, style);
        this.callback = callback;
        init();
    }

    /**
     * Initialize class.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(3, false);
        Group commonGrp = new Group(this, SWT.NONE);
        commonGrp.setLayout(gl);
        commonGrp.setLayoutData(gd);
        commonGrp.setText(" Common Attributes ");

        // Label for directions
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 3;
        Label directionsLabel = new Label(commonGrp, SWT.NONE);
        directionsLabel.setLayoutData(gd);
        directionsLabel
                .setText("These attributes are common to all data types.");

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gl = new GridLayout(2, false);
        gl.marginRight = 10;
        Composite paramComp = new Composite(commonGrp, SWT.NONE);
        paramComp.setLayout(gl);
        paramComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label label = new Label(paramComp, SWT.NONE);
        label.setLayoutData(gd);
        label.setText("Parameters:");

        parameterSpinner = new Spinner(paramComp, SWT.BORDER);
        parameterSpinner.setMinimum(0);
        parameterSpinner.setMaximum(100);
        parameterSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                callback.applyChange();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gl = new GridLayout(2, false);
        gl.marginRight = 10;
        Composite spatialComp = new Composite(commonGrp, SWT.NONE);
        spatialComp.setLayout(gl);
        spatialComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label label2 = new Label(spatialComp, SWT.NONE);
        label2.setLayoutData(gd);
        label2.setText("Spatial:");

        spatialSpinner = new Spinner(spatialComp, SWT.BORDER);
        spatialSpinner.setMinimum(0);
        spatialSpinner.setMaximum(100);
        spatialSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                callback.applyChange();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Composite applyAllComp = new Composite(commonGrp, SWT.NONE);
        applyAllComp.setLayout(gl);
        applyAllComp.setLayoutData(gd);

        applyAllBtn = new Button(applyAllComp, SWT.CHECK);
        applyAllBtn.setText("Apply to all data types");
        applyAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                callback.applyChange();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gl = new GridLayout(2, false);
        gl.marginRight = 10;
        Composite matchComp = new Composite(commonGrp, SWT.NONE);
        matchComp.setLayout(gl);
        matchComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label matchLabel = new Label(matchComp, SWT.NONE);
        matchLabel.setLayoutData(gd);
        matchLabel.setText("Match:");

    }

    /**
     * @param maxAllowedParameterDuplication
     */
    public void setParameterValue(int maxAllowedParameterDuplication) {
        this.parameterSpinner.setSelection(maxAllowedParameterDuplication);
    }

    /**
     * @param maxAllowedSpatialDuplication
     */
    public void setSpatialValue(int maxAllowedSpatialDuplication) {
        this.spatialSpinner.setSelection(maxAllowedSpatialDuplication);
    }

    /**
     * @return
     */
    public int getParameterValue() {
        return this.parameterSpinner.getSelection();
    }

    /**
     * @return
     */
    public int getSpatialValue() {
        return this.spatialSpinner.getSelection();
    }

    /**
     * @return
     */
    public boolean isApplyAll() {
        return applyAllBtn.getSelection();
    }
}
