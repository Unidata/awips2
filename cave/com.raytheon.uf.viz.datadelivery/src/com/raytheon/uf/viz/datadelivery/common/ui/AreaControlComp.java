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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.datadelivery.browser.ArealSelectionDlg;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This is the areal controls composite. This class is intended to be extended
 * so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012     702    jpiatt     Initial creation.
 * Dec 07, 2012 1278       bgonzale   pass coords to ArealSelectionDlg.
 * Dec 10, 2012   1259     bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class AreaControlComp extends Composite {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AreaControlComp.class);

    /** Set Area button. */
    private Button areaBtn = null;

    /** Clear button. */
    private Button clearBtn;

    /** Area check box. */
    private Button areaChk = null;

    /** The area selected label. */
    private Label areaSelectedLbl;

    /** Area group. */
    private Group areaGrp = null;

    /** Referenced Envelope. */
    private ReferencedEnvelope envelope = null;

    /** ArealSelectionDlg object. */
    private ArealSelectionDlg arealDlg;

    /** Flag for area dirty. */
    private boolean areaDirty = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public AreaControlComp(Composite parent) {
        super(parent, SWT.NONE);
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createAreaControls();

    }

    /**
     * Create the controls for selecting an area.
     */
    private void createAreaControls() {
        areaGrp = new Group(this, SWT.NONE);
        areaGrp.setLayout(new GridLayout(1, false));
        areaGrp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        areaGrp.setText(" Areal Coverage: ");

        areaChk = new Button(areaGrp, SWT.CHECK);
        areaChk.setText("No Areal Coverage");
        areaChk.setSelection(true);
        areaChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                if (!areaChk.getSelection()) {
                    areaSelectedLbl.setEnabled(true);
                    clearBtn.setEnabled(true);
                    areaBtn.setEnabled(true);
                } else {
                    areaSelectedLbl.setEnabled(false);
                    clearBtn.setEnabled(false);
                    areaBtn.setEnabled(false);
                }

            } 
        });

        Composite areaComp = new Composite(areaGrp, SWT.NONE);
        areaComp.setLayout(new GridLayout(4, false));
        areaComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label areaLabel = new Label(areaComp, SWT.NONE);
        areaLabel.setText("Area:");
        areaLabel.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false, false));

        areaSelectedLbl = new Label(areaComp, SWT.BORDER);
        areaSelectedLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

        clearBtn = new Button(areaComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setEnabled(false);
        clearBtn.setLayoutData(new GridData(90, SWT.DEFAULT));
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleClearArea();
            }
        });

        areaBtn = new Button(areaComp, SWT.PUSH);
        areaBtn.setText("Set Area...");
        areaBtn.setEnabled(false);
        areaBtn.setLayoutData(new GridData(90, SWT.DEFAULT));
        areaBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAreaSelection();
            }
        });
    }

    /**
     * Handle the area selection.
     */
    private void handleAreaSelection() {
        if (arealDlg == null || arealDlg.isDisposed()) {
            arealDlg = new ArealSelectionDlg(getShell(), envelope);
            arealDlg.open();
        } else {
            arealDlg.bringToTop();
        }

        if (arealDlg.getReturnValue() == null || (Boolean) arealDlg.getReturnValue() == false) {
            return;
        }

        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        updateAreaLabel(arealDlg.getEnvelope());

        getShell().setCursor(null);

    }

    /**
     * Update the area label.
     * 
     * @param envelope
     *            referenced envelope
     */
    public void updateAreaLabel(ReferencedEnvelope envelope) {

        this.envelope = envelope;

        if (envelope == null || envelope.isEmpty()) {
            areaSelectedLbl.setText("");
            clearBtn.setEnabled(false);
        }

        NumberFormat formatter = new DecimalFormat(".0000");

        Coordinate ul = EnvelopeUtils.getUpperLeftLatLon(envelope);
        Coordinate lr = EnvelopeUtils.getLowerRightLatLon(envelope);

        // Check for empty values
        if (ul.x == 0 && ul.y == 0 && lr.x == 0 && lr.y == 0) {
            return;
        }

        StringBuilder sb = new StringBuilder("UL: ");
        sb.append(formatter.format(ul.x) + "," + formatter.format(ul.y));
        sb.append(", LR: " + formatter.format(lr.x) + ","
                + formatter.format(lr.y));

        areaSelectedLbl.setText(sb.toString());

        if (areaSelectedLbl.getText().length() > 0) {
            clearBtn.setEnabled(true);
        }

        this.areaDirty = true;
    }

    /**
     * Get the envelope
     * 
     * @return an envelope
     */
    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    /**
     * 
     * @return true if area has been changed
     */
    public boolean isDirty() {
        return areaDirty;
    }

    /**
     * 
     * @return true if area has been selected
     */
    public boolean isAreaChk() {
        return areaChk.getSelection();
    }

    /**
     * Set the does not expire check box.
     * 
     * @param flag
     */
    public void setNoArealCoverage(boolean flag) {
        areaChk.setSelection(flag);
    }

    /**
     * Set the Clear button.
     * 
     * @param flag
     */
    public void setClearButton(boolean flag) {
        clearBtn.setEnabled(flag);
    }

    /**
     * Set the Area button.
     * 
     * @param flag
     */
    public void setAreaButton(boolean flag) {
        areaBtn.setEnabled(flag);
    }  
    
    /**
     * Enable or disable text boxes.
     * 
     * @param flag 
     */
    public void resetTextBoxes(boolean flag) {
        areaSelectedLbl.setText("");
    }

    /**
     * Handle clear the area text.
     */
    private void handleClearArea() {
        getShell().setCursor(getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        areaSelectedLbl.setText("");
        clearBtn.setEnabled(false);
        envelope = null;

        getShell().setCursor(null);
    }

}
