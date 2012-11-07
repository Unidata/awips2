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
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The define samples via lat long dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2008            Eric Babin  Initial Creation
 * 04/10/2009   #1290      rjpeter     Moved field validation to when ok pressed.
 * Oct 24, 2012 #1287      rferrel     Change shell style.
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class DefineSamplesViaLatLongDialog extends CaveJFACEDialog {

    private Composite top = null;

    private double latitude = 40;

    private double longitude = -96;

    private Text latitudeField, longitudeField;

    public DefineSamplesViaLatLongDialog(Shell parent) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        top.setLayout(new GridLayout(2, true));

        initializeComponents();

        return top;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Set", true);
        createButton(parent, IDialogConstants.CANCEL_ID, "Dismiss", false);
    }

    private void initializeComponents() {
        Label lab = new Label(top, SWT.NONE);
        lab.setText("Enter Latitude:");
        lab.setLayoutData(new GridData(SWT.LEAD, SWT.CENTER, false, false));

        latitudeField = new Text(top, SWT.BORDER);

        Label lab2 = new Label(top, SWT.NONE);
        lab2.setText("Enter Longitude:");
        lab2.setLayoutData(new GridData(SWT.LEAD, SWT.CENTER, false, false));

        longitudeField = new Text(top, SWT.BORDER);
        // if lat > -90 and lat < 90 and lon >= -180 and lon <= 180:

        GridData data = new GridData(60, SWT.DEFAULT);
        latitudeField.setText(String.valueOf(latitude));
        latitudeField.setLayoutData(data);

        data = new GridData(60, SWT.DEFAULT);
        longitudeField.setLayoutData(data);
        longitudeField.setText(String.valueOf(longitude));

    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Define Samples by Lat/Lon");

    }

    @Override
    protected void okPressed() {
        if (validateFields()) {
            DataManager
                    .getCurrentInstance()
                    .getSampleSetManager()
                    .addAnchoredSample(
                            new Coordinate(getLongitude(), getLatitude()));
        }
    }

    private boolean validateFields() {
        boolean invalid = false;

        try {
            Double d = Double.parseDouble(latitudeField.getText());
            if (d > -90 && d < 90) {
                latitude = d;
            } else {
                invalid = true;
            }
        } catch (NumberFormatException nfe) {
            invalid = true;
        }

        if (invalid) {
            MessageDialog.openWarning(null, "Invalid Value",
                    "Latitude must be between -90 and 90");
            latitudeField.setFocus();
        } else {
            try {
                Double d = Double.parseDouble(longitudeField.getText());
                if (d >= -180 && d <= 180) {
                    longitude = d;
                } else {
                    invalid = true;
                }
            } catch (NumberFormatException nfe) {
                invalid = true;
            }

            if (invalid) {
                MessageDialog.openWarning(null, "Invalid Value",
                        "Longitude must be between -180 and 180");
                longitudeField.setFocus();
            }
        }

        return !invalid;
    }
}
