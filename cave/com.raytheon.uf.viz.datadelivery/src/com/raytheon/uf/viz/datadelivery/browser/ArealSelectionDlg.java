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
package com.raytheon.uf.viz.datadelivery.browser;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.viz.datadelivery.common.ui.AreaComp;
import com.raytheon.uf.viz.datadelivery.subscription.subset.IDataSize;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to allow the user to select area filter criteria.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            mpduff     Initial creation.
 * Jun  1, 2012   645      jpiatt     Added tooltips.
 * Aug 10, 2012  1002      mpduff     Implementing dataset size estimation.
 * Oct 31, 2012  1278      mpduff     Change validation method call.
 * Dec 07, 2012  1278      bgonzale   Coordinate Array initialization in ctor.
 * Dec 10, 2012  1259      bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ArealSelectionDlg extends CaveSWTDialog implements IDataSize {

    /** Area composite. */
    private AreaComp areaComp;

    /** Envelope */
    private ReferencedEnvelope envelope;

    /** Callback for data sizing */
    private final IDataSize callback;

    /**
     * Constructor.
     * 
     * @param parentShell
     */
    public ArealSelectionDlg(Shell parentShell, IDataSize callback,
            ReferencedEnvelope envelope) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        this.setText("Area Filter Selection");
        this.callback = callback;
        this.envelope = envelope;
    }

    /**
     * Constructor
     * 
     * @param parentShell
     */
    public ArealSelectionDlg(Shell parentShell, ReferencedEnvelope envelope) {
        this(parentShell, null, envelope);
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {

        areaComp = new AreaComp(shell, "Filter by Area", this, null, envelope);
        areaComp.setToolTipText("Select area filter criteria");

        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);

        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        //OK button
        Button okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(new GridData(70, SWT.DEFAULT));
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!areaComp.isEnvelopeValid()) {
                    return;
                }

                envelope = areaComp.getEnvelope();
                setReturnValue(true);
                close();
                if (callback != null) {
                    callback.updateDataSize();
                }
            }
        });

        //Cancel button
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(new GridData(70, SWT.DEFAULT));
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int answer = DataDeliveryUtils.showMessage(getShell(), SWT.YES | SWT.NO, "Cancel Changes?",
                        "Are you sure you wish to close without selecting an area?");
                if (answer == SWT.YES) {
                    setReturnValue(false);
                    close();
                }
            }
        });
    }

    /**
     * Get the selected envelope
     * 
     * @return the selected envelope
     */
    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    @Override
    public void updateDataSize() {
        if (callback != null) {
            callback.updateDataSize();
        }
    }
}
