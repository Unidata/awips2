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
package com.raytheon.viz.awipstools.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.LabelMode;
import com.raytheon.viz.awipstools.ui.layer.DistanceSpeedLayer;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description SkewtParamsDialog.java Nov 28, 2007
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Nov 28, 2007					Eric Babin Initial Creation
 * 2013-12-20   ss#114      D. Friedman Change 2 of 3
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class DistanceSpeedDialog extends CaveJFACEDialog {

    public String[] paramData;

    public String dialogTitle;

    private Composite top = null;

    private DistanceSpeedLayer dsLayer;

    private Button pointRadio, polyRadio, timeRadio, speedRadio;

    public DistanceSpeedDialog(Shell parShell, DistanceSpeedLayer dsLayer) {
        super(parShell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.dsLayer = dsLayer;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        top.setLayout(new GridLayout(1, true));

        initializeComponents();

        return top;
    }

    private void initializeComponents() {
        Group modeGroup = new Group(top, SWT.NONE);
        modeGroup.setLayout(new GridLayout(3, true));
        modeGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));
        Label pntModeLabel = new Label(modeGroup, SWT.NONE);
        pntModeLabel.setText("Mode:");
        pointRadio = new Button(modeGroup, SWT.RADIO);
        pointRadio.setText("Point");
        pointRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                updateDisplayType(DisplayType.POINT);
            }
        });
        pointRadio.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));
        polyRadio = new Button(modeGroup, SWT.RADIO);
        polyRadio.setText("Polyline");
        polyRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                updateDisplayType(DisplayType.POLY);
            }
        });
        Group legendGroup = new Group(top, SWT.NONE);
        legendGroup.setLayout(new GridLayout(3, true));
        legendGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        Label legendLabel = new Label(legendGroup, SWT.NONE);
        legendLabel.setText("Legend:");
        timeRadio = new Button(legendGroup, SWT.RADIO);
        timeRadio.setText("Time");
        timeRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                updateLabelType(LabelMode.TIME);
            }
        });
        timeRadio.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));
        speedRadio = new Button(legendGroup, SWT.RADIO);
        speedRadio.setText("Speed");
        speedRadio.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                updateLabelType(LabelMode.SPEED);
            }
        });

        switch (dsLayer.getStormTrackState().labelMode) {
        case SPEED: {
            speedRadio.setSelection(true);
            break;
        }
        case TIME: {
            timeRadio.setSelection(true);
            break;
        }
        }

        switch (dsLayer.getStormTrackState().displayType) {
        case POLY: {
            polyRadio.setSelection(true);
            break;
        }
        case POINT: {
            pointRadio.setSelection(true);
            break;
        }
        }
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
        shell.setText(this.dsLayer.dialogTitle);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {

        return new Point(300, 140);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {

    }

    private void updateDisplayType(DisplayType type) {
        dsLayer.getStormTrackState().displayType = type;
        dsLayer.issueRefresh();
    }

    private void updateLabelType(LabelMode mode) {
        dsLayer.getStormTrackState().labelMode = mode;
        dsLayer.issueRefresh();
    }

}
