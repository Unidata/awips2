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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.ui.layer.TimeOfArrivalLayer;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description SkewtParamsDialog.java Nov 28, 2007
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Nov 28, 2007					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class TimeOfArrivalDialog extends CaveJFACEDialog {

    public String[] paramData;

    public String dialogTitle;

    private Composite top = null;

    private TimeOfArrivalLayer toaLayer;

    private Button pointRadio, polyRadio, circRadio;

    public TimeOfArrivalDialog(Shell parShell, TimeOfArrivalLayer toaLayer) {
        super(parShell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.toaLayer = toaLayer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        top.setLayout(new GridLayout(2, true));

        initializeComponents();
        return top;
    }

    private void initializeComponents() {
        Label pntModeLabel = new Label(top, SWT.NONE);
        pntModeLabel.setText("Point Mode:");
        Label plyModeLabel = new Label(top, SWT.NONE);
        plyModeLabel.setText("Polyline Mode:");
        pointRadio = new Button(top, SWT.RADIO);
        pointRadio.setText("Point");
        pointRadio.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent arg0) {
                updateDisplayType(DisplayType.POINT);
            }
        });

        polyRadio = new Button(top, SWT.RADIO);
        polyRadio.setText("Polyline");
        polyRadio.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent arg0) {
                updateDisplayType(DisplayType.POLY);
            }
        });

        circRadio = new Button(top, SWT.RADIO);
        circRadio.setText("Circular Front");
        circRadio.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent arg0) {
                updateDisplayType(DisplayType.CIRCULAR);
            }
        });

        switch (toaLayer.getStormTrackState().displayType) {
        case CIRCULAR: {
            circRadio.setSelection(true);
            break;
        }
        case POINT: {
            pointRadio.setSelection(true);
            break;
        }
        case POLY: {
            polyRadio.setSelection(true);
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
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("TOA / LT");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {
        return new Point(300, 200);
    }

    private void updateDisplayType(DisplayType type) {
        toaLayer.getStormTrackState().displayType = type;
        toaLayer.getStormTrackState().geomChanged = true;
        toaLayer.issueRefresh();
    }
}
