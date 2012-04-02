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
package com.raytheon.uf.viz.monitor.scan.tables;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager.AlertedAlarms;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Shows what alarms are currently going off
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2010            mnash     Initial creation
 * 
 * 03/15/2012	13939	   Mike Duff    For a SCAN Alarms issue
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SCANAlarmsDlg extends CaveSWTDialog {

    private ScrolledComposite sc;

    private Composite btnComp;

    private int buttonWidth = 150;

    private SCANAlarmAlertManager mgr;

    private ScanTables type;

    private Button clearAllBtn;

    private String site;

    /**
     * @param parentShell
     */
    protected SCANAlarmsDlg(Shell parentShell, ScanTables scanTable, String site) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        this.site = site;
        type = scanTable;
        mgr = SCANAlarmAlertManager.getInstance(site);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        if (type == ScanTables.CELL) {
            setText("Cell Alarms");
        } else if (type == ScanTables.DMD) {
            setText("DMD Alarms");
        }

        // Initialize data and all of the controls and layouts
        createAlarmButtons(shell);
        createClearAllButton(shell);
    }

    private void createAlarmButtons(final Shell shell) {
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        comp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayoutData(gd);

        sc = new ScrolledComposite(comp, SWT.BORDER | SWT.V_SCROLL);
        sc.setAlwaysShowScrollBars(true);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        sc.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 160;
        gd.heightHint = 325;
        sc.setLayoutData(gd);

        btnComp = new Composite(sc, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        btnComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        btnComp.setLayoutData(gd);

        for (final AlertedAlarms alarm : mgr.getAlertedAlarms(site, type)) {
            if (alarm.cleared == false) {
                gd = new GridData(buttonWidth, SWT.DEFAULT);
                final Button btn = new Button(btnComp, SWT.PUSH);
                btn.setText(alarm.ident + " --> " + alarm.colName + " "
                        + alarm.type.getName());
                btn.setData("ident", alarm.ident);
                btn.setData("attr", alarm.colName);
                btn.setLayoutData(gd);
                btn.setBackground(Display.getDefault()
                        .getSystemColor(SWT.COLOR_RED));
                btn.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        String ident = btn.getData("ident").toString();
                        String attr = btn.getData("attr").toString();
                        // get instance of the table dialog to use for the functions
                        AbstractTableDlg tableDlg = ScanMonitor.getInstance()
                                .getDialog(type, site);
                        // display the trend graph dialogs
                        if (tableDlg instanceof SCANCellTableDlg) {
                            ((SCANCellTableDlg) tableDlg).getScanTableComp()
                                    .displayTrendGraphDialog(ident, attr);
                        } else if (tableDlg instanceof SCANDmdTableDlg) {
                            ((SCANDmdTableDlg) tableDlg).getScanTableComp()
                                    .displayTrendGraphDialog(ident, attr);
                        }
                        // recenter the map on the ident
                        tableDlg.fireRecenter(ident, type, site);
                        // highlight the correct row
                        // TODO highlight the row 
                        
                        if (tableDlg instanceof SCANDmdTableDlg) {
                            ((SCANDmdTableDlg) tableDlg).alarmSelection(ident);
                        }
    
                        tableDlg.mgr.clearAlarm(site, type, alarm);
                        clearAllBtn.setText("Clear All "
                                + mgr.getAlertedAlarms(site, type).size()
                                + " Alarms");
                        // remove the btn
                        btn.dispose();
                        btnComp.layout();
                        if (mgr.getAlertedAlarms(site, type).size() <= 0) {
                            shell.dispose();
                        }
                    }
                });
            }
        }

        btnComp.layout();

        sc.setContent(btnComp);
        sc.setExpandHorizontal(true);
        sc.setExpandVertical(true);
        sc.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                Rectangle r = sc.getClientArea();
                sc.setMinSize(btnComp.computeSize(r.width, SWT.DEFAULT));
            }
        });
        sc.layout();
    }

    private void createClearAllButton(final Shell shell) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth + 30;
        clearAllBtn = new Button(shell, SWT.PUSH);
        int numAlarms = 0;
        for (AlertedAlarms alarm: mgr.getAlertedAlarms(site, type)) {
            if (alarm.cleared == false) {
                numAlarms++;
            }
        }
        clearAllBtn.setText("Clear All " + numAlarms + " Alarms");
        clearAllBtn.setBackground(Display.getDefault().getSystemColor(
                SWT.COLOR_RED));
        clearAllBtn.setLayoutData(gd);
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                mgr.clearAlertedAlarms(site, type);
                shell.dispose();
            }
        });
    }

    public void closeDisplay() {
        shell.dispose();
    }
}
