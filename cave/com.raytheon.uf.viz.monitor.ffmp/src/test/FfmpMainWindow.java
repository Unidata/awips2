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
package test;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpBasinTableDlg;

public class FfmpMainWindow {
    private Display display;

    private Shell shell;

    private FFMPTableData tData;

    private FfmpBasinTableDlg ffmp;

    public FfmpMainWindow() {
        display = new Display();
        shell = new Shell(display);
    }

    public void run() {
        shell.setLayout(new GridLayout(1, true));

        addDisplayButtons();

        shell.setSize(500, 500);

        // shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }

        display.dispose();
    }

    private void addDisplayButtons() {

        Button ffmpDlg = new Button(shell, SWT.PUSH);
        ffmpDlg.setText("FFMP Dlg...");
        ffmpDlg.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                createTableData(CommonConfig.AppName.FFMP);
                ffmp = new FfmpBasinTableDlg(shell, tData, null);
                ffmp.open();
            }
        });
    }

    private void createTableData(CommonConfig.AppName app) {
        tData = null;
        // DataGenerator dg = new DataGenerator();

        // tData = dg.generateFFMPData();
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        FfmpMainWindow mainWin = new FfmpMainWindow();
        mainWin.run();

    }
}
