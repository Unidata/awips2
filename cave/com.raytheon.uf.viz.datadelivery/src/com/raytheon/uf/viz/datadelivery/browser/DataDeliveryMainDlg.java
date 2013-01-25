package com.raytheon.uf.viz.datadelivery.browser;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

public class DataDeliveryMainDlg {
    private Display display;

    private Shell shell;

    private DataBrowserDlg dataBroswerDlg;

    public DataDeliveryMainDlg() {
        display = new Display();
        shell = new Shell(display);
    }

    public void run() {
        shell.setLayout(new GridLayout(1, true));

        createButtons();

        shell.setSize(400, 250);
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }

        display.dispose();
    }

    private void createButtons() {
        Button ddBrowserDlgBtn = new Button(shell, SWT.PUSH);
        ddBrowserDlgBtn.setText("Data Browser Dialog...");
        ddBrowserDlgBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (dataBroswerDlg == null || dataBroswerDlg.isDisposed()) {
                    dataBroswerDlg = new DataBrowserDlg(shell);
                    dataBroswerDlg.open();
                }
            }
        });
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        DataDeliveryMainDlg hb = new DataDeliveryMainDlg();
        hb.run();

    }
}
