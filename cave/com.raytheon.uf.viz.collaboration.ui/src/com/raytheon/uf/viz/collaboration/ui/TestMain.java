package com.raytheon.uf.viz.collaboration.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;

public class TestMain {
    private static CollaborationUsersDlg userDlg;

    /**
     * @param args
     */

    public static void main(String[] args) {
        Display display = new Display();
        final Shell shell = new Shell(display);
        Button button = new Button(shell, SWT.PUSH);
        button.setText("Log on dialog");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent ev) {
                super.widgetSelected(ev);
                LoginDialog d = new LoginDialog(shell);
                Object r = d.open();
                System.err.println("Result: "
                        + ((r == null) ? null : r.toString()));
            }
        });

        button = new Button(shell, SWT.PUSH);
        button.setText("Contacts");
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent ev) {
                super.widgetSelected(ev);
                if (userDlg == null || userDlg.isDisposed()) {
                    userDlg = new CollaborationUsersDlg(shell);
                    userDlg.open();
                } else {
                    userDlg.bringToTop();
                }
            }
        });

        shell.setDefaultButton(button);
        shell.setLayout(new RowLayout());
        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
        display.dispose();
    }

}
