package com.raytheon.viz.awipstools.ui.action;

import java.io.IOException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.viz.core.VizApp;

public class ArbitraryHandler extends AbstractHandler {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArbitraryHandler.class);

    private class ScrollableDialog extends TitleAreaDialog {
        private String title;

        private String text;

        private String scrollableText;

        public ScrollableDialog(Shell parentShell, String title, String text,
                String scrollableText) {
            super(parentShell);
            this.title = title;
            this.text = text;
            this.scrollableText = scrollableText;
        }

        @Override
        protected Control createDialogArea(Composite parent) {
            Composite composite = (Composite) super.createDialogArea(parent);

            GridData gridData = new GridData();
            gridData.grabExcessHorizontalSpace = true;
            gridData.horizontalAlignment = GridData.FILL;
            gridData.grabExcessVerticalSpace = true; // Layout vertically, too!
            gridData.verticalAlignment = GridData.FILL;

            Text scrollable = new Text(composite, SWT.BORDER | SWT.V_SCROLL);
            scrollable.setLayoutData(gridData);
            scrollable.setText(scrollableText);

            return composite;
        }

        @Override
        public void create() {
            super.create();

            // This is not necessary; the dialog will become bigger as the text
            // grows but at the same time,
            // the user will be able to see all (or at least more) of the error
            // message at once
            getShell().setSize(500, 500);
            setTitle(title);
            setMessage(text, IMessageProvider.INFORMATION);

        }

        @Override
        protected void createButtonsForButtonBar(Composite parent) {
            Button okButton = createButton(parent, OK, "OK", true);
            okButton.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    close();
                }
            });
        }

        @Override
        protected boolean isResizable() {
            return true; // Allow the user to change the dialog size!
        }
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        final String command = event.getParameter("commandAction");
        final String captureOutput = event.getParameter("captureOutput");
        final String showStdOut = event.getParameter("showStdOut");

        // execute in separate thread because capturing output will lock up cave
        // if run on ui thread.
        Thread t = new Thread(new Runnable() {

            @Override
            public void run() {
                // get booleans for optional parameters
                boolean doCapture = false;
                if (captureOutput != null
                        && captureOutput.toLowerCase().equals("true")) {
                    doCapture = true;
                }
                boolean doStdOut = false;
                if (showStdOut != null
                        && showStdOut.toLowerCase().equals("true")) {
                    doStdOut = true;
                }

                try {
                    if (command != null) {

                        // DR#10995
                        RunProcess p = RunProcess.getRunProcess().exec(command);

                        if (doCapture || doStdOut) {
                            final String stringOutput = p.getStdout();

                            if (doStdOut) {
                                System.out.println(stringOutput);
                            }

                            if (doCapture) {
                                VizApp.runAsync(new Runnable() {

                                    @Override
                                    public void run() {
                                        final ScrollableDialog dialog = new ScrollableDialog(
                                                new Shell(), "\'" + command
                                                        + "\' Output",
                                                "Output from execution:",
                                                stringOutput);
                                        dialog.open();
                                    }
                                });
                            }
                        }
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occurred launching custom command:", e);
                }
            }
        });

        t.start();

        return null;
    }
}
