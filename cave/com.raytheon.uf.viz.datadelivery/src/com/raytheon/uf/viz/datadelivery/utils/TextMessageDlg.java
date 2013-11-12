package com.raytheon.uf.viz.datadelivery.utils;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Replace a message box for messages with too big number of lines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 7, 2013            skorolev     Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class TextMessageDlg extends CaveSWTDialog {

    private String messageText;

    protected TextMessageDlg(Shell parentShell, String messageTitle,
            String messageText) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        this.messageText = messageText;
        this.setText(messageTitle);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        GridLayout gl = new GridLayout(1, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        gd = new GridData();
        gd.widthHint = 350;
        gd.heightHint = 350;
        StyledText text = new StyledText(mainComp, SWT.MULTI | SWT.WRAP
                | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        text.setLayoutData(gd);
        text.setText(messageText);

        Button okBtn = new Button(mainComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, true));
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

}
