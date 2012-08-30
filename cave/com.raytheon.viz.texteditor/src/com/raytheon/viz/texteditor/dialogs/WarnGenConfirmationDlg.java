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

package com.raytheon.viz.texteditor.dialogs;

import java.io.InputStream;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Main Text Editor dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 27Jul2010    4773        cjeanbap    Initial development
 * 10Aug2010    2187        cjeanbap    Removed warnGenFlag.
 * 10Nov2011    11552       rferrel     returnvalue no longer null
 * 08/20/2012   DR 15340    D. Friedman Use callbacks for closing
 * 
 * </pre>
 * 
 * @author cjeanbap
 */
public class WarnGenConfirmationDlg extends CaveSWTDialog {

    private String productMessage;

    private String modeMessage;

    private CAVEMode mode;

    private String IMAGE_OPERATIONAL = "res/images/twsOper.gif";

    private String IMAGE_TEST = "res/images/twsTest.gif";

    private String IMAGE_PRACTICE = "res/images/twsPractice.gif";
    
    public static interface SessionDelegate {
        void dialogDismissed(Object result);
    }
    
    private SessionDelegate sessionDelegate;

    protected WarnGenConfirmationDlg(Shell parentShell, String title,
            String productMessage, String modeMessage, CAVEMode mode) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL, 
                CAVE.NONE | CAVE.DO_NOT_BLOCK);

        setText(title);

        this.productMessage = productMessage;
        this.modeMessage = modeMessage;
        this.mode = mode;
        setReturnValue(Boolean.FALSE);
    }
    
    public void open(SessionDelegate sessionDelegate) {
        if (sessionDelegate != null && isOpen())
            throw new RuntimeException(String.format("Dialog \"%s\" already open", getText()));
        this.sessionDelegate = sessionDelegate;
        super.open();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComposite = new Composite(shell, SWT.NONE);
        mainComposite.setLayout(new GridLayout(1, false));
        createImage(mainComposite);
        createMessageLabel(mainComposite);
        createButtonRow(mainComposite);
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (sessionDelegate != null)
                    sessionDelegate.dialogDismissed(getReturnValue());
            }
        });
    }

    private void createImage(Composite mainComposite) {
        InputStream is = null;

        ClassLoader cl = WarnGenConfirmationDlg.class.getClassLoader();

        if (mode.equals(CAVEMode.OPERATIONAL)) {
            // add Live image
            is = cl.getResourceAsStream(IMAGE_OPERATIONAL);
        } else if (mode.equals(CAVEMode.TEST)) {
            // add Test image
            is = cl.getResourceAsStream(IMAGE_TEST);
        } else if (mode.equals(CAVEMode.PRACTICE)) {
            // add Practice image
            is = cl.getResourceAsStream(IMAGE_PRACTICE);
        } else {
            // unknown
            is = cl.getResourceAsStream(IMAGE_OPERATIONAL);
        }

        Image stopSign = new Image(mainComposite.getDisplay(), is);
        Label stopSignLbl = new Label(mainComposite, 0);
        stopSignLbl.setImage(stopSign);
    }

    private void createMessageLabel(Composite mainComposite) {
        Label productMsgLbl = new Label(mainComposite, 0);
        productMsgLbl.setText(this.productMessage);
        Label modeMsgLbl = new Label(mainComposite, 0);
        modeMsgLbl.setText(this.modeMessage);

        Label sepLbl = new Label(mainComposite, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
    }

    private void createButtonRow(Composite mainComposite) {
        Composite buttonRowComp = new Composite(mainComposite, SWT.NONE);
        buttonRowComp.setLayout(new GridLayout(2, true));
        buttonRowComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Add the Go Ahead (Save) button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button saveBtn = new Button(buttonRowComp, SWT.PUSH);
        saveBtn.setText("Go Ahead!");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose(Boolean.TRUE);
            }
        });

        // Add the Abort button.
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button abortCutBtn = new Button(buttonRowComp, SWT.PUSH);
        abortCutBtn.setText("Abort");
        abortCutBtn.setLayoutData(gd);
        abortCutBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                dispose(Boolean.FALSE);
            }
        });
    }

    private void dispose(Boolean returnValue) {
        setReturnValue(returnValue);
        shell.dispose();
    }
}
