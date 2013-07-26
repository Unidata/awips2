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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * DMD Time Height Close Dialog Message Box.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 25, 2011            mpduff     Initial creation
 * Dec 23, 2011 13608	   mgamazay	  Added initialization of ident to the constructor 
 * 24 Jul 2013 #2143       skorolev   Changes for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TimeHeightMsgBox extends CaveSWTDialog implements
        ICommonDialogAction {

    /** The ident of the cell. */
    private String ident = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param swtStyle
     */
    protected TimeHeightMsgBox(Shell parentShell, String ident) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MODELESS, CAVE.DO_NOT_BLOCK);
        setText("Unavailable T-H Trend");
        this.ident = ident;

    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org.eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createMessage();
        createButtons();
    }

    /**
     * Create Message.
     */
    private void createMessage() {
        Image img = Display.getCurrent().getSystemImage(SWT.ICON_QUESTION);

        Composite msgComp = new Composite(shell, SWT.NONE);
        msgComp.setLayout(new GridLayout(2, false));
        msgComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label imgLabel = new Label(msgComp, SWT.ICON);
        imgLabel.setImage(img);
        Label msgLabel = new Label(msgComp, SWT.NONE);

        String msg = "Extrapolated feature " + ident + " no longer \n";
        msg = msg
                .concat("valid in current volume scan!\n\nClose T-H trend window?");

        msgLabel.setText(msg);
    }

    /**
     * Create Buttons.
     */
    private void createButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, false,
                false));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 80;

        final Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(cancelBtn.getText());
                closeDialog();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        final Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                System.out.println("Close the TH dialog");
                setReturnValue(okBtn.getText());
                closeDialog();
            }
        });
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.scan.commondialogs.ICommonDialogAction#closeDialog()
     */
    @Override
    public void closeDialog() {
        close();
    }
}
