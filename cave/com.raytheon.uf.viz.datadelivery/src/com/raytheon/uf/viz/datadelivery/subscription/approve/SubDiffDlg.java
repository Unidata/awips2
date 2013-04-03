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
package com.raytheon.uf.viz.datadelivery.subscription.approve;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to show the difference between an existing subscription and a
 * pending subscription.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class SubDiffDlg extends CaveSWTDialog {
    private Font textFont;

    private StyledText stText1;
    
    private String details1;
    
    /**
     * @param parentShell
     */
    protected SubDiffDlg(Shell parentShell, String details1) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE);
        this.details1 = details1;
        setText("Subscription Differences");
    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;

        return mainLayout;
    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org.eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        shell.setMinimumSize(600, 300);
        createTextArea();
        
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);

        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(gl);
        comp.setLayoutData(gd);

        GridData btnData = new GridData(90, SWT.DEFAULT);

        Button b = new Button(comp, SWT.PUSH);
        b.setText("Close");
        b.setLayoutData(btnData);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }     
        });
    }
    
    private void createTextArea() {
        textFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        Composite comp1 = new Composite(shell, SWT.NONE);
        comp1.setLayout(gl);
        comp1.setLayoutData(gd);
        
        stText1 = new StyledText(comp1, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        stText1.setFont(textFont);
        stText1.setLayoutData(gd);
        stText1.setEditable(false);
        stText1.setText(details1);
    }

    @Override
    protected void disposed() {
        textFont.dispose();
    }
}
