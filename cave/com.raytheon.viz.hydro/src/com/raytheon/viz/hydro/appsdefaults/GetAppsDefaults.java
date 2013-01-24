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
package com.raytheon.viz.hydro.appsdefaults;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to allow user to display desired application defaults.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2011            mpduff     Initial creation
 * Dec 07, 2012 1353       rferrel    Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GetAppsDefaults extends CaveSWTDialog {
    /**
     * Font used in the dialog.
     */
    private Font font;

    /**
     * The text area.
     */
    private Text textArea;

    /**
     * The search text field.
     */
    private Text searchField;

    /**
     * The search button.
     */
    private Button searchButton;

    private AppsDefaults appsDef = AppsDefaults.getInstance();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public GetAppsDefaults(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("GAD");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize text area
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 500;
        gd.heightHint = 250;
        textArea = new Text(shell, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        textArea.setFont(font);
        textArea.setEditable(false);
        textArea.setLayoutData(gd);
        textArea.getVerticalBar().setSelection(
                textArea.getVerticalBar().getMaximum());

        // Initialize search area
        Composite searchComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        searchComp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        searchComp.setLayoutData(gd);

        gd = new GridData(35, SWT.DEFAULT);
        gd.widthHint = 175;
        Label l = new Label(searchComp, SWT.NONE);
        l.setText("Token: ");
        searchField = new Text(searchComp, SWT.BORDER);
        searchField.setFont(font);
        searchField.setLayoutData(gd);
        searchField.setEditable(true);
        searchField.setFocus();
        searchField.addKeyListener(new org.eclipse.swt.events.KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if ((e.keyCode == 13) || (e.keyCode == 16777296)) {
                    gad();
                }
            }
        });

        gd = new GridData(35, SWT.DEFAULT);
        gd.widthHint = 65;
        searchButton = new Button(searchComp, SWT.NONE);
        searchButton.setText("Search");
        searchButton.setLayoutData(gd);
        searchButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                gad();
            }
        });

        // Close button
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl2 = new GridLayout(1, false);
        centeredComp.setLayout(gl2);
        GridData gd3 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        centeredComp.setLayoutData(gd3);

        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.setLayoutData(gd3);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    private void gad() {
        String token = searchField.getText();
        if ((token != null) && (token.length() > 0)) {
            String value = appsDef.getToken(token);
            String output = String.format("%-26s %s", token, value);
            textArea.append(output + "\n");
        }
    }
}
