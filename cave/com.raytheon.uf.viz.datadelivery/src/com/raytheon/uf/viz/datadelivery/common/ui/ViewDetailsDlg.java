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
package com.raytheon.uf.viz.datadelivery.common.ui;

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

/**
 * Data Delivery GUI Details Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012                 Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */

public class ViewDetailsDlg extends AbstractViewDlg {

    /** Styled text control. */
    private StyledText stText;

    /** Text font */
    private Font textFont;

    /** Details to be displayed */
    private String detailsStr;

    /** Dialog width */
    private int dialogWidth;

    /** Dialog height */
    private int dialogHeight;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param detailsStr
     *            Details string.
     * @param titleStr
     *            Title string.
     * @param callback
     *            Dialog closed callback.
     * @param id
     *            ID string to give this dialog a unique ID.
     */
    public ViewDetailsDlg(Shell parentShell, String detailsStr, String titleStr, IDialogClosed callback, String id) {
        // Call other constructor with a default size of 600x350
        this(parentShell, detailsStr, titleStr, 600, 350, callback, id);
    }

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param detailsStr
     *            Details string.
     * @param titleStr
     *            Title string.
     * @param width
     *            Dialog width.
     * @param height
     *            Dialog height.
     * @param callback
     *            Dialog closed callback.
     * @param id
     *            ID string to give this dialog a unique ID.
     */
    public ViewDetailsDlg(Shell parentShell, String detailsStr, String titleStr, int width, int height,
            IDialogClosed callback, String id) {
        super(parentShell, callback, null, id);

        this.detailsStr = detailsStr;
        this.dialogWidth = width;
        this.dialogHeight = height;

        if (titleStr == null) {
            titleStr = "Details";
        }
        setText(titleStr);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        textFont.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        shell.setMinimumSize(300, 300);
        createTextControl();
        createCloseButton();
    }

    /**
     * Create the text control.
     */
    private void createTextControl() {
        textFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = dialogHeight;
        gd.widthHint = dialogWidth;
        stText = new StyledText(shell, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        stText.setFont(textFont);
        stText.setLayoutData(gd);
        stText.setEditable(false);

        stText.setText(detailsStr);
    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        gd = new GridData(75, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }
}
