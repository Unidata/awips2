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
package com.raytheon.viz.texteditor.scripting.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Implements the Script Output Dialog for the Text Workstation Script running
 * capability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009            mfegan     Initial creation
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class ScriptOutputDlg extends CaveSWTDialog implements SelectionListener {
    private static final String DLG_TITLE_FMT = "Text %s Script Output";

    /** the window displaying the script output */
    private Text outputDisplay = null;

    /**
     * @param parentShell
     */
    public ScriptOutputDlg(Shell parent, String token) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT);
        setText(String.format(DLG_TITLE_FMT, token));
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout layout = new GridLayout(1, false);
        layout.marginHeight = 2;
        layout.marginLeft = 0;
        layout.marginRight = 0;
        layout.verticalSpacing = 1;
        return layout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Point size = new Point(500, 300);
        shell.setSize(size);

        outputDisplay = new Text(shell, SWT.BORDER | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.MULTI);
        outputDisplay.setLayoutData(new GridData(GridData.FILL_BOTH));

        Canvas cv = new Canvas(shell, SWT.NONE);
        GridLayout layout = new GridLayout(2, true);
        cv.setLayout(layout);
        cv.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_CENTER));

        GridData gd = new GridData(SWT.CENTER);
        gd.widthHint = 50;
        Button btn = new Button(cv, SWT.PUSH);
        btn.setText("Clear");
        btn.setLayoutData(gd);
        btn.addSelectionListener(this);

        btn = new Button(cv, SWT.PUSH);
        btn.setText("Close");
        btn.setLayoutData(gd);
        btn.addSelectionListener(this);
    }

    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        // intentionally empty
    }

    @Override
    public void widgetSelected(SelectionEvent e) {
        Object object = e.getSource();
        if (!(object instanceof Button)) {
            return;
        }
        Button source = (Button) object;
        String name = source.getText();
        System.out.println("widgetSelected(): " + name);
        if ("clear".equalsIgnoreCase(name)) {
            outputDisplay.setText("");
        } else if ("close".equalsIgnoreCase(name)) {
            shell.dispose();
        } else {
            // intentionally empty -- for future use
        }

    }

    /**
     * Appends a text message to the display
     * 
     * @param msg
     *            the message to append
     */
    public void addMessage(String msg) {
        outputDisplay.append(msg);
    }

    /**
     * Appends a message to the display, and then appends a new line.
     * 
     * @param msg
     *            the message to append
     */
    public void addNlMessage(String msg) {
        outputDisplay.append(msg + System.getProperty("line.separator"));
    }
}
