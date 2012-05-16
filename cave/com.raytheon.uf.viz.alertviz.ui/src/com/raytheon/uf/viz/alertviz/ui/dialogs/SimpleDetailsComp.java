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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.message.StatusMessage;

/**
 * Provides a simple mechanism to view stacktraces
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2008            chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SimpleDetailsComp extends Composite {

    /**
     * Shell of the parent composite
     */
    private Shell shell = null;

    /**
     * Actually contains text of details message, lives in textBox
     */
    private StyledText st = null;

    /**
     * 
     * @param parent
     * @param style
     */
    public SimpleDetailsComp(Composite parent, int style) {
        super(parent, style);
        this.shell = parent.getShell();

        initComponents();
        setVisible(false);
    }

    /**
     * Lay out composite with StyledText
     */
    private void initComponents() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        setLayout(new GridLayout(1, false));
        setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 250;
        st = new StyledText(this, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        st.setEditable(false);

        st.setLayoutData(gd);

        Menu popupMenu = new Menu(st);
        MenuItem copyMI = new MenuItem(popupMenu, SWT.NONE);
        copyMI.setText("Copy Contents to Clipboard");
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                copyContents();
            }
        });

        st.setMenu(popupMenu);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Button copyBtn = new Button(this, SWT.PUSH);
        copyBtn.setText("Copy Contents to Clipboard");
        copyBtn.setLayoutData(gd);
        copyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                copyContents();
            }
        });
    }

    /**
     * Display details text in the StyledText box
     * 
     * @param sm
     *            status message to display
     */
    public void displayDetails(StatusMessage sm) {
        if (sm == null) {
            st.setText("");
        } else {
            StringBuilder sb = new StringBuilder();
            // sb.append(sm.getMessage()).append("\n");

            String details = sm.getDetails();
            String[] lines = details.split("[\n]");

            for (String line : lines) {
                if (line.length() > 500) {
                    sb.append(line.substring(0, 500)).append(
                            "...text truncated\n");

                } else {
                    sb.append(line).append("\n");
                }
            }
            st.setText(sb.toString());
        }
    }

    /**
     * Override so we can exclude this composite from the layout when hidden
     */
    @Override
    public void setVisible(boolean visible) {
        ((GridData) this.getLayoutData()).exclude = !visible;

        if (visible == true) {
            ((GridData) st.getLayoutData()).widthHint = getParent().getBounds().width - 46;
        }

        super.setVisible(visible);

        shell.layout();
        shell.pack();

    }

    /**
     * 
     */
    private void copyContents() {
        st.selectAll();
        st.copy();
    }
}
