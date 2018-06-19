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

import com.raytheon.uf.common.message.StatusMessage;

/**
 * Provides a simple mechanism to view stacktraces
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 09, 2008            chammack    Initial creation
 * Jun 29, 2015  4311      randerso    Reworking AlertViz dialogs to be resizable.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SimpleDetailsComp extends Composite {

    private static final int NUM_DETAIL_LINES = 13;

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
        initComponents();
    }

    /**
     * Lay out composite with StyledText
     */
    private void initComponents() {
        GridLayout layout = new GridLayout(1, false);
        setLayout(layout);

        st = new StyledText(this, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER
                | SWT.WRAP);
        st.setEditable(false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = (st.getLineHeight() * NUM_DETAIL_LINES)
                + (st.getBorderWidth() * 2);
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
            st.setText(sm.getDetails());
        }
    }

    /**
     * 
     */
    private void copyContents() {
        st.selectAll();
        st.copy();
    }
}
