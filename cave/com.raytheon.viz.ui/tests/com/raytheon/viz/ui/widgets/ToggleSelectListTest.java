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
package com.raytheon.viz.ui.widgets;

import java.util.Arrays;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * Test driver for ToggleSelectlist
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2009      #2315 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ToggleSelectListTest {

    /**
     * @param args
     */
    public static void main(String[] args) {
        class MyDialog extends Dialog {

            private ToggleSelectList list;

            MyDialog(Shell parent) {
                super(parent);
            }

            @Override
            protected Control createDialogArea(Composite parent) {
                Composite comp = (Composite) super.createDialogArea(parent);

                list = new ToggleSelectList(comp, SWT.SINGLE | SWT.BORDER
                        | SWT.V_SCROLL);
                GridData data = new GridData(SWT.FILL, SWT.TOP, true, false);

                int count = 20;
                data.heightHint = list.getItemHeight() * Math.min(count, 10);
                list.setLayoutData(data);

                for (int i = 0; i < count; i++) {
                    list.add("Item " + i);
                }
                list.setSelection(new int[] { 4, 6, 8 });
                list.setTopIndex(list.getSelectionIndex());

                list.addSelectionListener(new SelectionAdapter() {

                    /*
                     * (non-Javadoc)
                     * 
                     * @see
                     * org.eclipse.swt.events.SelectionAdapter#widgetSelected
                     * (org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        System.out.println(Arrays.toString(list
                                .getSelectionIndices()));
                    }

                });
                return comp;
            }

            /*
             * (non-Javadoc)
             * 
             * @see org.eclipse.jface.dialogs.Dialog#okPressed()
             */
            @Override
            protected void okPressed() {
                System.out.println(Arrays.toString(list.getSelectionIndices()));
                super.okPressed();
            }

        }

        MyDialog dlg = new MyDialog((Shell) null);
        dlg.setBlockOnOpen(true);
        dlg.open();
    }

}
