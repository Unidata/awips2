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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class LabeledScaleTest {

    public static void main(String[] args) {
        class MyDialog extends Dialog {

            private LabeledScale scale;

            private int scaleValue;

            MyDialog(Shell parent) {
                super(parent);
            }

            @Override
            protected Control createDialogArea(Composite parent) {
                Composite comp = (Composite) super.createDialogArea(parent);

                scale = new LabeledScale(comp);
                GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                gd.widthHint = 300;
                scale.setLayoutData(gd);

                scale.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        scaleValue = scale.getSelection();
                    }

                });

                // final Font font = new Font(parent.getDisplay(), "Monospace",
                // 14, SWT.NORMAL);
                // scale.setFont(font);
                //
                // comp.addDisposeListener(new DisposeListener() {
                //
                // @Override
                // public void widgetDisposed(DisposeEvent e) {
                // font.dispose();
                // }
                //
                // });

                return comp;
            }

            public int getScaleValue() {
                return scaleValue;
            }

        }

        MyDialog dlg = new MyDialog((Shell) null);
        if (dlg.open() == IDialogConstants.OK_ID) {
            System.out.println("Scale value: " + dlg.getScaleValue());
        }
    }
}
