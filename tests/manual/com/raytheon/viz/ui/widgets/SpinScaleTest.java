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
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SpinScaleTest {

    public static void main(String[] args) {
        class MyDialog extends Dialog {

            private SpinScale hscale;

            private SpinScale vscale;

            private int hscaleValue;

            private int vscaleValue;

            MyDialog(Shell parent) {
                super(parent);
            }

            @Override
            protected Control createDialogArea(Composite parent) {
                Composite comp = (Composite) super.createDialogArea(parent);

                hscale = new SpinScale(comp, SWT.HORIZONTAL);
                GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                gd.widthHint = 300;
                hscale.setLayoutData(gd);
                hscale.setValues(0, 0, 100, 1, 1, 10);

                hscale.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        hscaleValue = hscale.getSelection();
                    }

                });

                vscale = new SpinScale(comp, SWT.VERTICAL);
                gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
                gd.heightHint = 300;
                vscale.setLayoutData(gd);
                vscale.setValues(0, -100, 0, 1, 1, 10);

                vscale.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        vscaleValue = vscale.getSelection();
                    }

                });

                return comp;
            }

            public int gethScaleValue() {
                return hscaleValue;
            }

            public int getvScaleValue() {
                return vscaleValue;
            }

        }

        MyDialog dlg = new MyDialog((Shell) null);
        if (dlg.open() == IDialogConstants.OK_ID) {
            System.out.println("Horiz. Scale value: " + dlg.gethScaleValue());
            System.out.println("Vert.  Scale value: " + dlg.getvScaleValue());
        }
    }

}
