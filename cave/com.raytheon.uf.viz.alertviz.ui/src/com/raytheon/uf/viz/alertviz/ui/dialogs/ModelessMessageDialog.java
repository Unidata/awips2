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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

/**
 * Modeless MessageDialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2011            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ModelessMessageDialog extends
        org.eclipse.jface.dialogs.MessageDialog {

    private static String[] buttons = new String[] { "YES", "NO" };

    private int style;

    public ModelessMessageDialog(Shell parent, String title, String message) {
        this(parent, title, message, SWT.DIALOG_TRIM);
    }

    public ModelessMessageDialog(Shell parent, String title, String message,
            int style) {
        super(parent, title, null, message, MessageDialog.QUESTION, buttons, 1);
        this.style = style;
    }

    public int open() {
        setShellStyle(style);

        if (super.open() == 1) {
            return SWT.NO;
        } else {
            return SWT.YES;
        }
    }
}
