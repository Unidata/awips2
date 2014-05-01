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
package com.raytheon.uf.viz.collaboration.ui.session;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class SessionMsgArchiveDialog extends Dialog {

    private Shell shell;

    /**
     * @param parent
     */
    public SessionMsgArchiveDialog(Shell parent) {
        super(parent);
    }

    /**
     * @param parent
     * @param style
     */
    public SessionMsgArchiveDialog(Shell parent, int style) {
        super(parent, style);
    }

    public void open(LocalizationFile logDir) {
        Shell parent = getParent();

        shell = new Shell(parent.getDisplay(), SWT.TITLE | SWT.RESIZE);
        shell.setText(getText());
        shell.setLayout(new GridLayout(1, false));

        SessionMsgArchiveBrowser smab = new SessionMsgArchiveBrowser(shell,
                SWT.NONE, logDir);

        shell.pack();
        shell.open();

        // Wait until the shell is disposed.
        Display display = parent.getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

    }
}
