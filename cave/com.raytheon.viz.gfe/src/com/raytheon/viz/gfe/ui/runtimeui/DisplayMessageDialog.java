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
package com.raytheon.viz.gfe.ui.runtimeui;

import org.eclipse.jface.dialogs.MessageDialog;

import com.raytheon.uf.viz.core.VizApp;

/**
 * Wrapper around org.eclipse.jface.dialogs.MessageDialog to allow calling from
 * Python
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2015   #3955     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DisplayMessageDialog {

    /**
     * Convenience method to open a simple confirm (OK/Cancel) dialog.
     * 
     * @param title
     *            the dialog's title, or <code>null</code> if none
     * @param message
     *            the message
     * @return <code>true</code> if the user presses the OK button,
     *         <code>false</code> otherwise
     */
    public static boolean openConfirm(final String title, final String message) {
        final boolean[] status = new boolean[1];
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                status[0] = MessageDialog.openConfirm(null, title, message);
            }
        });
        return status[0];
    }

    /**
     * Convenience method to open a standard error dialog.
     * 
     * @param title
     *            the dialog's title, or <code>null</code> if none
     * @param message
     *            the message
     */
    public static void openError(final String title, final String message) {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                MessageDialog.openError(null, title, message);
            }
        });
    }

    /**
     * Convenience method to open a standard information dialog.
     * 
     * @param title
     *            the dialog's title, or <code>null</code> if none
     * @param message
     *            the message
     */
    public static void openInformation(final String title, final String message) {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                MessageDialog.openInformation(null, title, message);
            }
        });
    }

    /**
     * Convenience method to open a simple Yes/No question dialog.
     * 
     * @param title
     *            the dialog's title, or <code>null</code> if none
     * @param message
     *            the message
     * @return <code>true</code> if the user presses the Yes button,
     *         <code>false</code> otherwise
     */
    public static boolean openQuestion(final String title, final String message) {
        final boolean[] status = new boolean[1];
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                status[0] = MessageDialog.openQuestion(null, title, message);
            }
        });
        return status[0];
    }

    /**
     * Convenience method to open a standard warning dialog.
     * 
     * @param title
     *            the dialog's title, or <code>null</code> if none
     * @param message
     *            the message
     */
    public static void openWarning(final String title, final String message) {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                MessageDialog.openWarning(null, title, message);
            }
        });
    }
}
