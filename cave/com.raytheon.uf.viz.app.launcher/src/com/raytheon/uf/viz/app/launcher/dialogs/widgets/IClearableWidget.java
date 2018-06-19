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
package com.raytheon.uf.viz.app.launcher.dialogs.widgets;

/**
 * 
 * Specifies a mechanism for defining a widget as clearable. A clearable
 * widget is one that can clear its contents.
 * <P>
 * The idea is to support code similar to
 * <pre>
 *    List<IClearableWidget> clearables = new ArrayList<IClearableWidget>();
 *    ...
 *    private void clearDialog() {
 *       for (IClearableWigdet cw : clearables) {
 *          cw.clear();
 *       }
 *    }
 * </pre>
 * This clearDialog() method would "clear" the dialog's  clearable widgets without
 * knowing how to clear the individual widgets.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
public interface IClearableWidget {
    /**
     * Causes the widget to clear its contents. The exact meaning of "clear
     * its contents" is delegated to the widget.
     */
    public void clear();
}
