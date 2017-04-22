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
/**
 * 
 */
package com.raytheon.viz.xdat;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;

/**
 * Action for unimplemented features. To be used temporarily until final
 * behavior is implemented.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 6/27/06                 lvenable    Initial Creation.
 * 3/9/09                  wkwock      eliminate the DB name argument in calling XdatDlg.
 *                                     XdatDlg extracts the DB name from the URL.
 * 01/26/2016   5054       randerso    Made XdatDlg parented to display
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class XdatAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        XdatDlg xdatDlg = new XdatDlg(Display.getCurrent());
        xdatDlg.open();

        return null;
    }

}
