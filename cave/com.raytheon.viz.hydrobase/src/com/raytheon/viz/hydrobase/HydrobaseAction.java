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
package com.raytheon.viz.hydrobase;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;

/**
 * Action for unimplemented features. To be used temporarily until final
 * behavior is implemented.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/27/06                  lvenable    Initial Creation.
 * 6/21/2010   #5412        lbousaid    Added database name
 * 07/03/2010   6586        mduff       Fixed problem introduced from the 
 *                                      CaveSWTDialog refactor and added
 *                                      a pre-selected lid.
 * 06/27/2013   2088        rferrel     Changes for non-blocking HydroBaseDlg.
 * 15/01/2015   5054        randerso    Remove unnecessary new Shell
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class HydrobaseAction extends AbstractHandler {
    private HydroBaseDlg hydrobaseDlg;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (hydrobaseDlg == null || hydrobaseDlg.isDisposed()) {
            Shell shell = HandlerUtil.getActiveShell(event);
            String lid = HydroDisplayManager.getInstance().getCurrentLid();
            String dbName = AppsDefaults.getInstance().getToken("db_name");
            hydrobaseDlg = new HydroBaseDlg(shell, dbName, lid);
            boolean verified = hydrobaseDlg.promptForPassword(shell);

            if (verified) {
                hydrobaseDlg.open();
            } else {
                hydrobaseDlg = null;
            }
        } else {
            hydrobaseDlg.bringToTop();
        }

        return null;
    }

}
