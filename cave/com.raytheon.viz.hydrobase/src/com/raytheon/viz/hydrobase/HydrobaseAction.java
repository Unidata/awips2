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
import org.eclipse.ui.PlatformUI;

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
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  lvenable    Initial Creation.
 * 6/21/2010	#5412		lbousaid    Added database name	
 * 07/03/2010   6586        mduff       Fixed problem introduced from the 
 *                                      CaveSWTDialog refactor and added
 *                                      a pre-selected lid.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class HydrobaseAction extends AbstractHandler {
    
	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException {
	    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
	    
	    String lid = HydroDisplayManager.getInstance().getCurrentLid();
	    String dbName = AppsDefaults.getInstance().getToken("db_name");
	    HydroBaseDlg hydrobaseDlg = new HydroBaseDlg(shell, dbName, lid);
	    boolean verified = hydrobaseDlg.promptForPassword(new Shell());
	    
	    if (verified) {
	        hydrobaseDlg.open();
	    }
	    
		return null;
	}

}
