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
package com.raytheon.viz.hydro.contacts;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.contacts.ContactsDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * 07/10/2013   2088        rferrel     Changes for non-blocking ContactsDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class ContactsAction extends AbstractHandler {
    /**
     * Allow only one dialog per station.
     */
    private final Map<String, ContactsDlg> contactsDlgMap = new HashMap<String, ContactsDlg>();

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        // Assume when false error message already displayed.
        if (HydroDisplayManager.getInstance().isCurrentLidSelected(shell) == false) {
            return null;
        }

        String lid = HydroDisplayManager.getInstance().getCurrentLid();

        ContactsDlg contactsDlg = contactsDlgMap.get(lid);
        if (contactsDlg == null || contactsDlg.isDisposed()) {

            String name = HydroDisplayManager.getInstance().getCurrentData()
                    .getName();
            StringBuilder title = new StringBuilder(" - ");

            title.append(lid);

            if (name != null && name.compareTo("") != 0) {
                title.append(" - ").append(name);
            }

            contactsDlg = new ContactsDlg(shell, title.toString(), false, lid);
            contactsDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String lid = returnValue.toString();
                        contactsDlgMap.remove(lid);
                    }
                }
            });
            contactsDlgMap.put(lid, contactsDlg);
            contactsDlg.open();
        } else {
            contactsDlg.bringToTop();
        }
        return null;
    }
}
