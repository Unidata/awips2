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

package gov.noaa.nws.ncep.viz.ui.perspectives;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.ui.dialogs.PreferencesUtil;

/**
 * Opens the preference window
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2007            chammack    Initial Creation.
 * Not really created by chammack.
 * Nov 15, 2013            G. Hull     changed id to gov.noaa.nws.ncep.ui.pgen.PgenPreferences
 *                                     (No longer getting called from PGEN Layer Link Option.)   
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class PreferencesHandler extends AbstractHandler  {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	String prefPageId = "gov.noaa.nws.ncep.ui.pgen.PgenPreferences";
    	
        PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(
                null, prefPageId, new String[] { prefPageId }, null);
        dialog.open();
        return null;
    }

}
