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
package com.raytheon.viz.mpe.ui;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.services.ISourceProviderService;

import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Provider for enabling/disabling change time menus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ChangeTimeProvider extends AbstractSourceProvider {

    private static final String[] MENU_ENABLED = new String[] {
            "com.raytheon.viz.mpe.ui.previousHour",
            "com.raytheon.viz.mpe.ui.nextHour" };

    private Map<String, Boolean> sourceMap = new HashMap<String, Boolean>();

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#dispose()
     */
    @Override
    public void dispose() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getCurrentState()
     */
    @Override
    public Map<String, Boolean> getCurrentState() {
        return getCurrentState(MPEDisplayManager.getCurrent());
    }

    public Map<String, Boolean> getCurrentState(MPEDisplayManager mgr) {
        if (mgr == null) {
            sourceMap.put(MENU_ENABLED[0], false);
            sourceMap.put(MENU_ENABLED[1], false);
            return sourceMap;
        }
        MPEDataManager dmMgr = MPEDataManager.getInstance();
        if (dmMgr == null) {
            sourceMap.put(MENU_ENABLED[0], false);
            sourceMap.put(MENU_ENABLED[1], false);
        } else {
            Date firstDate = dmMgr.getEarliestDate();
            Date curDate = mgr.getCurrentEditDate();
            Date latestDate = dmMgr.getLatestDate();
            boolean enabled = false;
            if (curDate != null && firstDate != null
                    && curDate.after(firstDate)) {
                enabled = true;
            }
            sourceMap.put(MENU_ENABLED[0], enabled);
            enabled = false;
            if (curDate != null && latestDate != null
                    && curDate.before(latestDate)) {
                enabled = true;
            }
            sourceMap.put(MENU_ENABLED[1], enabled);
        }
        return sourceMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
     */
    @Override
    public String[] getProvidedSourceNames() {
        return MENU_ENABLED;
    }

    public static void update(MPEDisplayManager displayMgr) {
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        if (window != null) {
            ISourceProviderService service = (ISourceProviderService) window
                    .getService(ISourceProviderService.class);
            ChangeTimeProvider provider = (ChangeTimeProvider) service
                    .getSourceProvider(MENU_ENABLED[0]);
            provider.fireSourceChanged(ISources.ACTIVE_WORKBENCH_WINDOW,
                    provider.getCurrentState(displayMgr));
        }
    }
}
