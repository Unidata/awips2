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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.services.ISourceProviderService;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 05, 2015 #14246     lbousaidi     Initial creation
 * 
 * </pre>
 * 
 * @author lbousaidi
 * @version 1.0
 */

public class TransmitBestEstimateQPEProvider extends AbstractSourceProvider {

    private static final String[] MENU_ENABLED = new String[] { "com.raytheon.viz.mpe.ui.transmitBestEstimateQPE" };

    private final boolean enabled = false;

    private final Map<String, Boolean> sourceMap = new HashMap<String, Boolean>();

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
        sourceMap.put(MENU_ENABLED[0], enabled);
        return sourceMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISourceProvider#getProvidedSourceNames()
     */
    @Override
    public String[] getProvidedSourceNames() {
        // TODO Auto-generated method stub
        return MENU_ENABLED;
    }
    
    public static void setEnabled(boolean enabled) {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        boolean transmitFlag=appsDefaults.getBoolean("mpe_send_qpe_to_sbn", false);

        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                        .getCurrentWindow();
        if (window != null) {
                ISourceProviderService service = (ISourceProviderService) window
                                .getService(ISourceProviderService.class);
                TransmitBestEstimateQPEProvider provider =
                        (TransmitBestEstimateQPEProvider) service
                                .getSourceProvider(MENU_ENABLED[0]);

                Map<String, Boolean> sourceMap = new HashMap<String, Boolean>();
                sourceMap.put(MENU_ENABLED[0], transmitFlag?enabled:false);

                provider.fireSourceChanged(ISources.ACTIVE_WORKBENCH_WINDOW,
                                sourceMap);
        }
    }

    public static TransmitBestEstimateQPEProvider getProvider(ExecutionEvent event) {
        ISourceProviderService service = (ISourceProviderService) HandlerUtil
                        .getActiveWorkbenchWindow(event).getService(
                                        ISourceProviderService.class);
        return (TransmitBestEstimateQPEProvider) service
                        .getSourceProvider(MENU_ENABLED[0]);
    }

}
