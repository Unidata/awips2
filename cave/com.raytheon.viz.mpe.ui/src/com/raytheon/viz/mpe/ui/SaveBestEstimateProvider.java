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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.ISourceProviderService;

/**
 * Source Provider for best estimate menu enablement
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep  2, 2010            mschenke    Initial creation
 * Feb 26, 2014     2842   mpduff      Use PlatformUI rather than HandlerUtil.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SaveBestEstimateProvider extends AbstractSourceProvider {

    private static final String[] MENU_ENABLED = new String[] { "com.raytheon.viz.mpe.ui.saveBestEstBottom" };

    private final Map<String, Boolean> sourceMap = new HashMap<String, Boolean>();

    private boolean enabled = false;

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
        return MENU_ENABLED;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
        fireSourceChanged(ISources.ACTIVE_WORKBENCH_WINDOW, getCurrentState());
    }

    public static SaveBestEstimateProvider getProvider(ExecutionEvent event) {
        ISourceProviderService service = (ISourceProviderService) PlatformUI
                .getWorkbench().getService(ISourceProviderService.class);
        return (SaveBestEstimateProvider) service
                .getSourceProvider(MENU_ENABLED[0]);
    }
}
