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
package com.raytheon.viz.gfe.smarttool.script;

import java.util.Map;

import jep.JepException;

import org.eclipse.core.runtime.ListenerList;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.msgs.ISmartToolInventoryChanged;

/**
 * Smart tool interpreter used primarily to determine list of smart tools and
 * attributes on those tools. Use SmartToolJob.enqueue() to run a tool, this
 * class should only be used for retrieving information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            njensen     Initial creation
 * Mar 01, 2012  #346      dgilling    Use identity-based ListenerLists.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolUIController extends SmartToolController {

    private ListenerList invChangedListeners;

    public SmartToolUIController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager);

        invChangedListeners = new ListenerList(ListenerList.IDENTITY);
    }

    /**
     * Gets the tool's execute() __doc__ string, explaining what the tool does
     * 
     * @param toolName
     *            the tool to find info on
     * @return the tool's execute() documentation, or null if there isn't any
     * @throws JepException
     */
    public String getInfo(String toolName) throws JepException {
        Map<String, Object> args = getStarterMap(toolName);
        args.put(PyConstants.METHOD_NAME, "execute");
        return (String) execute("getMethodInfo", INTERFACE, args);
    }

    public void addListener(ISmartToolInventoryChanged listener) {
        invChangedListeners.add(listener);
    }

    public void removeListener(ISmartToolInventoryChanged listener) {
        invChangedListeners.remove(listener);
    }

    private void notifyListeners() {
        for (Object listener : invChangedListeners.getListeners()) {
            ((ISmartToolInventoryChanged) listener).smartToolInventoryChanged();
        }
    }

    @Override
    public void fileUpdated(final FileUpdatedMessage message) {
        super.fileUpdated(message);

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                processFileUpdates();
                notifyListeners();
            }
        });
    }

}
