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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.ListenerList;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.concurrent.IPythonJobListener;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IAsyncStartupObjectListener;
import com.raytheon.viz.gfe.core.msgs.ISmartToolInventoryChanged;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;

/**
 * Manages the smart tool inventory and the associated metadata for each tool
 * for the current GFE session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SmartToolMetadataManager implements ILocalizationFileObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final PythonJobCoordinator<SmartToolMetadataController> jobCoordinator;

    private final Map<String, SmartToolMetadata> toolMetadata;

    private final Object accessLock;

    private final LocalizationFile smartToolDir;

    private final LocalizationFile utilitiesDir;

    private final ListenerList invChangedListeners;

    public SmartToolMetadataManager(final DataManager dataMgr) {
        SmartToolMetadataScriptFactory scriptFactory = new SmartToolMetadataScriptFactory(
                dataMgr);
        this.jobCoordinator = PythonJobCoordinator.newInstance(scriptFactory);

        this.toolMetadata = new HashMap<>();
        this.accessLock = new Object();

        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);
        this.smartToolDir = GfePyIncludeUtil.getSmartToolsLF(baseCtx);
        this.utilitiesDir = GfePyIncludeUtil.getUtilitiesLF(baseCtx);

        this.invChangedListeners = new ListenerList(ListenerList.IDENTITY);
    }

    public void initialize(final IAsyncStartupObjectListener startupListener) {
        SmartToolMetadataExecutor executor = new SmartToolMetadataExecutor();
        IPythonJobListener<Map<String, SmartToolMetadata>> listener = new IPythonJobListener<Map<String, SmartToolMetadata>>() {

            @Override
            public void jobFinished(Map<String, SmartToolMetadata> result) {
                updateMetadata(result);
                startupListener.objectInitialized();
            }

            @Override
            public void jobFailed(Throwable e) {
                statusHandler.error("Error initializing smart tool inventory.",
                        e);
                startupListener.objectInitialized();
            }
        };
        try {
            jobCoordinator.submitAsyncJob(executor, listener);
        } catch (Exception e1) {
            statusHandler.error("Error initializing smart tool metadata.", e1);
        }

        smartToolDir.addFileUpdatedObserver(this);
        utilitiesDir.addFileUpdatedObserver(this);
    }

    public void dispose() {
        smartToolDir.removeFileUpdatedObserver(this);
        utilitiesDir.removeFileUpdatedObserver(this);
        jobCoordinator.shutdown();
    }

    private void updateMetadata(Map<String, SmartToolMetadata> newMetadata) {
        synchronized (accessLock) {
            toolMetadata.clear();
            toolMetadata.putAll(newMetadata);
        }
    }

    /**
     * Lists all the tools that apply to the specified parm
     * 
     * @param parm
     *            the parm to find applicable tools for
     * @return the names of tools that apply to the parm
     */
    public String[] listTools(Parm parm) {
        String parmName = null;
        String parmTypeName = null;
        if (parm != null) {
            parmName = parm.getParmID().getParmName();
            parmTypeName = parm.getGridInfo().getGridType().name();
        }

        Collection<String> tools = new HashSet<>();
        boolean listAll = ((parmName == null) && (parmTypeName == null));
        synchronized (accessLock) {
            for (SmartToolMetadata toolData : toolMetadata.values()) {
                String toolName = toolData.getName();

                if (listAll) {
                    tools.add(toolName);
                } else if (toolData.isHideTool()) {
                    continue;
                } else if (parmName.equals(toolData.getWeatherElementEdited())) {
                    tools.add(toolName);
                } else if (toolData.getScreenList() != null) {
                    if ((toolData.getScreenList().contains(parmName))
                            || (toolData.getScreenList().contains(parmTypeName))) {
                        tools.add(toolName);
                    }
                } else if (toolData.getWeatherElementEdited().equals(
                        "variableElement")) {
                    tools.add(toolName);
                }
            }
        }

        String[] retVal = tools.toArray(new String[0]);
        Arrays.sort(retVal);
        return retVal;
    }

    /**
     * Gets the WeatherElementEdited variable from the smart tool
     * 
     * @param toolName
     *            the name of the tool
     * @return the WeatherElementEdited
     */
    public String getWeatherElementEdited(String toolName) {
        SmartToolMetadata toolData = null;
        synchronized (accessLock) {
            toolData = toolMetadata.get(toolName);
        }

        if (toolData != null) {
            return toolData.getWeatherElementEdited();
        }
        return null;
    }

    public List<FieldDefinition> getVarDictWidgets(String toolName) {
        SmartToolMetadata toolData = null;
        synchronized (accessLock) {
            toolData = toolMetadata.get(toolName);
        }

        if (toolData != null) {
            return toolData.getVarDictWidgets();
        }
        return Collections.emptyList();
    }

    /**
     * Gets the tool's execute() __doc__ string, explaining what the tool does
     * 
     * @param toolName
     *            the tool to find info on
     * @return the tool's execute() documentation, or null if there isn't any
     */
    public String getInfo(String toolName) {
        SmartToolMetadata toolData = null;
        synchronized (accessLock) {
            toolData = toolMetadata.get(toolName);
        }

        if (toolData != null) {
            return toolData.getDocString();
        }
        return null;
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
     * (com.raytheon.uf.common.localization.FileUpdatedMessage)
     */
    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        SmartToolMetadataExecutor executor = new SmartToolMetadataExecutor(
                message);
        IPythonJobListener<Map<String, SmartToolMetadata>> listener = new IPythonJobListener<Map<String, SmartToolMetadata>>() {

            @Override
            public void jobFinished(Map<String, SmartToolMetadata> result) {
                updateMetadata(result);
                notifyListeners();
            }

            @Override
            public void jobFailed(Throwable e) {
                statusHandler.error("Error updating smart tool metadata.", e);
            }
        };
        try {
            jobCoordinator.submitAsyncJob(executor, listener);
        } catch (Exception e1) {
            statusHandler.error("Error updating smart tool metadata.", e1);
        }
    }
}
