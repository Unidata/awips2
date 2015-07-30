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
package com.raytheon.viz.gfe.procedures;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

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
import com.raytheon.viz.gfe.smartscript.FieldDefinition;

/**
 * Manages the procedure inventory and the associated metadata for each
 * procedure for the current GFE session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ProcedureMetadataManager implements
        ILocalizationFileObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final PythonJobCoordinator<ProcedureMetadataController> jobCoordinator;

    private final AtomicReference<Map<String, ProcedureMetadata>> metadata;

    private final LocalizationFile proceduresDir;

    private final LocalizationFile utilitiesDir;

    public ProcedureMetadataManager(final DataManager dataMgr) {
        ProcedureMetadataScriptFactory scriptFactory = new ProcedureMetadataScriptFactory(
                dataMgr);
        this.jobCoordinator = PythonJobCoordinator.newInstance(scriptFactory);
        Map<String, ProcedureMetadata> emptyMap = Collections.emptyMap();
        this.metadata = new AtomicReference<>(emptyMap);

        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);
        this.proceduresDir = GfePyIncludeUtil.getProceduresLF(baseCtx);
        this.proceduresDir.addFileUpdatedObserver(this);
        this.utilitiesDir = GfePyIncludeUtil.getUtilitiesLF(baseCtx);
        this.utilitiesDir.addFileUpdatedObserver(this);
    }

    public void initialize(final IAsyncStartupObjectListener startupListener) {
        ProcedureMetadataExecutor executor = new ProcedureMetadataExecutor();
        IPythonJobListener<Map<String, ProcedureMetadata>> listener = new IPythonJobListener<Map<String, ProcedureMetadata>>() {

            @Override
            public void jobFinished(Map<String, ProcedureMetadata> result) {
                metadata.set(result);
                startupListener.objectInitialized();
            }

            @Override
            public void jobFailed(Throwable e) {
                statusHandler.error("Error initializing procedure inventory.",
                        e);
                startupListener.objectInitialized();
            }
        };
        try {
            jobCoordinator.submitAsyncJob(executor, listener);
        } catch (Exception e1) {
            statusHandler.error("Error initializing procedure metadata.", e1);
        }
    }

    public void dispose() {
        proceduresDir.removeFileUpdatedObserver(this);
        utilitiesDir.removeFileUpdatedObserver(this);
        jobCoordinator.shutdown();
    }

    /**
     * Lists all the procedures that correspond to the menu name
     * 
     * @param menuName
     *            the name of the menu
     * @return the names of procedures that should appear in the menu.
     */
    public List<String> getMenuItems(String menuName) {
        Map<String, ProcedureMetadata> inventory = metadata.get();
        List<String> proceduresForMenu = new ArrayList<>();
        for (ProcedureMetadata procMetadata : inventory.values()) {
            if (procMetadata.getMenuNames().contains(menuName)) {
                proceduresForMenu.add(procMetadata.getName());
            }
        }
        Collections.sort(proceduresForMenu);
        return proceduresForMenu;
    }

    public List<FieldDefinition> getVarDictWidgets(String procedureName) {
        ProcedureMetadata procMetadata = metadata.get().get(procedureName);
        if (procMetadata != null) {
            return procMetadata.getVarDictWidgets();
        }

        return Collections.emptyList();
    }

    public Collection<String> getMethodArguments(String procedureName) {
        ProcedureMetadata procMetadata = metadata.get().get(procedureName);
        if (procMetadata != null) {
            return procMetadata.getArgNames();
        }

        return Collections.emptyList();
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
        ProcedureMetadataExecutor executor = new ProcedureMetadataExecutor(
                message);
        IPythonJobListener<Map<String, ProcedureMetadata>> listener = new IPythonJobListener<Map<String, ProcedureMetadata>>() {

            @Override
            public void jobFinished(Map<String, ProcedureMetadata> result) {
                metadata.set(result);
            }

            @Override
            public void jobFailed(Throwable e) {
                statusHandler.error("Error updating procedure metadata.", e);
            }
        };
        try {
            jobCoordinator.submitAsyncJob(executor, listener);
        } catch (Exception e1) {
            statusHandler.error("Error updating procedure metadata.", e1);
        }
    }

}
