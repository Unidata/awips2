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
package com.raytheon.uf.viz.derivparam.library;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;

import javax.measure.unit.Unit;
import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.services.IDisposable;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.derivparam.DerivParamFunctionType;
import com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod.MethodType;

/**
 * Primary public interface for derived parameters. Introspection on the derived
 * parameters available can be done using {@link #getDerParLibrary()}. For
 * actually performing derived parameters calculations the
 * {@link #calculate(DerivedParameterRequest)} method can be used.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#   Engineer    Description
 * ------------- --------  ----------- --------------------------
 * Jul 03, 2008            brockwoo    Initial creation
 * Nov 16, 2009  3120      rjpeter     Removed use of LevelNameMappingFile.
 * Nov 20, 2009  3387      jelkins     Use derived script's variableId instead
 *                                     of filename
 * Nov 21, 2009  3576      rjpeter     Refactored DerivParamDesc.
 * Jun 04, 2013  2041      bsteffen    Switch derived parameters to use
 *                                     concurrent python for threading.
 * Nov 19, 2013  2361      njensen     Only shutdown if initialized
 * Jan 14, 2014  2661      bsteffen    Shutdown using uf.viz.core.Activator
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class DerivedParameterGenerator implements ILocalizationFileObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DerivedParameterGenerator.class);

    public static final String FUNCTIONS = "functions";

    public static final String DEFINITIONS = "definitions";

    public static final String DERIV_PARAM_DIR = "derivedParameters";

    public static final String FUNCTIONS_DIR = DERIV_PARAM_DIR + File.separator
            + FUNCTIONS;

    public static final String XML_DIR = DERIV_PARAM_DIR + File.separator
            + DEFINITIONS;

    private static final String EXTENSION = "com.raytheon.uf.viz.derivparam.functionType";

    public static interface DerivParamUpdateListener {
        public void updateDerParLibrary(
                Map<String, DerivParamDesc> derParLibrary);
    };

    private static DerivedParameterGenerator instance;

    // TODO: Handle multiple function types (python mixed with
    // gsl/cuda/anything)
    private IDerivParamFunctionAdapter adapter;

    private Set<DerivParamUpdateListener> listeners = new HashSet<DerivParamUpdateListener>();

    private Map<String, DerivParamDesc> derParLibrary;

    private boolean needsLibInit = true;

    private String extension = null;

    private Job notifyJob = new Job("Notify Derived Parameter Listeners") {

        @Override
        protected IStatus run(IProgressMonitor arg0) {
            Collection<DerivParamUpdateListener> l = null;
            synchronized (listeners) {
                l = new ArrayList<DerivParamUpdateListener>(listeners);
            }
            for (DerivParamUpdateListener listener : l) {
                listener.updateDerParLibrary(derParLibrary);
            }
            return Status.OK_STATUS;
        }

    };

    public static synchronized DerivedParameterGenerator getInstance() {
        if (instance == null) {
            instance = new DerivedParameterGenerator();
        }
        return instance;
    }

    public static DerivParamFunctionType[] getFunctionTypes() {
        List<DerivParamFunctionType> functionTypes = new ArrayList<DerivParamFunctionType>();
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry != null) {
            IExtensionPoint point = registry.getExtensionPoint(EXTENSION);

            IExtension[] extensions = point.getExtensions();

            for (IExtension ext : extensions) {
                IConfigurationElement[] config = ext.getConfigurationElements();

                for (IConfigurationElement cfg : config) {
                    try {
                        DerivParamFunctionType functionType = new DerivParamFunctionType();
                        functionType.setName(cfg.getAttribute("name"));
                        functionType
                                .setExtension(cfg.getAttribute("extension"));
                        functionType
                                .setAdapter((IDerivParamFunctionAdapter) cfg
                                        .createExecutableExtension("adapter"));
                        functionTypes.add(functionType);
                    } catch (Throwable t) {
                        statusHandler.handle(Priority.DEBUG,
                                t.getLocalizedMessage(), t);
                    }
                }
            }
        }
        return functionTypes.toArray(new DerivParamFunctionType[0]);
    }

    public static synchronized Map<String, DerivParamDesc> getDerParLibrary() {
        return getInstance().getLibrary();
    }

    public static void registerUpdateListener(DerivParamUpdateListener listener) {
        DerivedParameterGenerator instance = getInstance();
        synchronized (instance.listeners) {
            instance.listeners.add(listener);
        }
    }

    private DerivedParameterGenerator() {
        DerivParamFunctionType[] functionTypes = getFunctionTypes();

        if (functionTypes == null || functionTypes.length == 0) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating derived parameter function type,"
                            + " derived paramters will not be available");
        }
        Activator.getDefault().registerDisposable(new IDisposable() {

            @Override
            public void dispose() {
                shutdown();
            }
        });
        this.adapter = functionTypes[0].getAdapter();
        this.extension = functionTypes[0].getExtension();
        notifyJob.setSystem(true);
        LocalizationFile dir = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(DERIV_PARAM_DIR);
        if (dir != null) {
            dir.addFileUpdatedObserver(this);
        }

        initLibrary();
    }

    /**
     * Adds a task to the list of derived parameter requests.
     * 
     * @param task
     *            A derived parameter request
     * @return boolean indicating if the request was put into queue
     */
    public static List<IDataRecord> calculate(DerivedParameterRequest task)
            throws ExecutionException {
        return getInstance().adapter.executeFunction(task.getMethod(),
                Arrays.asList(task.getArgumentRecords()));
    }

    private synchronized void initLibrary() {
        if (needsLibInit) {
            long start = System.currentTimeMillis();
            Set<String> derivParamFiles = new HashSet<String>();
            Map<String, DerivParamDesc> derParLibrary = new HashMap<String, DerivParamDesc>();
            IPathManager pm = PathManagerFactory.getPathManager();

            // get all localization levels derived params and combine them
            LocalizationContext[] contexts = pm
                    .getLocalSearchHierarchy(LocalizationType.CAVE_STATIC);
            LocalizationFile[] xmlFiles = pm.listFiles(contexts, XML_DIR,
                    new String[] { ".xml" }, false, true);
            JAXBManager jaxbMan;
            try {
                jaxbMan = new JAXBManager(DerivParamDesc.class);
            } catch (JAXBException e1) {
                statusHandler
                        .handle(Priority.CRITICAL,
                                "DerivedParameters failed to load, no derived parameters will be available",
                                e1);
                return;
            }

            for (LocalizationFile file : xmlFiles) {
                try {
                    DerivParamDesc desc = jaxbMan.unmarshalFromXmlFile(
                            DerivParamDesc.class, file.getFile());
                    if (derParLibrary.containsKey(desc.getAbbreviation())) {
                        DerivParamDesc oldDesc = derParLibrary.get(desc
                                .getAbbreviation());
                        oldDesc.merge(desc);
                    } else {
                        derParLibrary.put(desc.getAbbreviation(), desc);
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "An error was encountered while creating the DerivedParameter from "
                                    + file.toString(), e);
                    continue;
                }
            }

            LocalizationFile[] functions = pm.listStaticFiles(FUNCTIONS_DIR,
                    new String[] { "." + extension }, false, true);
            for (LocalizationFile file : functions) {
                derivParamFiles.add("func:" + file.getFile().getName());
            }

            // Set the correct units on every field
            for (DerivParamDesc desc : derParLibrary.values()) {
                if (desc.getMethods() == null) {
                    continue;
                }
                for (DerivParamMethod method : desc.getMethods()) {
                    for (IDerivParamField ifield : method.getFields()) {
                        if (ifield instanceof DerivParamField) {
                            DerivParamField field = (DerivParamField) ifield;
                            DerivParamDesc fDesc = derParLibrary.get(field
                                    .getParam());
                            if (fDesc != null && field.getUnit() == Unit.ONE) {
                                field.setUnit(fDesc.getUnit());
                            }
                        }
                    }
                    if (method.getFrameworkMethod() == null) {
                        if (derivParamFiles.contains("func:"
                                + method.getName().split("[.]")[0] + "."
                                + extension)) {
                            method.setMethodType(MethodType.PYTHON);
                        } else {
                            method.setMethodType(MethodType.OTHER);
                        }
                    }
                }
            }
            this.derParLibrary = derParLibrary;
            adapter.init();

            notifyJob.schedule();

            System.out.println("time to init derived parameters: "
                    + (System.currentTimeMillis() - start) + "ms");
            needsLibInit = false;
        }
    }

    public Map<String, DerivParamDesc> getLibrary() {
        initLibrary();
        return derParLibrary;
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
        needsLibInit = true;
        initLibrary();
    }

    public static synchronized void shutdown() {
        if (instance != null) {
            getInstance().adapter.shutdown();
            instance = null;
        }
    }

}
