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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

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
import com.raytheon.uf.viz.derivparam.DerivParamFunctionType;
import com.raytheon.uf.viz.derivparam.IDerivParamFunctionAdapter;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod.MethodType;

/**
 * A thread that accepts requests for a derived parameter, passes it to JEP and
 * returns the request. It is important to note that this thread performs a
 * sleep instead of a schedule to keep the JEP instance in the same thread. To
 * properly interact with this thread, it is important to use
 * <code>DerivedParameterRequest</code> and submit it using the static method
 * <code>addTask</code>.<br/>
 * <br/>
 * <b>IMPORTANT</b> A call to <code>getDerivParamLib</code> should be made
 * immediately after an instance of the datacube container is created or after a
 * call to <code>requestDerivParamUpdate</code>. The reason for this is the main
 * thread will load all derived parameters and then offer them via a synchronous
 * queue. Until the queue is polled, the main derived parameter thread will
 * hang.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 3, 2008              brockwoo    Initial creation
 * Nov 16, 2009 #3120       rjpeter     Removed use of LevelNameMappingFile.
 * Nov 20, 2009 #3387       jelkins     Use derived script's variableId instead of filename
 * Nov 21, 2009 #3576       rjpeter     Refactored DerivParamDesc.
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
    private static DerivParamFunctionType functionType;

    static {
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
                        DerivedParameterGenerator.functionType = functionType;
                        break;
                    } catch (Throwable t) {
                    }
                }
                if (functionType != null) {
                    break;
                }
            }

            if (functionType == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating derived parameter function type,"
                                + " derived paramters will not be available");
            }
        }
    }

    private Set<DerivParamUpdateListener> listeners = new HashSet<DerivParamUpdateListener>();

    private Map<String, DerivParamDesc> derParLibrary;

    private BlockingQueue<DerivedParameterRequest> toDoList;

    private static int numJobs = 2;

    private DerivedParameterJob[] jobs = new DerivedParameterJob[numJobs];

    private boolean needsLibInit = true;

    private boolean runtime = true;

    private String extension = null;

    public static synchronized DerivedParameterGenerator getInstance() {
        if (instance == null) {
            instance = new DerivedParameterGenerator();
        }
        return instance;
    }

    public static DerivParamFunctionType[] getFunctionTypes() {
        DerivedParameterGenerator gen = getInstance();
        if (gen != null && gen.jobs.length > 0) {
            return new DerivParamFunctionType[] { instance.jobs[0].functionType };
        }

        return new DerivParamFunctionType[0];
    }

    public static synchronized Map<String, DerivParamDesc> getDerParLibrary() {
        return getInstance().getLibrary();
    }

    public static void registerUpdateListener(DerivParamUpdateListener listener) {
        getInstance().listeners.add(listener);
    }

    private DerivedParameterGenerator() {
        for (int i = 0; i < jobs.length; i++) {
            jobs[i] = new DerivedParameterJob("DerivParam Engine " + (i + 1));
        }

        // load the extensions
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry != null) {
            IExtensionPoint point = registry.getExtensionPoint(EXTENSION);

            IExtension[] extensions = point.getExtensions();

            for (IExtension ext : extensions) {
                IConfigurationElement[] config = ext.getConfigurationElements();

                for (DerivedParameterJob job : jobs) {
                    for (IConfigurationElement cfg : config) {
                        try {
                            DerivParamFunctionType functionType = new DerivParamFunctionType();
                            functionType.setName(cfg.getAttribute("name"));
                            functionType.setExtension(cfg
                                    .getAttribute("extension"));
                            this.extension = functionType.getExtension();
                            functionType
                                    .setAdapter((IDerivParamFunctionAdapter) cfg
                                            .createExecutableExtension("adapter"));
                            job.functionType = functionType;
                            break;
                        } catch (Throwable t) {
                        }
                    }
                    if (job.functionType == null) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Error creating derived parameter function type,"
                                                + " derived paramters will not be available");
                        break;
                    }
                }
            }
        }

        toDoList = new LinkedBlockingQueue<DerivedParameterRequest>();
        LocalizationFile dir = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(DERIV_PARAM_DIR);
        if (dir != null) {
            dir.addFileUpdatedObserver(this);
        }

        initLibrary();
        for (int i = 0; i < jobs.length; i++) {
            jobs[i].schedule();
        }
    }

    /**
     * Adds a task to the list of derived parameter requests.
     * 
     * @param task
     *            A derived parameter request
     * @return boolean indicating if the request was put into queue
     */
    public static boolean addTask(DerivedParameterRequest task) {
        return getInstance().toDoList.add(task);
    }

    private void initLibrary() {
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
                    DerivParamDesc desc = (DerivParamDesc) jaxbMan
                            .jaxbUnmarshalFromXmlFile(file.getFile());
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
            for (DerivParamUpdateListener listener : listeners) {
                listener.updateDerParLibrary(derParLibrary);
            }

            System.out.println("time to init derived parameter thread: "
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
        for (DerivedParameterJob job : jobs) {
            job.needsInit = true;
        }
    }

    public static void shutdown() {
        if (instance != null) {
            instance.runtime = false;
        }
    }

    private class DerivedParameterJob extends Job {
        // TODO: Handle multiple function types (python mixed with
        // gsl/cuda/anything)
        private DerivParamFunctionType functionType;

        private boolean needsInit = true;

        /**
         * @param name
         */
        public DerivedParameterJob(String name) {
            super(name);
            this.setSystem(true);
        }

        private void init() {
            if (needsInit) {
                functionType.getAdapter().init();
                DerivedParameterGenerator.this.initLibrary();
                needsInit = false;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (runtime) {
                if (derParLibrary == null) {
                    this.init();
                }

                DerivedParameterRequest request = null;
                try {
                    request = toDoList.take();
                } catch (InterruptedException e) {
                }
                if (needsInit) {
                    init();
                }
                if (request != null) {
                    // long t0 = System.currentTimeMillis();
                    try {
                        functionType.getAdapter().executeFunction(
                                request.getMethod(),
                                Arrays.asList(request.getArgumentRecords()));
                        List<IDataRecord> result = functionType.getAdapter()
                                .getRequestResults();
                        for (IDataRecord rec : result) {
                            rec.setName(request.getParameterAbbreviation());
                        }
                        request.setQueue(result);
                    } catch (Throwable e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Derived Parameter Engine failed to generate parameter",
                                        e);
                    }
                    // System.out.println(getName() + " calc took: "
                    // + (System.currentTimeMillis() - t0));

                }
            }
            return Status.OK_STATUS;
        }

    }
}
