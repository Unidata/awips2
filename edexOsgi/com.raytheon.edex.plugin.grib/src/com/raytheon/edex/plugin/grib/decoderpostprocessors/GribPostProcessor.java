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

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import org.apache.camel.Headers;

import com.raytheon.edex.plugin.grib.decoderpostprocessors.DecoderPostProcessor.PostProcessorType;
import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * An implementation to modify a grib record after the initial grid decoding if
 * necessary
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 30, 2010  5875     bphillip    Initial Creation
 * Sep 20, 2012  1206     jkorman     Added logging of postProcessedModels
 *                                    load.
 * Oct 15, 2013  2473     bsteffen    Rewrite deprecated and unused code.
 * Sep 24, 2015  3731     nabowle     Allow pre-registering shortnames and
 *                                    require fully qualified names otherwise.
 * Oct 07, 2015  3756     nabowle     Add separate post-processing after the
 *                                    decoded record is persisted.
 * Oct 14, 2015  4627     nabowle     Load post processor mappings at each
 *                                    localization level as available, appending
 *                                    only new processors.
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */
public class GribPostProcessor {
    private static final GridRecord[] EMPTY_ARR = new GridRecord[] {};

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribPostProcessor.class);

    /** The singleton instance */
    private static GribPostProcessor instance;

    /** The map containing the currently registered grib post processors */
    private Map<String, List<DecoderPostProcessor>> processorMap;

    private Map<String, String> knownProcessors = new HashMap<>();

    /**
     * Gets the singleton instance of GribPostProcessor
     *
     * @return The singleton instance of GribPostProcessor
     */
    public static synchronized GribPostProcessor getInstance() {
        if (instance == null) {
            instance = new GribPostProcessor();
        }
        return instance;
    }

    /**
     * Creates a new GribPostProcessor instance
     */
    private GribPostProcessor() {
        super();
    }

    /**
     * Processes the GribRecords to determine if they need post processing
     *
     * @param records
     *            The records to examine
     * @return The GribRecords including any new records created during the post
     *         processing
     * @throws GribException
     */
    public GridRecord[] process(GridRecord[] records) throws GribException {
        synchronized (this) {
            if (this.processorMap == null) {
                initProcessorMap();
            }
        }

        List<DecoderPostProcessor> processors;
        GridRecord[] results = null;
        List<GridRecord> additionalGrids = null;
        for (int i = 0; i < records.length; i++) {
            // Check the map to see if this grib record is part of a model for
            // which post processing is necessary
            processors = processorMap.get(records[i].getDatasetId());
            if (processors != null) {
                for (DecoderPostProcessor processor : processors) {
                    // Post processing is not necessary, so we continue
                    if (processor == null
                            || PostProcessorType.POST_PERSIST.equals(processor
                                    .getType())) {
                        continue;
                    }

                    // Post processing is necessary
                    results = processor.process(records[i]);
                    if (results.length == 0) {
                        return results;
                    }
                    records[i] = results[0];
                    if (results.length > 1) {
                        if (additionalGrids == null) {
                            additionalGrids = new ArrayList<GridRecord>();
                        }
                        for (int j = 1; j < results.length; j++) {
                            additionalGrids.add(results[j]);
                        }
                    }
                }
            }
        }
        if (additionalGrids == null) {
            return records;
        } else {
            for (int i = 0; i < records.length; i++) {
                additionalGrids.add(records[i]);
            }
            return additionalGrids.toArray(EMPTY_ARR);
        }
    }

    /**
     * Processes the GridRecords to determine if they need post processing
     *
     * @param notif
     *            A notification of datauri's that have been persisted.
     * @return Only grid records created by the post processors. The records
     *         matching the uri's will not be returned.
     * @throws GribException
     */
    public GridRecord[] processPersisted(DataURINotificationMessage notif,
            @Headers
            Map<String, Object> headers) throws GribException {
        headers.put("dequeueTime", System.currentTimeMillis());
        String[] dataURIs = notif.getDataURIs();
        if (dataURIs == null || dataURIs.length == 0) {
            return EMPTY_ARR;
        }

        synchronized (this) {
            if (this.processorMap == null) {
                initProcessorMap();
            }
        }

        List<DecoderPostProcessor> processors;
        GridRecord[] recordResults;
        Set<GridRecord> newGrids = new HashSet<>();
        GridRecord record;

        for (String uri : dataURIs) {
            try {
                record = (GridRecord) DataURIUtil.createPluginDataObject(uri);
            } catch (PluginException e) {
                throw new GribException(
                        "Could not create plugin data object for " + uri, e);
            }

            processors = processorMap.get(record.getDatasetId());
            if (processors != null) {
                for (DecoderPostProcessor processor : processors) {
                    if (processor == null
                            || PostProcessorType.PRE_PERSIST.equals(processor
                                    .getType())) {
                        continue;
                    }

                    recordResults = processor.process(record);

                    if (recordResults != null) {
                        for (GridRecord rec : recordResults) {
                            if (!uri.equals(rec.getDataURI())) {
                                newGrids.add(rec);
                            } else {
                                statusHandler
                                        .warn(uri
                                                + " will not be re-persisted to prevent an infinite post-processing loop. "
                                                + processor.getClass()
                                                        .getName()
                                                + " should be of type "
                                                + PostProcessorType.PRE_PERSIST
                                                        .name()
                                                + " or should not include the post-processed record in the results.");
                            }
                        }
                    }
                }
            }
        }
        return newGrids.toArray(EMPTY_ARR);
    }

    /**
     * Registers the DecoderPostProcessor classes for the supplied
     * fully-qualified classnames.
     *
     * @param fqClassNames
     *            The list of fully-qualified classnames to register.
     */
    public synchronized void register(String... fqClassNames) {
        String retClass;
        DecoderPostProcessor newProc;
        Object newObj;
        for (String className : fqClassNames) {
            if (className == null || className.trim().isEmpty()) {
                continue;
            }

            try {
                newObj = Class.forName(className).newInstance();
            } catch (InstantiationException | IllegalAccessException
                    | ClassNotFoundException e) {
                statusHandler.fatal(
                        "Cannot find a grib post processor for class "
                                + className, e);
                continue;
            }

            if (!(newObj instanceof DecoderPostProcessor)) {
                statusHandler.warn(className
                        + " is not an DecoderPostProcessor");
                continue;
            }

            newProc = (DecoderPostProcessor) newObj;
            statusHandler.debug("Registering grib post processor for "
                    + className);
            retClass = knownProcessors.put(newProc.getClass().getSimpleName(),
                    className);

            /* Warn if two registered classes share the same simple class name. */
            if (retClass != null && !retClass.equals(className)) {
                statusHandler.warn(retClass + " has been replaced by "
                        + className);
            }
        }
    }

    /**
     * Initializes the processor map. Starting at base working to site, the
     * localization files will be unmarshalled if present and new processors
     * will be appended to the list of processors for a model. If a processor
     * has already been configured for a model, it will not be added again.
     *
     * It's assumed that every processor will have already been registered under
     * its simple name, or is fully qualified.
     *
     * Other than the first initialization, the processor map will only be
     * changed if the new value is not an empty map.
     */
    private synchronized void initProcessorMap() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationLevel[] levels = new LocalizationLevel[] {
                LocalizationLevel.BASE, LocalizationLevel.REGION,
                LocalizationLevel.CONFIGURED, LocalizationLevel.SITE };

        LocalizationFile processorFile;
        Map<LocalizationLevel, LocalizationFile> files = pathMgr
                .getTieredLocalizationFile(LocalizationType.EDEX_STATIC,
                        "/grib/postProcessModels/postProcessedModels.xml");
        PostProcessedModelSet ppModelSet;
        List<PostProcessedModel> postProcessedModels = new ArrayList<>();
        Map<String, Integer> idMap = new HashMap<>();
        for (LocalizationLevel level : levels) {
            processorFile = files.get(level);
            if (processorFile == null) {
                continue;
            }

            try (InputStream is = processorFile.openInputStream()) {
                JAXBManager mgr = new JAXBManager(PostProcessedModelSet.class);
                ppModelSet = (PostProcessedModelSet) mgr
                        .unmarshalFromInputStream(is);
                statusHandler.info(String.format(
                        "Using postProcessorFile [%s]", processorFile));

                for (PostProcessedModel ppModel : ppModelSet.getModels()) {
                    if (ppModel.getId() == null
                            || ppModel.getId().trim().isEmpty()) {
                        // no id - just append in the order found
                        postProcessedModels.add(ppModel);
                    } else {
                        /*
                         * If the id is previously known, put this ppModel in
                         * its place in the list, otherwise just add to the end
                         * and track its index.
                         */
                        Integer idx = idMap.get(ppModel.getId());
                        if (idx == null) {
                            postProcessedModels.add(ppModel);
                            idMap.put(ppModel.getId(), postProcessedModels.size() - 1);
                        } else {
                            postProcessedModels.remove(idx.intValue());
                            postProcessedModels.add(idx.intValue(), ppModel);
                        }
                    }
                }
            } catch (LocalizationException | JAXBException | IOException
                    | SerializationException e) {
                statusHandler.fatal(
                        "Error unmarshalling post processed model list: "
                                + processorFile, e);
            }
        }

        /*
         * Iterate over post processed models. Determine which models apply to
         * each post processor if a regex is present
         */
        String knownProc;
        String classToLoad;
        List<DecoderPostProcessor> processorInstances;
        Set<String> modelNames = GribModelLookup.getInstance().getModelNames();
        Map<String, List<DecoderPostProcessor>> newMap = new HashMap<>();
        for (PostProcessedModel ppModel : postProcessedModels) {
            if (ppModel.getModelName() == null) {
                continue;
            }
            for (String modelName : modelNames) {
                if (modelName.matches(ppModel.getModelName())) {
                    processorInstances = newMap.get(modelName);
                    if (processorInstances == null) {
                        processorInstances = new ArrayList<DecoderPostProcessor>();
                        newMap.put(modelName, processorInstances);
                    }

                    for (String processor : ppModel.getProcessors()) {
                        knownProc = this.knownProcessors.get(processor);
                        if (knownProc != null) {
                            classToLoad = knownProc;
                        } else {
                            classToLoad = processor;
                        }

                        try {
                            boolean alreadyConfigured = false;
                            for (DecoderPostProcessor instance : processorInstances) {
                                if (classToLoad.equals(instance.getClass()
                                        .getName())) {
                                    alreadyConfigured = true;
                                    statusHandler.debug(classToLoad
                                            + " is already configured for "
                                            + modelName + ".");
                                    break;
                                }
                            }
                            if (!alreadyConfigured) {
                                processorInstances
                                        .add((DecoderPostProcessor) Class
                                                .forName(classToLoad)
                                                .newInstance());
                            }
                        } catch (Exception e) {
                            statusHandler.fatal(
                                    "Error instantiating grib post processor for "
                                            + processor, e);
                        }
                    }
                }
            }
        }

        if (this.processorMap == null || !newMap.isEmpty()) {
            this.processorMap = newMap;
        }
    }
}
