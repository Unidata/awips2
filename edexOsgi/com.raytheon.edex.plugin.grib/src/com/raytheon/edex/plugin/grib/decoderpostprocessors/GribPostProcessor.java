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
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
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
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */
public class GribPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribPostProcessor.class);

    /** The singleton instance */
    private static GribPostProcessor instance;

    /** The map containing the currently registered grib post processors */
    private Map<String, List<IDecoderPostProcessor>> processorMap;

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

        List<IDecoderPostProcessor> processors;
        GridRecord[] results = null;
        List<GridRecord> additionalGrids = null;
        for (int i = 0; i < records.length; i++) {
            // Check the map to see if this grib record is part of a model for
            // which post processing is necessary
            processors = processorMap.get(records[i].getDatasetId());
            if (processors != null) {
                for (IDecoderPostProcessor processor : processors) {
                    // Post processing is not necessary, so we continue
                    if (processor == null) {
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
            return additionalGrids.toArray(new GridRecord[] {});
        }
    }

    /**
     * Registers the IDecoderPostProcessor classes for the supplied
     * fully-qualified classnames.
     *
     * @param fqClassNames
     *            The list of fully-qualified classnames to register.
     */
    public synchronized void register(String... fqClassNames) {
        String retClass;
        IDecoderPostProcessor newProc;
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

            if (!(newObj instanceof IDecoderPostProcessor)) {
                statusHandler.warn(className
                        + " is not an IDecoderPostProcessor");
                continue;
            }

            newProc = (IDecoderPostProcessor) newObj;
            statusHandler.debug("Registering grib post processor for "
                    + className);
            retClass = knownProcessors.put(newProc.getClass()
                    .getSimpleName(), className);

            /* Warn if two registered classes share the same simple class name. */
            if (retClass != null && !retClass.equals(className)) {
                statusHandler.warn(retClass
                        + " has been replaced by " + className);
            }
        }
    }

    /**
     * Initializes the processor map. As long as the localization file can be
     * unmarshalled, the map will be swapped with the newly unmarshalled map.
     *
     * It's assumed that every processor will have already been registered under
     * its simple name, or is fully qualified.
     */
    private synchronized void initProcessorMap() {
        LocalizationFile processorFile = PathManagerFactory
                .getPathManager()
                .getStaticLocalizationFile(
                        "/grib/postProcessModels/postProcessedModels.xml");

        Set<String> modelNames = GribModelLookup.getInstance().getModelNames();

        try (InputStream is = processorFile.openInputStream()) {

            JAXBManager mgr = new JAXBManager(PostProcessedModelSet.class);
            PostProcessedModelSet ppModelSet = (PostProcessedModelSet) mgr
                    .unmarshalFromInputStream(is);

            statusHandler.info(String.format("Using postProcessorFile [%s]",
                    processorFile));

            Map<String, List<IDecoderPostProcessor>> newMap = new HashMap<>();

            /*
             * Iterate over post processed models. Determine which models apply
             * to each post processor if a regex is present
             */
            String knownProc;
            String classToLoad;
            for (PostProcessedModel ppModel : ppModelSet.getModels()) {
                for (String modelName : modelNames) {
                    if (modelName.matches(ppModel.getModelName())) {
                        List<IDecoderPostProcessor> processorInstances = newMap
                                .get(modelName);
                        if (processorInstances == null) {
                            processorInstances = new ArrayList<IDecoderPostProcessor>();
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
                                processorInstances
                                        .add((IDecoderPostProcessor) Class
                                                .forName(classToLoad)
                                                .newInstance());
                            } catch (Exception e) {
                                statusHandler.fatal(
                                        "Error instantiating grib post processor for "
                                                + processor, e);
                            }
                        }
                    }
                }
            }

            this.processorMap = newMap;
        } catch (LocalizationException | JAXBException | IOException
                | SerializationException e) {
            statusHandler.fatal(
                    "Error unmarshalling post processed model list: "
                            + processorFile, e);

            if (this.processorMap == null) {
                this.processorMap = new HashMap<>();
            }
        }
    }
}
