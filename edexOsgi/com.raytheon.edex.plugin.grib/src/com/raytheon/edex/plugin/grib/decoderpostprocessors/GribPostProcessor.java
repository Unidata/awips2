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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
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
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/30/10      5875        bphillip    Initial Creation
 * 9/20/2012    1206        jkorman     Added logging of postProcessedModels load.
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
    private static Map<String, List<IDecoderPostProcessor>> processorMap = new HashMap<String, List<IDecoderPostProcessor>>();

    private static final String CLASS_PREFIX = "com.raytheon.edex.plugin.grib.decoderpostprocessors.";

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
        String processorFile = PathManagerFactory
                .getPathManager()
                .getStaticFile(
                        "/grib/postProcessModels/postProcessedModels.xml")
                .getPath();

        try {
            // Get the list of available model names
            Set<String> modelNames = GribModelLookup.getInstance()
                    .getModelNames();

            // Unmarshal the post processed model file
            PostProcessedModelSet ppModelSet = (PostProcessedModelSet) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(processorFile);

			statusHandler.info(String.format("Using postProcessorFile [%s]", processorFile));
			
            /*
             * Iterate over post processed models. Determine which models apply
             * to each post processor if a regex is present
             */
            for (PostProcessedModel ppModel : ppModelSet.getModels()) {
                for (String modelName : modelNames) {
                    if (modelName.matches(ppModel.getModelName())) {
                        List<IDecoderPostProcessor> processorInstances = processorMap
                                .get(modelName);
                        if (processorInstances == null) {
                            processorMap.put(modelName,
                                    new ArrayList<IDecoderPostProcessor>());
                            processorInstances = processorMap.get(modelName);
                        }

                        for (String processor : ppModel.getProcessors()) {
                            try {
                                processorInstances
                                        .add((IDecoderPostProcessor) Class
                                                .forName(
                                                        CLASS_PREFIX
                                                                + processor)
                                                .newInstance());
                            } catch (InstantiationException e) {
                                statusHandler
                                        .fatal("Error instantiating grib post processor!",
                                                e);
                            } catch (IllegalAccessException e) {
                                statusHandler
                                        .fatal("Error instantiating grib post processor!",
                                                e);
                            } catch (ClassNotFoundException e) {
                                statusHandler
                                        .info("Class ["
                                                + CLASS_PREFIX
                                                + processor
                                                + "] not found.  Trying to load class: ["
                                                + processor + "]");
                                try {
                                    processorInstances
                                            .add((IDecoderPostProcessor) Class
                                                    .forName(processor)
                                                    .newInstance());
                                } catch (Exception e1) {
                                    statusHandler
                                            .fatal("Error instantiating grib post processor!",
                                                    e1);
                                }
                            }

                        }

                    }
                }
            }
        } catch (SerializationException e) {
            statusHandler.fatal(
                    "Error unmarshalling post processed model list: "
                            + processorFile, e);
        }

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
                    else {
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

    private IDecoderPostProcessor getPostProcessor(String modelName,
            String processorClassName) throws GribException {

        for (List<IDecoderPostProcessor> processors : processorMap.values()) {
            for (IDecoderPostProcessor processor : processors) {
                if (processor.getClass().getCanonicalName()
                        .equals(processorClassName)) {
                    return processor;
                }
            }
        }
        try {
            return (IDecoderPostProcessor) Class.forName(processorClassName)
                    .newInstance();
        } catch (Exception e) {
            throw new GribException(
                    "Error instantiating decoder post processor for "
                            + modelName + " model.", e);
        }
    }
}
