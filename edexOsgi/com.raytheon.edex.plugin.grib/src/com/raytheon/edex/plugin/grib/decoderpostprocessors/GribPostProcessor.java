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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

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
    private static Map<String, List<IDecoderPostProcessor>> processorMap = new HashMap<String, List<IDecoderPostProcessor>>();

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
        IPathManager pm = PathManagerFactory.getPathManager();
        String processorFile = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE),
                "/grib/postProcessModels/postProcessedModels.txt").getPath();
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(processorFile));
            String line = null;
            String[] tokens = null;
            while ((line = in.readLine()) != null) {
                if (line.startsWith("#") || line.trim().isEmpty()) {
                    continue;
                }
                tokens = line.split(":");
                String model = tokens[0].trim();
                IDecoderPostProcessor postProcessor = getPostProcessor(
                        tokens[0].trim(), tokens[1].trim());
                if (processorMap.containsKey(model)) {
                    processorMap.get(model).add(postProcessor);
                } else {
                    ArrayList<IDecoderPostProcessor> processors = new ArrayList<IDecoderPostProcessor>();
                    processors.add(postProcessor);
                    processorMap.put(model, processors);
                }
            }
            in.close();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading post processed model file", e);
        } catch (GribException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error instantiating decoder post processor", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error closing post processed model file", e);
                }
            }
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
    public GribRecord[] process(GribRecord[] records) throws GribException {
        List<IDecoderPostProcessor> processors;
        GribRecord[] results = null;
        List<GribRecord> additionalGrids = null;
        for (int i = 0; i < records.length; i++) {
            // Check the map to see if this grib record is part of a model for
            // which post processing is necessary
            processors = processorMap.get(records[i].getModelInfo()
                    .getModelName());
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
                                additionalGrids = new ArrayList<GribRecord>();
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
            return additionalGrids.toArray(new GribRecord[] {});
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
