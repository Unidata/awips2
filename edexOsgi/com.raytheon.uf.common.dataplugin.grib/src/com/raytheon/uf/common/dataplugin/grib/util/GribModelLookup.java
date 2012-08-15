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
package com.raytheon.uf.common.dataplugin.grib.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;

public class GribModelLookup {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance of GribModelLookup **/
    private static GribModelLookup instance;

    /**
     * A map of the models. The key is a hash of the center, grid, and process
     * id rcg: changed key to a string, too many identical key were generated
     * with the hash.
     */
    private final Map<String, GridModel> models;

    /** A map of the models. The key is the title of the model */
    private final Map<String, GridModel> modelsByName;

    public static synchronized GribModelLookup getInstance() {
        if (instance == null) {
            instance = new GribModelLookup();
        }
        return instance;
    }

    private GribModelLookup() {
        models = new HashMap<String, GridModel>();
        modelsByName = new HashMap<String, GridModel>();
        try {
            initModelList();
        } catch (GribException e) {
            logger.error("Unable to initialize model list!", e);
        }
    }

    public GridModel getModel(int center, int subcenter, String grid,
            int process) {
        return models.get(toKey(center, subcenter, grid, process));
    }

    public GridModel getModel(int center, int subcenter, int gridid, int process) {
        return getModel(center, subcenter, String.valueOf(gridid), process);
    }

    public GridModel getModelByName(String name) {
        return modelsByName.get(name);
    }

    public Map<String, GridModel> getModelByNameMap() {
        return modelsByName;
    }

    private void initModelList() throws GribException {

        LocalizationContext commonStaticBase = PathManagerFactory
                .getPathManager().getContext(
                        LocalizationContext.LocalizationType.COMMON_STATIC,
                        LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext commonStaticSite = PathManagerFactory
                .getPathManager().getContext(
                        LocalizationContext.LocalizationType.COMMON_STATIC,
                        LocalizationContext.LocalizationLevel.SITE);

        LocalizationFile[] modelFiles = PathManagerFactory.getPathManager()
                .listFiles(
                        new LocalizationContext[] { commonStaticSite,
                                commonStaticBase },
                        "grid" + IPathManager.SEPARATOR + "models", // Win32
                        new String[] { ".xml" }, false, true);
        GridModelSet modelSet = new GridModelSet();

        for (LocalizationFile modelFile : modelFiles) {
            try {
                modelSet.addModels(((GridModelSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(modelFile.getFile().getPath()))
                        .getModels());
            } catch (Exception e) {
                throw new GribException("Unable to unmarshal grib models file:"
                        + modelFile);
            }
        }

        for (GridModel model : modelSet.getModels()) {
            modelsByName.put(model.getName(), model);
            for (int process : model.getProcess()) {
                models.put(
                        toKey(model.getCenter(),
                                Integer.parseInt(model.getSubCenter()),
                                String.valueOf(model.getGrid()), process),
                        model);
            }
        }
    }
    
    public Set<String> getModelNames(){
    	return modelsByName.keySet();
    }

    private String toKey(Integer center, Integer subcenter, String grid,
            Integer process) {
        StringBuilder builder = new StringBuilder();
        builder.append(center);
        builder.append(subcenter);
        builder.append(grid);
        builder.append(process);
        return builder.toString();
        // final int PRIME = 31;
        // int result = 1;
        // result = PRIME * result + ((center == null) ? 0 : center.hashCode());
        // result = PRIME * result
        // + ((subcenter == null) ? 0 : subcenter.hashCode());
        // result = PRIME * result + ((grid == null) ? 0 : grid.hashCode());
        // result = PRIME * result + ((process == null) ? 0 :
        // process.hashCode());
        // return result;
    }
}
