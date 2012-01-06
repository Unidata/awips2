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
package gov.noaa.nws.ncep.common.dataplugin.ncgrib.util;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Load the ncgrib model xml file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/12/10      4758       bphillip     Initial creation
 * 10/13/10     276        llin			Modified for NC GRIB.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcgribModelLookup {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance of NcgribModelLookup **/
    private static NcgribModelLookup instance;

    /**
     * A map of the models. The key is a hash of the center, grid, and process
     * id
     */
    private Map<Integer, NcgridModel> models;

    /** A map of the models. The key is the title of the model */
    private Map<String, NcgridModel> modelsByName;

    public static synchronized NcgribModelLookup getInstance() {
        if (instance == null) {
            // System.out.println("in reloading ncgribMolels.xml ....");
            instance = new NcgribModelLookup();
        }
        return instance;
    }

    /**
     * If file has been modified, then reload it again
     * 
     * @return
     * @return
     */
    public static void ReloadInstance() {
        instance = null;
    }

    private NcgribModelLookup() {
        models = new HashMap<Integer, NcgridModel>();
        modelsByName = new HashMap<String, NcgridModel>();
        try {
            initModelList();
        } catch (GribException e) {
            logger.error("Unable to initialize model list!", e);
        }
    }

    public NcgridModel getModel(int center, int subcenter, String grid,
            int process) {
        return models.get(toHash(center, subcenter, grid, process));
    }

    public void setModel(int center, int subcenter, String grid, int process,
            String modelName) {
        NcgridModel model = new NcgridModel();
        modelsByName.put(modelName, model);
        models.put(toHash(center, subcenter, grid, process), model);
    }

    public NcgridModel getModel(int center, int subcenter, int gridid,
            int process) {
        return getModel(center, subcenter, String.valueOf(gridid), process);
    }

    public NcgridModel getModelByName(String name) {
        return modelsByName.get(name);
    }

    public Map<String, NcgridModel> getModelByNameMap() {
        return modelsByName;
    }

    private void initModelList() throws GribException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        String path = "";
        String sitePath = "";
        try {
            path = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "ncgribModels.xml")
                    .getCanonicalPath();
            // System.out.println("ncgribModels path=" + path);
            sitePath = pathMgr.getFile(commonStaticSite,
                    "ncgrid" + File.separator + "ncgribModels.xml")
                    .getCanonicalPath();
        } catch (IOException e) {
            logger.error("Error getting ncep grib model definitions", e);
        }
        File modelFile = new File(path);
        File siteModelFile = new File(sitePath);
        NcgridModelSet modelSet = null;
        try {
            if (modelFile.exists()) {
                modelSet = (NcgridModelSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(modelFile.getPath());
            } else {
                ArrayList<NcgridModel> emptyList = new ArrayList<NcgridModel>();
                modelSet = new NcgridModelSet();
                modelSet.setModels(emptyList);
            }

            // Add any models defined by the site
            if (siteModelFile.exists()) {
                List<NcgridModel> siteModels = ((NcgridModelSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(siteModelFile.getPath()))
                        .getModels();
                modelSet.addModels(siteModels);
            }
        } catch (Exception e) {
            throw new GribException("Unable to unmarshal ncep grib models file");
        }

        for (NcgridModel model : modelSet.getModels()) {
            modelsByName.put(model.getName(), model);
            Integer center = model.getCenter();
            Integer subCenter = Integer.parseInt(model.getSubCenter());
            String grid = String.valueOf(model.getGrid());
            for (int process : model.getProcess()) {
                models.put(toHash(center, subCenter, grid, process), model);
            }
        }
        // System.out.println(" reload and init ncgribMolels.xml successfully ....");
    }

    private int toHash(Integer center, Integer subcenter, String grid,
            Integer process) {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((center == null) ? 0 : center.hashCode());
        result = PRIME * result + ((subcenter == null) ? 0 : center.hashCode());
        result = PRIME * result + ((grid == null) ? 0 : grid.hashCode());
        result = PRIME * result + ((process == null) ? 0 : process.hashCode());
        return result;
    }

}
