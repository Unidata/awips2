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
package gov.noaa.nws.ncep.edex.util.ncgrib;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgridModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgridModelSet;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

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
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 3/12/10      4758       	bphillip    Initial creation
 * 10/13/10     276        	llin     	Modified for NC GRIB.
 * 11/02/11                	xguo		Updated gridid
 * 3/2012					T. Lee		Get ensemble modelName via template 
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
            logger.error("Unable to initialize ncep model list!", e);
        }
    }

    public NcgridModel getModel(int center, int subcenter, String grid,
            int process, String filename, NcgribModel nbm) {
    	NcgridModel model = new NcgridModel ();
    	model = models.get(toHash(center, subcenter, grid, process));
    	if ( model == null ) {
    		return model;
    	} 
    	String template = model.getTemplate();

    	//System.out.println ( " Center: "+ center + " subCenter: "+ subcenter +  " gridID: " + grid
    	//		+ " modelID: "+ process +  " modelName: " + model.getName() );

    	if ( !template.equals("NONE")) {
    		/* 
		 	 * Using Template to get ModelName 
			 */
    		String[] tokens = template.replaceAll("\\s","").split(";");

    		// CMC ensemble			
			if ( Pattern.matches("cmc.*", filename)){
				//System.out.println ( " CMC ensemble " + "!!!\n");
				for ( String token : tokens ) {
					String[] alias = token.split("\\|");
					if ( Pattern.matches(alias[0], filename) ) {
						model.setName(alias[1]);
						break;
					}
				}

			// SREF ensemble
			} else if ( Pattern.matches("sref.*", filename)) {
				for ( String token : tokens ) {
					String[] alias = token.split("\\|"); 
					if ( Pattern.matches(alias[0], filename)) {
						model.setName(alias[1]);
						
						// perturbation number
						String[] pert = filename.split("\\.");
						if ( pert[3].startsWith("p") || pert[3].startsWith("n")) {
							nbm.setPerturbationNumber(pert[3]);
						}
						break;
					}
				}
				
			// NAEFS ensemble
			} else if ( Pattern.matches("naefs.*", filename)) {
				for ( String token : tokens ) {
					String[] alias = token.split("\\|");
					if ( Pattern.matches(alias[0], filename)) {
						model.setName(alias[1]);
						break;
					}
				}
				
			// GEFS ensemble
			} else if ( Pattern.matches("ge.*", filename)) {
				for ( String token : tokens ) {
					String[] alias = token.split("\\|");
					if ( Pattern.matches(alias[0], filename)) {
						model.setName(alias[1]);
						break;
					}
				}
				
			// GWW derived data
			} else if ( Pattern.matches("mean.*", filename) ||
					    Pattern.matches("probab.*", filename) ||
					    Pattern.matches("spread.*", filename)) {
				for ( String token : tokens ) {
					String[] alias = token.split("\\|");
					if ( Pattern.matches(alias[0], filename)) {
						model.setName(alias[1]);
						break;
					}
				}
			}

    	}
    	return model;
    }

    public synchronized void setModel(int center, int subcenter, String grid,
            int process, String modelName) {
        NcgridModel model = new NcgridModel();
        model.setCenter(center);
        model.setName(modelName);
        model.setSubCenter(Integer.toString(subcenter));
        model.setGrid(grid);
        ArrayList<Integer> processList = new ArrayList<Integer>();
        processList.add(process);
        model.setProcess(processList);
        // System.out.println (" NcgribModel lookup add a new , model=" +
        // modelName);
        modelsByName.put(modelName, model);
        models.put(toHash(center, subcenter, grid, process), model);
    }

    public NcgridModel getModel(int center, int subcenter, int gridid,
            int process, String filename, NcgribModel model) {
        return getModel(center, subcenter, String.valueOf(gridid), process, filename, model);
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
        System.out
                .println("ncgribMolels.xml has been loaded successfully ....");
    }

    private int toHash(Integer center, Integer subcenter, String grid,
            Integer process) {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((center == null) ? 0 : center.hashCode());
        result = PRIME * result
                + ((subcenter == null) ? 0 : subcenter.hashCode());
        result = PRIME * result + ((grid == null) ? 0 : grid.hashCode());
        result = PRIME * result + ((process == null) ? 0 : process.hashCode());
        return result;
    }
}
