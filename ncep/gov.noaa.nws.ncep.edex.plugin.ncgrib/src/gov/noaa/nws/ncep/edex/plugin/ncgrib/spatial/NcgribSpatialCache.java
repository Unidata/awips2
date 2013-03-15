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

package gov.noaa.nws.ncep.edex.plugin.ncgrib.spatial;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.subgrid.SubNcgrid;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgridModel;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.dao.INcgridCoverageDao;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribModelLookup;

import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Cache used for holding GridCoverage objects. Since creating geometries and
 * CRS objects are expensive operations, this cache is used to store
 * GridCoverages as the are produced.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * Mar 14, 2013 1794        djohnson    FileUtil.listFiles now returns List.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcgribSpatialCache {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static NcgribSpatialCache instance;

    /**
     * Map containing the GridCoverages<br>
     * The key for this map is the id field of the GridCoverage object stored as
     * the value of the map
     */
    private final Map<Integer, NcgridCoverage> ncspatialMap;

    /**
     * Map containing the GridCoverages<br>
     * The key for this map is the name field of the GridCoverage object stored
     * as the value of the map. This is only used internally for lookup of a
     * coverage by name aka gridId.
     */
    private final Map<String, NcgridCoverage> ncspatialNameMap;

    /**
     * Map containing the subGrid coverage based on a model name.
     */
    private final Map<String, Integer> subNcgridCoverageMap;

    /**
     * Map containing the subGrid definition based on a model name.
     */
    private final Map<String, SubNcgrid> definedSubNcgridMap;

    /**
     * Gets the singleton instance of NcgribSpatialCache
     * 
     * @return The singleton instance of the NcgribSpatialCache
     */
    public static synchronized NcgribSpatialCache getInstance() {
        if (instance == null) {
            instance = new NcgribSpatialCache();
        }
        return instance;
    }

    /**
     * Creates a new NcgribSpatialCache
     */
    private NcgribSpatialCache() {
        //System.out.println("ncep spatial grid coverage initial=");
        ncspatialMap = new HashMap<Integer, NcgridCoverage>();
        ncspatialNameMap = new HashMap<String, NcgridCoverage>();
        definedSubNcgridMap = new HashMap<String, SubNcgrid>();
        subNcgridCoverageMap = new HashMap<String, Integer>();
        initializeDefaultGrids();
        initializeDefaultSubGrids();
    }

    /**
     * Retrieves a grid from the map. If the grid does not exist, null is
     * returned
     * 
     * @param id
     *            The id of the NcgridCoverage to retrieve
     * @return The NcgridCoverage object, null if not present
     * @throws GribException
     * @throws DataAccessLayerException
     */
    public NcgridCoverage getGrid(NcgridCoverage coverage) throws GribException {
    	
    	//System.out.println("ncep grid coverage getid=" + coverage.getId());
    	
        NcgridCoverage retVal = ncspatialMap.get(coverage.getId());
        //System.out.println("N grid coverage retVal=" + retVal);

        if (retVal == null) {
            /*
             * Coverage not found in cache, but the values provided in the GDS
             * may be slightly different than those for the grid in the cache.
             * Check the database to be sure.
             */
            try {
            	String where = "Nc" + coverage.getProjectionType().replaceAll(" ", "") + "Dao";
                //System.out.println("ncep where=" + where);

            	retVal = ((INcgridCoverageDao) EDEXUtil.getESBComponent("Nc" + coverage
                        .getProjectionType().replaceAll(" ", "")
                        + "Dao")).checkGrid(coverage);
                //System.out.println("ncep grid coverage=" + retVal);

            } catch (DataAccessLayerException e) {
                throw new GribException("Error querying for ncgrib coverage!", e);
            }
            if (retVal != null) {
                putGrid(retVal, false);
            } 

        }

        return retVal;
    }

    public NcgridCoverage getGrid(int id) {
        return ncspatialMap.get(id);
    }

    public NcgridCoverage getGrid(String modelName) {
        NcgridCoverage rval = null;
        
        //System.out.println("ncep get grid coverage, model Name=" + modelName);

        if (modelName != null) {
            if (subNcgridCoverageMap.containsKey(modelName)) {
                rval = ncspatialMap.get(subNcgridCoverageMap.get(modelName));
            } else {
                NcgridModel model = NcgribModelLookup.getInstance().getModelByName(
                        modelName);
                if (model != null) {
                    rval = ncspatialNameMap.get(model.getGrid().toString());
                }
            }
        }
        //System.out.println("ncep get grid coverage, return coverage=" + rval);

        return rval;
    }
    
    public NcgridCoverage getGridByName(String name){
        return ncspatialNameMap.get(name);
    }

    /**
     * Puts a grid into the NcgribSpatialCache.
     * 
     * @param grid
     *            The grid to store
     * @param persistToDb
     *            True if this NcgridCoverage object is to be persisted to the
     *            database
     * @throws GribException
     *             If problems occur while initializing the grid
     */
    public void putGrid(NcgridCoverage grid, boolean persistToDb)
            throws GribException {

        /*
         * Prepare the grid to be stored into the cache. Initializes the
         * geometry and crs objects and generates the id field
         */
        grid.initialize();
        if (grid.getName() == null) {
            grid.generateName();
        }
        ncspatialMap.put(grid.getId(), grid);
        ncspatialNameMap.put(grid.getName(), grid);

        // Persist to the database if desired
        if (persistToDb) {
            new CoreDao(DaoConfig.DEFAULT).saveOrUpdate(grid);
        }
    }

    public SubNcgrid getSubGrid(String modelName) {
        return definedSubNcgridMap.get(modelName);
    }

    public NcgridCoverage getSubGridCoverage(String modelName) {
        NcgridCoverage rval = null;

        if (subNcgridCoverageMap.containsKey(modelName)) {
            rval = ncspatialMap.get(subNcgridCoverageMap.get(modelName));
        }

        return rval;
    }

    /**
     * Initializes the predefined set of grids. The grids are stored in xml
     * format in the utility folder so the localization service has access to
     * them.<br>
     * NcgridCoverage are created from the xml via JaxB and placed in the cache
     */
    private void initializeDefaultGrids() {

        /*
         * Retrieve the list of files from the localization service
         */
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.trim().endsWith(".xml");
            }
        };
        IPathManager pm = PathManagerFactory.getPathManager();
        String basePath = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), "/ncgrib/ncgrids").getPath();

        //System.out.println("ncep default basePath=" + basePath);
        
        String sitePath = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.SITE), "/ncgrib/ncgrids").getPath();

        //System.out.println("ncep default sitePah=" + sitePath);

        List<File> files = FileUtil.listFiles(new File(basePath), filter,
                true);

        // Add any spatial information defined by the site
        files.addAll(FileUtil.listFiles(new File(sitePath), filter, true));

        /*
         * Iterate over file list. Unmarshal to NcgridCoverage object Persist to
         * cache and database
         */
        for (File file : files) {
            try {
            	//System.out.println ("ncep default filePath=" + file.getPath());
                NcgridCoverage grid = SerializationUtil
                        .jaxbUnmarshalFromXmlFile(NcgridCoverage.class,
                                file.getPath());
                putGrid(grid, true);
            } catch (Exception e) {
                // Log error but do not throw exception
                logger.error("Unable to read default NCEP grids file: " + file, e);
            }
        }
    }

    /**
     * Initializes the predefined set of sub grids. The grids are stored in xml
     * format in the utility folder so the localization service has access to
     * them.<br>
     * NcgridCoverage are created from the xml via JaxB and placed in the cache
     */
    private void initializeDefaultSubGrids() {

        /*
         * Retrieve the list of files from the localization service
         */
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
            	//System.out.println ("default nc init dir=" + dir);
            	//System.out.println ("default nc init name=" + name);
            	//System.out.println ("default nc init return=" + name.trim().endsWith(".xml"));
                return name.trim().endsWith(".xml");
            }
        };
        IPathManager pm = PathManagerFactory.getPathManager();

        // load base sub grid definitions
        File path = pm.getFile(pm.getContext(LocalizationType.EDEX_STATIC,
                LocalizationLevel.BASE), "/ncgrib/subncgrids");
        
        //System.out.println("default init ncgrib Path1=" + path);

        if (path.exists() && path.isDirectory()) {
            loadSubGridFiles(FileUtil.listFiles(path, filter, true));
        }

        // load site sub grid definitions
        path = pm.getFile(pm.getContext(LocalizationType.EDEX_STATIC,
                LocalizationLevel.SITE), "/ncgrib/subncgrids");
        
        //System.out.println("default init ncgrib Path2=" + path);

        if (path.exists() && path.isDirectory()) {
            loadSubGridFiles(FileUtil.listFiles(path, filter, true));
        }
    }

    private void loadSubGridFiles(List<File> files) {
        NcgribModelLookup gribModelLUT = NcgribModelLookup.getInstance();

        for (File file : files) {
        	//System.out.println (" load nc subgrid file=" + file );
            try {
                SubNcgrid subGrid = (SubNcgrid) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(file.getPath());

                // do a reverse lookup of the model name to get its associated
                // grid id
                String modelName = subGrid.getModelName();
                //System.out.println (" load nc subgrid file model=" + modelName );
                
                NcgridModel model = gribModelLUT.getModelByName(modelName);
                if (model != null) {
                    NcgridCoverage baseCoverage = ncspatialNameMap.get(model
                            .getGrid().toString());

                    //System.out.println (" load nc subgrid baseCoverage=" + baseCoverage );
                    
                    if (baseCoverage != null) {
                        NcgridCoverage subGridCoverage = baseCoverage
                                .trim(subGrid);
                        
                        //System.out.println (" load nc subgrid subGridCoverage=" + subGridCoverage );

                        if (subGridCoverage != null) {
                            NcgridCoverage storedCoverage = getGrid(subGridCoverage);
                            if (storedCoverage == null) {
                                putGrid(subGridCoverage, true);
                                storedCoverage = subGridCoverage;
                            }
                            //System.out.println (" load nc subgrid after store coverage" );

                            definedSubNcgridMap.put(modelName, subGrid);
                            subNcgridCoverageMap.put(modelName, storedCoverage
                                    .getId());
                        }
                    }
                }
            } catch (Exception e) {
                // Log error but do not throw exception
                logger
                        .error("Unable to read default sub ncgrid file: " + file,
                                e);
            }
        }
    }
}
