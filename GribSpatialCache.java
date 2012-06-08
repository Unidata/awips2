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

package com.raytheon.edex.plugin.grib.spatial;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.edex.plugin.grib.dao.GribModelDao;
import com.raytheon.edex.plugin.grib.dao.GridCoverageDao;
import com.raytheon.edex.plugin.grib.dao.IGridCoverageDao;
import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.awipstools.GetWfoCenterPoint;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.subgrid.SubGrid;
import com.raytheon.uf.common.dataplugin.grib.subgrid.SubGridDef;
import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.awipstools.GetWfoCenterHandler;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Cache used for holding GridCoverage objects. Since creating geometries and
 * CRS objects are expensive operations, this cache is used to store
 * GridCoverages as they are produced.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GribSpatialCache {

	/** The logger */
	protected transient Log logger = LogFactory.getLog(getClass());

	/** The singleton instance */
	private static GribSpatialCache instance = new GribSpatialCache();

	/**
	 * Map containing the GridCoverages<br>
	 * The key for this map is the id field of the GridCoverage object stored as
	 * the value of the map
	 */
	private Map<Integer, GridCoverage> spatialMap;

	/**
	 * Map containing the GridCoverages<br>
	 * The key for this map is the name field of the GridCoverage object stored
	 * as the value of the map. This is only used internally for lookup of a
	 * coverage by name aka gridId.
	 */
	private Map<String, GridCoverage> spatialNameMap;

	/**
	 * Map containing the subGrid coverage based on a model name.
	 */
	private Map<String, Integer> subGridCoverageMap;

	/**
	 * Map containing the subGrid definition based on a model name.
	 */
	private Map<String, SubGrid> definedSubGridMap;

	/**
	 * Gets the singleton instance of GribSpatialCache
	 * 
	 * @return The singleton instance of the GribSpatialCache
	 */
	public static GribSpatialCache getInstance() {
		return instance;
	}

	/**
	 * Creates a new GribSpatialCache
	 */
	private GribSpatialCache() {
		spatialMap = new HashMap<Integer, GridCoverage>();
		spatialNameMap = new HashMap<String, GridCoverage>();
		definedSubGridMap = new HashMap<String, SubGrid>();
		subGridCoverageMap = new HashMap<String, Integer>();
		initializeGrids();
	}

	/**
	 * Retrieves a grid from the map. If the grid does not exist, null is
	 * returned
	 * 
	 * @param id
	 *            The id of the GridCoverage to retrieve
	 * @return The GridCoverage object, null if not present
	 * @throws GribException
	 * @throws DataAccessLayerException
	 */
	public GridCoverage getGrid(GridCoverage coverage) throws GribException {
		GridCoverage retVal = spatialMap.get(coverage.getId());

		if (retVal == null) {
			/*
			 * Coverage not found in cache, but the values provided in the GDS
			 * may be slightly different than those for the grid in the cache.
			 * Check the database to be sure.
			 */
			try {
				retVal = ((IGridCoverageDao) EDEXUtil.getESBComponent(coverage
						.getProjectionType().replaceAll(" ", "") + "Dao"))
						.checkGrid(coverage);
			} catch (DataAccessLayerException e) {
				throw new GribException("Error querying for grib coverage!", e);
			}

			if (retVal != null) {
				spatialMap.put(coverage.getId(), retVal);
				spatialNameMap.put(coverage.getName(), retVal);
			}

		}

		return retVal;
	}

	public GridCoverage getGrid(int id) {
		return spatialMap.get(id);
	}

	public GridCoverage getGrid(String modelName) {
		GridCoverage rval = null;

		if (modelName != null) {
			if (subGridCoverageMap.containsKey(modelName)) {
				rval = spatialMap.get(subGridCoverageMap.get(modelName));
			} else {
				GridModel model = GribModelLookup.getInstance().getModelByName(
						modelName);
				if (model != null) {
					rval = spatialNameMap.get(model.getGrid().toString());
				}
			}
		}

		return rval;
	}

	public GridCoverage getGridByName(String name) {
		return spatialNameMap.get(name);
	}

	/**
	 * Puts a grid into the GribSpatialCache.
	 * 
	 * @param grid
	 *            The grid to store
	 * @param persistToDb
	 *            True if this GridCoverage object is to be persisted to the
	 *            database
	 * @throws GribException
	 *             If problems occur while initializing the grid
	 */
	public void putGrid(GridCoverage grid, boolean initializeGrid,
			boolean persistToDb) throws GribException {
		if (initializeGrid) {
			/*
			 * Prepare the grid to be stored into the cache. Initializes the
			 * geometry and crs objects and generates the id field
			 */
			grid.initialize();
			if (grid.getName() == null) {
				grid.generateName();
			}
		}

		// Persist to the database if desired
		if (persistToDb) {
			new CoreDao(DaoConfig.DEFAULT).saveOrUpdate(grid);
		}

		spatialMap.put(grid.getId(), grid);
		spatialNameMap.put(grid.getName(), grid);
	}

	public SubGrid getSubGrid(String modelName) {
		return definedSubGridMap.get(modelName);
	}

	public GridCoverage getSubGridCoverage(String modelName) {
		GridCoverage rval = null;

		if (subGridCoverageMap.containsKey(modelName)) {
			rval = spatialMap.get(subGridCoverageMap.get(modelName));
		}

		return rval;
	}

	/**
	 * Initializes the predefined set of grids. The grids are stored in xml
	 * format in the utility folder so the localization service has access to
	 * them.<br>
	 * GridCoverage are created from the xml via JaxB and placed in the cache
	 */
	private void initializeGrids() {
		ClusterTask ct = null;

		do {
			ct = ClusterLockUtils.lock("grib", "spatialCache", 120000, true);
		} while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

		try {
			// pull all the coverage from the database
			GridCoverageDao dao = new GridCoverageDao();
			FileDataList previousFdl = getPreviousFileDataList();
			FileDataList currentFdl = generateFileDataList();

			if (isDefintionChanged(previousFdl, currentFdl)) {
				processBaseGridsChanged(dao, currentFdl);
				saveFileDataList(currentFdl);
			} else {
				List<? extends GridCoverage> baseCoverages = dao
						.loadBaseGrids();

				if (baseCoverages != null && baseCoverages.size() > 0) {
					for (Object obj : baseCoverages) {
						try {
							putGrid((GridCoverage) obj, false, false);
						} catch (Exception e) {
							// Log error but do not throw exception, technically
							// is
							// only from initialize which isn't being called
							logger.error(
									"Unable to load grid coverage into cache "
											+ obj, e);
						}
					}
				} else {
					// database wiped/plugin re-initialized need to repopulate
					processBaseGridsChanged(dao, currentFdl);
					saveFileDataList(currentFdl);
				}
			}

			processUnknownGrids(dao);
			processSubGrids(dao, currentFdl);
		} finally {
			ClusterLockUtils.unlock(ct, false);
		}
	}

	/**
	 * A non subgridded definition has been added, deleted, or changed.
	 * Changed/delete both delete all records, models, and coverage defintion.
	 * Then Change/Add put in a new coverage definition.
	 * 
	 * TODO: Post process Unknown definitions to see if they are now known. If
	 * now known delete definitions of unknown.
	 * 
	 * @param dao
	 * @param currentFdl
	 */
	private void processBaseGridsChanged(GridCoverageDao dao,
			FileDataList currentFdl) {
		List<? extends GridCoverage> baseCoverages = dao.loadBaseGrids();
		Map<String, GridCoverage> fileCoverageMap = loadGridDefinitionsFromDisk(currentFdl);

		// update needs to delete all hdf5 same as delete, so update is
		// a delete and then an add to simplify logic and handle primary
		// key changes.
		List<GridCoverage> coveragesToDelete = new LinkedList<GridCoverage>();
		HashSet<String> validDbCoverageNames = new HashSet<String>(
				(int) (baseCoverages.size() * 1.25) + 1);

		Iterator<? extends GridCoverage> iter = baseCoverages.iterator();
		while (iter.hasNext()) {
			GridCoverage dbCov = iter.next();
			GridCoverage fileCoverage = fileCoverageMap.get(dbCov.getName());
			if (!dbCov.equals(fileCoverage)) {
				// coverage not in flat file or coverage has changed,
				// delete coverage old coverage
				coveragesToDelete.add(dbCov);
				iter.remove();
			} else {
				// current coverage still valid
				validDbCoverageNames.add(dbCov.getName());
			}
		}

		// delete grids, models, coverages, and hdf5 for namesToDelete.
		for (GridCoverage cov : coveragesToDelete) {
			logger.info("GridCoverage " + cov.getName()
					+ " has changed.  Deleting out of date data");
			if (!dao.deleteCoverageAssociatedData(cov, true)) {
				logger.warn("Failed to delete GridCoverage " + cov.getName()
						+ ".  Manual intervention required.");
			} else {
				logger.info("GridCoverage successfully deleted");
			}
		}

		// remove the valid db coverages from the map
		fileCoverageMap.keySet().removeAll(validDbCoverageNames);

		// add new grids in bulk
		for (GridCoverage cov : fileCoverageMap.values()) {
			try {
				putGrid(cov, true, false);
			} catch (Exception e) {
				logger.error(
						"Failed to initialize grid definition " + cov.getName(),
						e);
			}
		}

		// bulk persist the spatial maps
		if (spatialMap.size() > 0) {
			dao.persistAll(spatialMap.values());
		}

		for (GridCoverage cov : baseCoverages) {
			try {
				putGrid(cov, false, false);
			} catch (Exception e) {
				logger.error(
						"Failed to initialize grid definition " + cov.getName(),
						e);
			}
		}
	}

	/**
	 * A non subGridd definition has been added, deleted, or changed.
	 * Changed/delete both delete all records, models, and coverage defintion.
	 * Then Change/Add put in a new coverage definition, and also delete any
	 * data associated with base model definition.
	 * 
	 * @param dao
	 * @param currentFdl
	 */
	private void processSubGrids(GridCoverageDao dao, FileDataList currentFdl) {
		List<? extends GridCoverage> oldSubGridCoverages = dao.loadSubGrids();
		Map<String, GridCoverage> fileSubGridCoverageMap = loadSubGridDefinitionsFromDisk(currentFdl);

		// update needs to delete all hdf5 same as delete, so update is
		// a delete and then an add to simplify logic and handle primary
		// key changes.
		List<GridCoverage> coveragesToDelete = new LinkedList<GridCoverage>();
		HashSet<String> validDbCoverageNames = new HashSet<String>(
				(int) (oldSubGridCoverages.size() * 1.25) + 1);

		Iterator<? extends GridCoverage> iter = oldSubGridCoverages.iterator();
		while (iter.hasNext()) {
			GridCoverage dbCov = iter.next();
			GridCoverage fileCoverage = fileSubGridCoverageMap.get(dbCov
					.getName());
			if (!dbCov.equals(fileCoverage)) {
				// coverage not in flat file or coverage has changed,
				// delete coverage
				coveragesToDelete.add(dbCov);
				iter.remove();
			} else {
				// current coverage still valid
				validDbCoverageNames.add(dbCov.getName());
			}
		}

		// delete grids, models, coverages, and hdf5 for namesToDelete.
		for (GridCoverage cov : coveragesToDelete) {
			logger.info("Model "
					+ cov.getSubGridModel()
					+ " has changed subGrid definition, deleting out of date data");
			if (!dao.deleteCoverageAssociatedData(cov, true)) {
				logger.warn("Failed to delete GridCoverage " + cov.getName()
						+ ".  Manual intervention required.");
			} else {
				logger.info("GridModel successfully deleted");
			}
		}

		// remove the valid db coverages from the map
		fileSubGridCoverageMap.keySet().removeAll(validDbCoverageNames);

		// need to delete model information for new adds, as old grid may not
		// have been subgridded
		GribModelDao modelDao = new GribModelDao();
		for (GridCoverage cov : fileSubGridCoverageMap.values()) {
			logger.info("Model "
					+ cov.getSubGridModel()
					+ " has changed subGrid definition, deleting out of date data");
			// look up parent
			if (modelDao.deleteModelAndAssociatedData(cov.getSubGridModel()) < 0) {
				logger.warn("Failed to delete SubGrid Model "
						+ cov.getSubGridModel()
						+ ".  Manual intervention required.");
			} else {
				logger.info("GridModel successfully deleted");
			}
		}

		// add new grids, persisting individually
		for (GridCoverage cov : fileSubGridCoverageMap.values()) {
			try {
				putGrid(cov, true, true);
				subGridCoverageMap.put(cov.getSubGridModel(), cov.getId());
			} catch (Exception e) {
				logger.error(
						"Failed to initialize grid definition " + cov.getName(),
						e);
			}
		}

		// put database grids into map
		for (GridCoverage cov : oldSubGridCoverages) {
			try {
				putGrid(cov, true, true);
				subGridCoverageMap.put(cov.getSubGridModel(), cov.getId());
			} catch (Exception e) {
				logger.error(
						"Failed to initialize grid definition " + cov.getName(),
						e);
			}
		}
	}

	private void processUnknownGrids(GridCoverageDao dao) {
		List<? extends GridCoverage> unknownGrids = dao.loadUnknownGrids();
		for (GridCoverage cov : unknownGrids) {
			try {
				GridCoverage dbCov = getGrid(cov);
				if (!cov.getName().equals(dbCov.getName())) {
					logger.info("Unknown grid " + cov.getName()
							+ " is now mapped by " + dbCov.getName()
							+ ".  Deleting unknown grid");
					dao.deleteCoverageAssociatedData(cov, true);
				}
			} catch (Exception e) {
				logger.error("Erro occurred scanning unknown grids", e);
			}
		}
	}

	private Map<String, GridCoverage> loadSubGridDefinitionsFromDisk(
			FileDataList currentFdl) {
		GribModelLookup gribModelLUT = GribModelLookup.getInstance();
		List<FileData> subGridDefs = currentFdl.getSubGridFileList();
		Map<String, GridCoverage> subGrids = null;

		if (subGridDefs != null && subGridDefs.size() > 0) {
			subGrids = new HashMap<String, GridCoverage>(subGridDefs.size() * 3);
			Coordinate wfoCenterPoint = null;
			String wfo = SiteUtil.getSite();
			GetWfoCenterPoint centerPointRequest = new GetWfoCenterPoint(wfo);
			try {
				wfoCenterPoint = new GetWfoCenterHandler()
						.handleRequest(centerPointRequest);
			} catch (Exception e) {
				logger.error(
						"Failed to generate sub grid definitions.  Unable to lookup WFO Center Point",
						e);
				return new HashMap<String, GridCoverage>(0);
			}

			for (FileData fd : subGridDefs) {
				try {
					SubGridDef subGridDef = loadSubGridDef(fd.getFilePath());

					if (subGridDef != null) {
						String referenceModel = subGridDef.getReferenceModel();

						GridCoverage gridCoverage = getGrid(referenceModel);

						if (gridCoverage != null) {
							Coordinate wfoCenter = MapUtil
									.latLonToGridCoordinate(wfoCenterPoint,
											PixelOrientation.CENTER,
											gridCoverage);

							double xCenterPoint = wfoCenter.x;
							double yCenterPoint = wfoCenter.y;

							double xDistance = subGridDef.getNx() / 2;
							double yDistance = subGridDef.getNy() / 2;
							Coordinate lowerLeftPosition = new Coordinate(
									xCenterPoint - xDistance, yCenterPoint
											+ yDistance);
							Coordinate upperRightPosition = new Coordinate(
									xCenterPoint + xDistance, yCenterPoint
											- yDistance);

							lowerLeftPosition = MapUtil.gridCoordinateToLatLon(
									lowerLeftPosition, PixelOrientation.CENTER,
									gridCoverage);
							upperRightPosition = MapUtil
									.gridCoordinateToLatLon(upperRightPosition,
											PixelOrientation.CENTER,
											gridCoverage);

							subGridDef.setLowerLeftLon(lowerLeftPosition.x);
							subGridDef.setLowerLeftLat(lowerLeftPosition.y);
							subGridDef.setUpperRightLon(upperRightPosition.x);
							subGridDef.setUpperRightLat(upperRightPosition.y);

							// verify numbers in -180 -> 180 range
							subGridDef.setLowerLeftLon(MapUtil
									.correctLon(subGridDef.getLowerLeftLon()));
							subGridDef.setUpperRightLon(MapUtil
									.correctLon(subGridDef.getUpperRightLon()));

							// do a reverse lookup of the model name to get its
							// associated grid id

							for (String modelName : subGridDef.getModelNames()) {
								GridModel model = gribModelLUT
										.getModelByName(modelName);
								if (model != null) {
									GridCoverage baseCoverage = spatialNameMap
											.get(model.getGrid().toString());

									if (baseCoverage != null) {
										SubGrid subGrid = new SubGrid();
										subGrid.setModelName(modelName);
										GridCoverage subGridCoverage = baseCoverage
												.trim(subGridDef, subGrid);
										if (subGridCoverage != null) {
											subGrids.put(
													subGridCoverage.getName(),
													subGridCoverage);
											definedSubGridMap.put(modelName,
													subGrid);
										}
									}
								}
							}
						} else {
							logger.error("Failed to generate sub grid for "
									+ fd.getFilePath()
									+ ".  Unable to determine coverage for referenceModel ["
									+ referenceModel + "]");
						}
					}
				} catch (Exception e) {
					// Log error but do not throw exception
					logger.error(
							"Failed processing sub grid file: "
									+ fd.getFilePath(), e);
				}
			}
		} else {
			subGrids = new HashMap<String, GridCoverage>(0);
		}

		return subGrids;
	}

	/**
	 * Loads and validates subGridDef pointed to by filePath. If definition
	 * empty/invalid returns null.
	 * 
	 * @param filePath
	 * @return
	 */
	private SubGridDef loadSubGridDef(String filePath) {
		SubGridDef rval = null;
		File f = new File(filePath);

		if (f.length() > 0) {
			try {
				rval = (SubGridDef) SerializationUtil
						.jaxbUnmarshalFromXmlFile(f);
				if (rval.getReferenceModel() == null
						|| rval.getModelNames() == null
						|| rval.getModelNames().size() == 0) {
					// sub grid didn't have required definitions
					rval = null;
				}
			} catch (SerializationException e) {
				logger.error("Failed reading sub grid file: " + filePath, e);
			}
		}

		return rval;
	}

	private static boolean isDefintionChanged(FileDataList previousFdl,
			FileDataList currentFdl) {
		boolean rval = true;
		if (currentFdl != null) {
			rval = !currentFdl.equals(previousFdl);
		} else {
			rval = previousFdl != null;
		}

		return rval;
	}

	private FileDataList generateFileDataList() {
		/*
		 * Retrieve the list of files from the localization service
		 */
		IPathManager pm = PathManagerFactory.getPathManager();
		FileDataList fileList = new FileDataList();
		LocalizationContext[] contexts = pm
				.getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);
		fileList.addCoverageFiles(pm.listFiles(contexts, "/grib/grids",
				new String[] { "xml" }, true, true));
		fileList.addSubGridFiles(pm.listFiles(contexts, "/grib/subgrids",
				new String[] { "xml" }, true, true));

		return fileList;
	}

	private FileDataList getPreviousFileDataList() {
		IPathManager pm = PathManagerFactory.getPathManager();
		File previousFileData = pm.getFile(pm.getContext(
				LocalizationType.EDEX_STATIC, LocalizationLevel.CONFIGURED),
				"/grib/gridDefFileListing.xml");
		FileDataList rval = null;

		if (previousFileData.exists() && previousFileData.length() > 0) {
			try {
				Object obj = SerializationUtil
						.jaxbUnmarshalFromXmlFile(previousFileData);
				if (obj instanceof FileDataList) {
					rval = (FileDataList) obj;
				} else {
					logger.error("Error occurred deserializing "
							+ previousFileData.getAbsolutePath()
							+ ", expected type " + FileDataList.class
							+ " received " + obj.getClass());
				}
			} catch (Exception e) {
				logger.error(
						"Error occurred deserializing "
								+ previousFileData.getAbsolutePath(), e);
			}
		}
		return rval;
	}

	private Map<String, GridCoverage> loadGridDefinitionsFromDisk(
			FileDataList currentFdl) {
		List<FileData> coverageFiles = currentFdl.getCoverageFileList();
		Map<String, GridCoverage> fileCoverageMap = new HashMap<String, GridCoverage>(
				(int) (coverageFiles.size() * 1.25) + 1);

		/*
		 * Iterate over file list. Unmarshal to GridCoverage object
		 */
		for (FileData fd : coverageFiles) {
			try {
				GridCoverage grid = (GridCoverage) SerializationUtil
						.jaxbUnmarshalFromXmlFile(fd.getFilePath());
				GridCoverage previousGrid = fileCoverageMap.put(grid.getName(),
						grid);
				if (previousGrid != null) {
					for (FileData fd2 : coverageFiles) {
						GridCoverage grid2 = (GridCoverage) SerializationUtil
								.jaxbUnmarshalFromXmlFile(fd2.getFilePath());
						if (grid.getName().equals(grid2.getName())) {
							logger.error("Grid " + grid.getName()
									+ " has already been defined.  "
									+ fd.getFilePath() + " and "
									+ fd2.getFilePath()
									+ " have same name.  Using "
									+ fd2.getFilePath());
							break;
						}
					}
				}
			} catch (Exception e) {
				// Log error but do not throw exception
				logger.error(
						"Unable to read default grids file: "
								+ fd.getFilePath(), e);
			}
		}

		return fileCoverageMap;
	}

	private void saveFileDataList(FileDataList fdl) {
		try {
			IPathManager pm = PathManagerFactory.getPathManager();
			LocalizationFile lf = pm.getLocalizationFile(
					pm.getContext(LocalizationType.EDEX_STATIC,
							LocalizationLevel.CONFIGURED),
					"/grib/gridDefFileListing.xml");
			SerializationUtil.jaxbMarshalToXmlFile(fdl, lf.getFile()
					.getAbsolutePath());
			lf.save();
		} catch (Exception e) {
			logger.error(
					"Failed to save coverage file data list, coverages may be reloaded on next restart",
					e);
		}
	}

	public static void reinitialize() {
		GribSpatialCache newInstance = new GribSpatialCache();
		instance = newInstance;
	}
}
