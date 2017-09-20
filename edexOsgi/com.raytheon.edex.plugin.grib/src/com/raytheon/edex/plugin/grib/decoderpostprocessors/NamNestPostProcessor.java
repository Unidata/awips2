package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Grib post processor implementation to generate 1-hr precipitation grids from
 * the cycling (1-hr, 2-hr, 3-hr, 1-hr, 2-hr, 3-hr, etc.) precip grids in the
 * NAM Nest output.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 05, 2014           M. Foster   Initial Creation
 * 
 *
 * </pre>
 *
 * @author matthew.foster
 * @version 1.0
 */

public class NamNestPostProcessor extends OneHrPrecipGridProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        // Post process the data if this is a Total Precipitation grid
        if (record.getParameter().getAbbreviation().equals("TP2hr") ||
        	record.getParameter().getAbbreviation().equals("TP3hr")) {
            return super.process(record);
        }
        return new GridRecord[] { record };
    }

    /**
     * Retrieves a grid inventory for the provided datasetid and parameter
     *
     * @param datasetid
     *            The datasetid of the model being worked on
     * @param parm
     *            The parameter being retrieved (e.g. TP3hr)
     * @param refTime
     *            The refTime (cycle time) of the model
     * @return A List of GridRecord
     * @throws GribException
     */
    @SuppressWarnings("unchecked")
    protected List<GridRecord> getPrecipInventory(String datasetid,
    		String parm, Date refTime) throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, parm);
        query.addQueryParam(GridConstants.DATASET_ID, datasetid);
        query.addQueryParam("dataTime.refTime", refTime);
        query.addOrder("dataTime.fcstTime", true);
        try {
            return (List<GridRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    String.format("Error getting Precip inventory for %s!",
                    		datasetid), e);
        }
    }

    /**
     *
     * @param refTime
     *            The reftime (cycle time) of the model being worked on
     * @return List of Integer of the fcstTimes of the current 1hr precip
     *            inventory
     * @throws GribException
     */
    @SuppressWarnings("unchecked")
    protected HashSet<Integer> getPrecip1hrInventory(String datasetId, Date refTime)
    		throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "TP1hr");
        query.addQueryParam(GridConstants.DATASET_ID, datasetId, 
        		QueryOperand.EQUALS);
        query.addQueryParam("dataTime.refTime", refTime);
        query.addReturnedField("dataTime.fcstTime");
        query.setDistinct(true);
        try {
            return new HashSet<Integer>((List<Integer>) dao.queryByCriteria(query));
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for NAMNest!", e);
        }
    }

    /**
     * Generates the 1 hour accumulated grid from the run accumulated
     * precipitation grids. This function will look in the inventory and
     * generate any 1 hr grids that can be generated.
     *
     * @param record
     *            The grib record for which to generate the 1 hour accumulated
     *            precipitation grid
     * @return The generated 1-hr precipitation grids
     * @throws GribException
     */
    protected synchronized GridRecord[] generate1hrPrecipGrids(GridRecord record)
            throws GribException {

    	List<GridRecord> currInventory;
    	List<GridRecord> prevInventory;
    	HashSet<Integer> precip1hrInventory;
    	
    	if (record.getParameter().getAbbreviation().equals("TP3hr")) {
    		// Get an inventory of TP3hr grids
    		currInventory = getPrecipInventory(record.getDatasetId(), "TP3hr",
        			record.getDataTime().getRefTime());
        	
        	// Get an inventory of TP2hr grids
        	prevInventory = getPrecipInventory(record.getDatasetId(), "TP2hr", 
        			record.getDataTime().getRefTime());
        	
        	// The current 1hr precip inventory
        	precip1hrInventory = getPrecip1hrInventory(record.getDatasetId(),
        			record.getDataTime().getRefTime());
        	
    	} else if (record.getParameter().getAbbreviation().equals("TP2hr")) {
    		// Get an inventory of TP2hr grids
        	currInventory = getPrecipInventory(record.getDatasetId(), "TP2hr",
        			record.getDataTime().getRefTime());
        	// Get an inventory of TP1hr grids
        	prevInventory = getPrecipInventory(record.getDatasetId(), "TP1hr", 
        			record.getDataTime().getRefTime());
        	
        	precip1hrInventory = new HashSet<Integer>();
        	for (GridRecord rec : prevInventory) {
        		precip1hrInventory.add(rec.getDataTime().getFcstTime());
        	}
    	} else {
    		throw new GribException("Didn't get TP3hr or TP2hr grid");
    	}

        // Adds the current record to the precip inventory
        float[] currentData = (float[]) record.getMessageData();
        record.setMessageData(currentData);
        currInventory.add(record);

        // Examine each grid in the inventory and generate the 1hr precipitation
        // grid if possible
        List<GridRecord> generatedRecords = new ArrayList<GridRecord>();
        for (GridRecord currRecord : currInventory) {
        	// Check if the 1hr precipitation grid has already been produced
        	if (! precip1hrInventory.contains(currRecord.getDataTime()
        			.getFcstTime())) {
        		List<GridRecord> generated1hrPrecips = generate1hrPrecip(
        				currRecord, prevInventory);
        		for (GridRecord newRecord : generated1hrPrecips) {
        			// Add the generated grid to the current inventory
        			if (newRecord != null) {
        				precip1hrInventory.add(newRecord.getDataTime()
        						.getFcstTime());
        				generatedRecords.add(newRecord);
        			}
        		}
        	}
        }

        return generatedRecords.toArray(new GridRecord[] {});
    }

	/**
     * Calculates the new data by subtracting the previous inventory data from
     * the current data
     * 
     * @param inventoryData
     *            The data from the previous precipitation record
     * @param newData
     *            The data from the current precipitation record
     */
    protected void calculatePrecipValues(float[] inventoryData, float[] newData) {
        for (int i = 0; i < inventoryData.length; i++) {
            newData[i] = newData[i] - inventoryData[i];
            if (newData[i] < 0) {
                newData[i] = 0;
            }
        }
    }
}
