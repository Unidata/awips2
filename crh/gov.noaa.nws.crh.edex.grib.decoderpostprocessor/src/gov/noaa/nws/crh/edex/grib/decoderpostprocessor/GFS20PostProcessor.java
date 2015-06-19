package gov.noaa.nws.crh.edex.grib.decoderpostprocessor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.grib.decoderpostprocessors.ThreeHrPrecipGridProcessor;
import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Grib post processor implementation to generate 3-hr precipitation grids from
 * the alternating (3-hr, 6-hr, 3-hr, 3-hr, 6-hr, etc.) precip grids in the
 * GFS20 output.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 08, 2015           M. Foster   Initial Creation
 *
 *
 * </pre>
 *
 * @author matthew.foster
 * @version 1.0
 */

public class GFS20PostProcessor extends ThreeHrPrecipGridProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        // Post process the data if this is a Total Precipitation grid
        if (record.getParameter().getAbbreviation().equals("TP6hr")) {
            return super.process(record);
        }
        return new GridRecord[] { record };
    }

    /**
     * Retrieves a List of GridRecord via DAO query for the given datasetId, parm
     * and refTime.
     *
     * @param datasetId
     *          The datasetId from which to retrieve the GridRecords
     * @param parm
     *          The parameter for which to retrieve GridRecords
     * @param refTime
     *          The reference (cycle) time for the aforementioned datasetId
     * @return
     * @throws GribException
     */
    @SuppressWarnings("unchecked")
    protected List<GridRecord> getPrecipInventory(String datasetId, String parm,
                                                  Date refTime) throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, parm);
        query.addQueryParam(GridConstants.DATASET_ID, datasetId);
        query.addQueryParam("dataTime.refTime", refTime);
        query.addOrder("dataTime.fcstTime", true);
        try {
            return (List<GridRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for "+datasetId, e);
        }
    }

    /**
     * Generates the 3 hour accumulated grid by taking the difference of the
     * current 6-hr accumulation and the previous 3-hr accumulation.
     * This function will look in the inventory and generate any 3-hr grids
     * that can be generated.
     *
     * @param record
     *            The grib record for which to generate the 3 hour accumulated
     *            precipitation grid
     * @return The generated 3-hr precipitation grids
     * @throws GribException
     */
    protected synchronized GridRecord[] generate3hrPrecipGrids(GridRecord record)
            throws GribException {

        // The current 6-hr precipitation grid inventory in the database
        List<GridRecord> precip6hrInventory = getPrecipInventory(record.getDatasetId(),
                "TP6hr", record.getDataTime().getRefTime());

        // The current 3-hr precipitation grid inventory in the database
        List<GridRecord> precip3hrInventory = getPrecipInventory(record.getDatasetId(),
                "TP3hr", record.getDataTime().getRefTime());
        
        // Make a list of the 3-hr forecast times
        List<Integer> precip3hrTimes = new ArrayList<Integer>();
        for (int i=0; i < precip3hrInventory.size(); i++) {
            precip3hrTimes.add(precip3hrInventory.get(i)
                    .getDataTime().getFcstTime());
        }

        // Adds the current record to the precip inventory
        float[] currentData = (float[]) record.getMessageData();
        record.setMessageData(currentData);
        precip6hrInventory.add(record);

        // Examine each grid in the inventory and generate the 3hr precipitation
        // grid if possible
        List<GridRecord> generatedRecords = new ArrayList<GridRecord>();
        for (int i = 0; i < precip6hrInventory.size(); i++) {
            // Check if the 3hr precipitation grid has already been produced
            if (!precip3hrTimes.contains(precip6hrInventory.get(i)
                    .getDataTime().getFcstTime())) {
                // If the precipitation grid has not been produced, generate it
                List<GridRecord> generated3hrPrecips = generate3hrPrecip(
                        precip6hrInventory.get(i), precip3hrInventory);
                for (GridRecord newRecord : generated3hrPrecips) {
                    // Add the generated grid to the current inventory
                    if (newRecord != null) {
                        precip3hrTimes.add(newRecord.getDataTime()
                                .getFcstTime());
                        generatedRecords.add(newRecord);
                    }
                }
            }
        }

        return generatedRecords.toArray(new GridRecord[] {});
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void calculatePrecipValues(float[] inventoryData, float[] newData) {
        for (int i = 0; i < inventoryData.length; i++) {
            newData[i] = newData[i] - inventoryData[i];
            if (newData[i] < 0) {
                newData[i] = 0;
            }
        }
    }

}
