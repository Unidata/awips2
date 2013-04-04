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
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Grib post processor implementation to generate 3-hr precipitation grids from
 * run accumulated total precipitation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/18/2012                porricel    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class CanadianRegPostProcessor extends ThreeHrPrecipGridProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        // Post process the data if this is a Total Precipitation grid
        if (record.getParameter().getAbbreviation().equals("TPrun")) {
            return super.process(record);
        }
        return new GridRecord[] { record };
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    protected List<GridRecord> getPrecipInventory(Date refTime)
            throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "TPrun");
        query.addQueryParam(GridConstants.DATASET_ID, "Canadian-Reg");
        query.addQueryParam("dataTime.refTime", refTime);
        query.addOrder("dataTime.fcstTime", true);
        try {
            return (List<GridRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for Canadian-Reg!", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    protected List<Integer> getPrecip3hrInventory(Date refTime)
            throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "TP3hr");
        query.addQueryParam(GridConstants.DATASET_ID, "Canadian-Reg");
        query.addQueryParam("dataTime.refTime", refTime);
        query.addReturnedField("dataTime.fcstTime");
        try {
            return (List<Integer>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for Canadian-Reg!", e);
        }
    }

    /**
     * Generates the 3 hour accumulated grid from the run accumulated
     * precipitation grids. This function will look in the inventory and
     * generate any 3 hr grids that can be generated.
     * 
     * @param record
     *            The grib record for which to generate the 3 hour accumulated
     *            precipitation grid
     * @return The generated 3-hr precipitation grids
     * @throws GribException
     */
    protected synchronized GridRecord[] generate3hrPrecipGrids(GridRecord record)
            throws GribException {

        // The current run accumulated precipitation grid inventory in the
        // database
        List<GridRecord> precipInventory = getPrecipInventory(record
                .getDataTime().getRefTime());

        // The current 3-hr precipitation grid inventory in the database
        List<Integer> precip3hrInventory = getPrecip3hrInventory(record
                .getDataTime().getRefTime());

        // Adds the current record to the precip inventory
        float[] currentData = (float[]) record.getMessageData();
        record.setMessageData(currentData);
        precipInventory.add(record);

        // Examine each grid in the inventory and generate the 3hr precipitation
        // grid if possible
        List<GridRecord> generatedRecords = new ArrayList<GridRecord>();
        for (int i = 0; i < precipInventory.size(); i++) {
            // Check if the 3hr precipitation grid has already been produced
            if (!precip3hrInventory.contains(precipInventory.get(i)
                    .getDataTime().getFcstTime())) {
                // If the precipitation grid has not been produced, generate it
                List<GridRecord> generated3hrPrecips = generate3hrPrecip(
                        precipInventory.get(i), precipInventory,
                        precip3hrInventory);
                for (GridRecord newRecord : generated3hrPrecips) {
                    // Add the generated grid to the current inventory
                    if (newRecord != null) {
                        precip3hrInventory.add(newRecord.getDataTime()
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
