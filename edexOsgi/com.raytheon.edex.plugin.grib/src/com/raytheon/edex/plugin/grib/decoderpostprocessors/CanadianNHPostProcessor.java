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
 * Grib post processor implementation to generate 6-hr precipitation grids from
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
public class CanadianNHPostProcessor extends SixHrPrecipGridProcessor {

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
        query.addQueryParam(GridConstants.DATASET_ID, "Canadian-NH");
        query.addQueryParam("dataTime.refTime", refTime);
        query.addOrder("dataTime.fcstTime", true);
        try {
            return (List<GridRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for Canadian-NH!", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    protected List<Integer> getPrecip6hrInventory(Date refTime)
            throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "TP6hr");
        query.addQueryParam(GridConstants.DATASET_ID, "Canadian-NH");
        query.addQueryParam("dataTime.refTime", refTime);
        query.addReturnedField("dataTime.fcstTime");
        try {
            return (List<Integer>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for Canadian-NH!", e);
        }
    }

    /**
     * Generates the 6 hour accumulated grid from the run accumulated
     * precipitation grids. This function will look in the inventory and
     * generate any 6 hr grids that can be generated.
     * 
     * @param record
     *            The grib record for which to generate the 6 hour accumulated
     *            precipitation grid
     * @return The generated 6-hr precipitation grids
     * @throws GribException
     */
    protected synchronized GridRecord[] generate6hrPrecipGrids(GridRecord record)
            throws GribException {

        // The current run accumulated precipitation grid inventory in the
        // database
        List<GridRecord> precipInventory = getPrecipInventory(record
                .getDataTime().getRefTime());

        // The current 6-hr precipitation grid inventory in the database
        List<Integer> precip6hrInventory = getPrecip6hrInventory(record
                .getDataTime().getRefTime());

        // Adds the current record to the precip inventory
        float[] currentData = (float[]) record.getMessageData();
        record.setMessageData(currentData);
        precipInventory.add(record);

        // Examine each grid in the inventory and generate the 6hr precipitation
        // grid if possible
        List<GridRecord> generatedRecords = new ArrayList<GridRecord>();
        for (int i = 0; i < precipInventory.size(); i++) {
            // Check if the 6hr precipitation grid has already been produced
            if (!precip6hrInventory.contains(precipInventory.get(i)
                    .getDataTime().getFcstTime())) {
                // If the precipitation grid has not been produced, generate it
                List<GridRecord> generated6hrPrecips = generate6hrPrecip(
                        precipInventory.get(i), precipInventory,
                        precip6hrInventory);
                for (GridRecord newRecord : generated6hrPrecips) {
                    // Add the generated grid to the current inventory
                    if (newRecord != null) {
                        precip6hrInventory.add(newRecord.getDataTime()
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
