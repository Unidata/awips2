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

import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * The FFGGribPostProcessor is a grib post processor implementation to update
 * the dataURI for FFG grids so grids are not overwritten
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 31, 2010  5875     bphillip    Initial Creation
 * Mar 26, 2013  1821     bsteffen    Optimize FFG version query.
 * Oct 15, 2013  2473     bsteffen    Remove deprecated method calls.
 * Apr 25, 2014  2060     njensen     Remove dependency on grid dataURI column
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class FFGGribPostProcessor implements IDecoderPostProcessor {

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        try {
            GridDao gribDao = (GridDao) PluginFactory.getInstance()
                    .getPluginDao(GridConstants.GRID);

            /*
             * All we want to do is check for pre-existing records, and if some
             * are found, increment the version number in the secondaryId so
             * it's not identified as a duplicate
             */
            DatabaseQuery query = new DatabaseQuery(GridRecord.class);
            query.addReturnedField(GridConstants.SECONDARY_ID);
            query.addQueryParam(GridConstants.DATASET_ID, record.getDatasetId());
            query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, record
                    .getParameter().getAbbreviation());
            query.addQueryParam(GridConstants.LEVEL_ID, record.getLevel()
                    .getId());
            query.addQueryParam(GridConstants.LOCATION_ID, record.getLocation()
                    .getId());
            query.addQueryParam("dataTime.refTime", record.getDataTime()
                    .getRefTime());
            query.addQueryParam("dataTime.fcstTime", record.getDataTime()
                    .getFcstTime());
            query.addQueryParam(GridConstants.ENSEMBLE_ID,
                    record.getEnsembleId());
            List<?> result = gribDao.queryByCriteria(query);

            /*
             * TODO this does not appear to be cluster safe, but we probably
             * dodge it due to the low frequency of this data arriving for the
             * same parameters, time, etc
             */

            // find the highest version number
            int maxVersion = -1;
            for (Object row : result) {
                String secondaryId = (String) row;
                if (secondaryId == null) {
                    continue;
                }
                secondaryId = secondaryId.replace("Version", "");
                try {
                    int version = Integer.parseInt(secondaryId);
                    maxVersion = Math.max(version, maxVersion);
                } catch (NumberFormatException e) {
                    ;// Just move on
                }
            }
            record.setSecondaryId("Version" + (maxVersion + 1));
            record.getInfo().setId(null);
            // clear out dataURI in case it was cached
            record.setDataURI(null);
        } catch (Exception e) {
            throw new GribException("Error decoding FFG grid", e);
        }
        return new GridRecord[] { record };
    }
}
