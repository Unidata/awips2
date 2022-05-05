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
package com.raytheon.uf.edex.plugin.scan.process;

import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.scan.ScanException;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 *
 * Abstract grid product for SCAN
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Mar 02, 2012           bsteffen  Initial creation
 * Jun 21, 2013  7613     zhao      Modified getGridSQL()
 * Apr 24, 2014  2060     njensen   Updates for removal of grid dataURI column
 * Apr 17, 2015  4260     dhladky   Update default SCAN models to HRRR
 * Apr 04, 2018  6696     randerso  Code cleanup
 *
 * </pre>
 *
 * @author bsteffen
 */
public abstract class GridProduct extends ScanProduct {

    private static final long serialVersionUID = 1L;

    protected static final String GRID = "grid";

    /**
     * Constructor
     *
     * @param uri
     * @param tableType
     * @param filter
     */
    public GridProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    /**
     * Gets a pattern for matching a URI with the specified parameters.
     *
     * @param dataset
     * @param parameter
     * @param levelName
     * @param levelOne
     * @param levelTwo
     * @return
     * @deprecated This entire method should be removed and/or replaced,
     *             possibly by using the PluginNotifier or methods in
     *             DataURIUtil. At present the method is coupled too tightly to
     *             the grid dataURI format.
     */
    @Deprecated
    protected static Pattern getGridPattern(String dataset, String parameter,
            String levelName, String levelOne, String levelTwo) {
        // Format =
        // /pluginName/time/datasetId/secondaryId/ensembleID/gridID/parameterAbbr/levelName/levelOne/levelTwo
        StringBuilder pattern = new StringBuilder("^");
        // pluginName
        pattern.append(DataURI.SEPARATOR);
        pattern.append(GRID);
        // dataTime
        pattern.append(DataURI.SEPARATOR);
        pattern.append(wildCard);
        // datasetId
        pattern.append(DataURI.SEPARATOR);
        pattern.append(dataset);
        // secondaryId
        pattern.append(DataURI.SEPARATOR);
        pattern.append(".*");
        // ensemble ID
        pattern.append(DataURI.SEPARATOR);
        pattern.append(".*");
        // grid ID
        pattern.append(DataURI.SEPARATOR);
        pattern.append(".*");
        // parameterAbbr
        pattern.append(DataURI.SEPARATOR);
        pattern.append(parameter);
        // levelName
        pattern.append(DataURI.SEPARATOR);
        pattern.append(levelName);
        // levelOne
        pattern.append(DataURI.SEPARATOR);
        pattern.append(levelOne);
        // levelTwo
        pattern.append(DataURI.SEPARATOR);
        pattern.append(levelTwo);

        return Pattern.compile(pattern.toString());

    }

    /**
     * Gets the newest grid record that has a reftime newer than the interval
     * back in time
     *
     * @param interval
     *            the number of minutes back from the current time to check for
     *            a matching record
     * @param dataset
     *            the grid dataset
     * @param parameter
     *            the grid parameter
     * @param levelName
     *            the grid level name
     * @param levelOne
     *            the grid level one value
     * @param levelTwo
     *            the grid level two value
     * @return the newest grid record that matches, or null if none are found
     * @throws ScanException
     */
    public static GridRecord getGridRecord(int interval, String dataset,
            String parameter, String levelName, String levelOne,
            String levelTwo) throws ScanException {
        DatabaseQuery dbQuery = new DatabaseQuery(GridRecord.class);
        dbQuery.addQueryParam(GridConstants.DATASET_ID, dataset);
        dbQuery.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, parameter);
        dbQuery.addQueryParam(GridConstants.MASTER_LEVEL_NAME, levelName);
        dbQuery.addQueryParam(GridConstants.LEVEL_ONE, levelOne);
        dbQuery.addQueryParam(GridConstants.LEVEL_TWO, levelTwo);
        dbQuery.addQueryParam(PluginDataObject.REFTIME_ID,
                new Date(SimulatedTime.getSystemTime().getMillis()
                        - (interval * TimeUtil.MILLIS_PER_MINUTE)),
                ">");
        dbQuery.addOrder(PluginDataObject.REFTIME_ID, false);
        dbQuery.addOrder(PluginDataObject.FCSTTIME_ID, false);
        dbQuery.setMaxResults(1);

        try {
            PluginDao dao = PluginFactory.getInstance()
                    .getPluginDao(GridConstants.GRID);
            List<?> list = dao.queryByCriteria(dbQuery);
            GridRecord result = null;
            if (list != null && !list.isEmpty()) {
                result = (GridRecord) list.get(0);
            }
            return result;
        } catch (Exception e) {
            throw new ScanException(
                    "Error querying database for grid record " + dbQuery, e);
        }
    }
}
