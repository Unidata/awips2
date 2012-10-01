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

import java.util.regex.Pattern;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 2, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class GridProduct extends ScanProduct {

    private static final long serialVersionUID = 1L;

    public static String GRID = "grid";

    public GridProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    protected static Pattern getGridPattern(String dataset, String parameter,
            String levelName, String levelOne, String levelTwo) {
        // Format =
        // /pluginName/time/datasetId/secondaryId/locationName/parameterAbbr/levelName/levelOne/levelTwo/perturbation
        StringBuilder pattern = new StringBuilder("^");
        // pluginName
        pattern.append(uriSeparator);
        pattern.append(GRID);
        // dataTime
        pattern.append(uriSeparator);
        pattern.append(wildCard);
        // datasetId
        pattern.append(uriSeparator);
        pattern.append(dataset);
        // secondaryId
        pattern.append(uriSeparator);
        pattern.append(".*");
        // locationName
        pattern.append(uriSeparator);
        pattern.append(".*");
        // parameterAbbr
        pattern.append(uriSeparator);
        pattern.append(parameter);
        // levelName
        pattern.append(uriSeparator);
        pattern.append(levelName);
        // levelOne
        pattern.append(uriSeparator);
        pattern.append(levelOne);
        // levelTwo
        pattern.append(uriSeparator);
        pattern.append(levelTwo);
        // perturbation
        pattern.append(uriSeparator);
        pattern.append("null");
        return Pattern.compile(pattern.toString());

    }

    /**
     * The SQL for MODEL 500 thickness
     * 
     * @return
     */
    public static String getGridSQL(int interval, String dataset,
            String parameter, String levelName, String levelOne, String levelTwo) {
        StringBuilder sql = new StringBuilder(
                "select grid.datauri from grid, grid_info, level where");
        sql.append(" grid.info_id = grid_info.id");
        sql.append(" and");
        // datasetId
        sql.append(" grid_info.datasetId = \'" + dataset + "\'");
        sql.append(" and");
        // parameterAbbreviation
        sql.append(" grid_info.parameter_abbreviation = \'" + parameter + "\'");
        sql.append(" and");
        // level
        sql.append(" level.id = grid_info.level_id");
        sql.append(" and");
        sql.append(" level.masterlevel_name = \'" + levelName + "\'");
        sql.append(" and");
        sql.append(" level.levelonevalue = \'" + levelOne + "\'");
        sql.append(" and");
        sql.append(" level.leveltwovalue = \'" + levelTwo + "\'");
        // interval
        sql.append("and reftime > (now()- interval \'" + interval
                + " minutes\')");
        sql.append(" order by forecasttime desc" + " limit 1");
        return sql.toString();
    }
}
