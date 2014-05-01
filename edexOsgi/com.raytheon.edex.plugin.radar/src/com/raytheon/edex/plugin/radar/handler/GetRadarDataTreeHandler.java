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
package com.raytheon.edex.plugin.radar.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarDataTreeRequest;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Returns the Radar DataTree for a given request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2010 #4473      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GetRadarDataTreeHandler implements
        IRequestHandler<GetRadarDataTreeRequest> {
    private static final String[] RADARFIELDS = { "icao", "productCode",
            "mnemonic", "unit", "primaryElevationAngle" };

    /**
     * Returns the Radar DataTree for the given request. Tree layout is icao,
     * productCode, level for primaryElevationAngle
     * 
     * @param request
     *            Request for a data tree.
     */
    @Override
    public DataTree handleRequest(GetRadarDataTreeRequest request)
            throws Exception {
        DataTree rval = new DataTree();
        DatabaseQuery query = new DatabaseQuery(RadarRecord.class.getName());
        List<String> distinctFields = Arrays.asList(RADARFIELDS);
        query.addDistinctParameter(distinctFields);
        String rdaId = request.getRdaId();

        if (rdaId != null) {
            // Decoder sets icao to lowercase rda_id. icao should be indexed so
            // querying on it will be faster than rda_id
            query.addQueryParam("icao", request.getRdaId().toLowerCase());
        }

        CoreDao dao = new CoreDao(DaoConfig.forClass(RadarRecord.class));
        List<?> queryResults = dao.queryByCriteria(query);
        if (queryResults.size() > 0) {
            LevelFactory lf = LevelFactory.getInstance();
            String source = null;
            Integer dT = new Integer(60);
            String productCode = null;
            String mnemonic = null;
            String unit = null;
            String levelId = null;

            for (Object resultRow : queryResults) {
                if (resultRow.getClass().isArray()) {
                    ArrayList<Object> resultFields = new ArrayList<Object>(
                            Arrays.asList((Object[]) resultRow));
                    // need to do level mapping for primary elevation
                    source = resultFields.get(0).toString();
                    productCode = resultFields.get(1).toString();
                    Double angle = (Double) resultFields.get(4);
                    Level l = lf.getLevel("TILT", angle);
                    levelId = Long.toString(l.getId());
                    Object mnemonicObj = resultFields.get(2);
                    mnemonic = (mnemonicObj != null ? mnemonicObj.toString()
                            : "");
                    Object unitObj = resultFields.get(3);
                    unit = (unitObj != null ? unitObj.toString() : "");

                    rval.addBranch(source, dT, productCode, mnemonic, unit,
                            levelId);
                }
            }
        }

        return rval;
    }

}
