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
package gov.noaa.nws.ncep.edex.uengine.tasks.ncgrib;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree.NcDataTree;

/**
 * The NcgridCatalog script task is used to retrieve the necessary elements needed
 * to populate a grid tree. This includes center id, subcenter id, generating
 * process, parameter abbreviation, and level info.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 10, 2008				brockwoo	Initial creation
 * May 1, 2009  2321        brockwoo    Check if a bad row out of grib model is returned
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class NcgridCatalog extends ScriptTask {

    protected final transient static Log logger = LogFactory
            .getLog(NcgridCatalog.class);

    private static final String[] GRIDFIELDS = { "modelName",
             "eventName", "parm", "vcord",
            "glevel1", "glevel2" };
    
    public static final String PLUGIN_NAME_QUERY = "pluginName";

    public static final String MODEL_NAME_QUERY = "modelName";
    
    public static final String EVENT_NAME_QUERY = "eventName";

    public static final String PARAMETER_QUERY = "parm";

    public static final String LEVEL_ID_QUERY = "vcord";

    public static final String LEVEL_ONE_QUERY = "glevel1";

    public static final String LEVEL_TWO_QUERY = "glevel2";


    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() throws EdexException {
        NcDataTree ncgridTree = null;
        CoreDao ncgribDao = null;
        List<?> queryResults = null;
        ncgribDao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));

        // if we do not get a table back, just return an empty list
        ncgridTree = new NcDataTree();
        DatabaseQuery query = new DatabaseQuery(NcgribRecord.class.getName());

        List<String> distinctFields = Arrays.asList(GRIDFIELDS);

        query.addOrder(distinctFields.get(0), true);
        query.addDistinctParameter(distinctFields);
        queryResults = ncgribDao.queryByCriteria(query);
        if (queryResults.size() > 0) {
            for (Object gridField : queryResults) {
                if (gridField.getClass().isArray()) {
                    ArrayList<Object> gridFields = new ArrayList<Object>(Arrays
                            .asList((Object[]) gridField));
                    String model = gridFields.get(0).toString();
                    String event = gridFields.get(1).toString();
                    String parm = gridFields.get(2).toString();
                    String vcord = gridFields.get(3).toString();
                    String level1 = gridFields.get(4).toString();
                    String level2 = gridFields.get(5).toString();

                    Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
                    rcMap.put(PLUGIN_NAME_QUERY, new RequestConstraint("ncgrib"));
                    rcMap.put(MODEL_NAME_QUERY,
                            new RequestConstraint(model));
                    rcMap.put(EVENT_NAME_QUERY,
                            new RequestConstraint(event));
                    rcMap.put(PARAMETER_QUERY,
                            new RequestConstraint(parm));
                    rcMap.put(LEVEL_ID_QUERY,
                            new RequestConstraint(vcord));
                    rcMap.put(LEVEL_ONE_QUERY,
                            new RequestConstraint(level1));
                    rcMap.put(LEVEL_TWO_QUERY,
                            new RequestConstraint(level2));
                    ncgridTree.addBranch(model, gridFields.get(1)
                            .toString(), gridFields.get(2).toString(),
                            gridFields.get(3).toString(), gridFields.get(4)
                                    .toString(), gridFields.get(5).toString(), rcMap);
                }
            }
        }
        return ncgridTree;
    }

}
