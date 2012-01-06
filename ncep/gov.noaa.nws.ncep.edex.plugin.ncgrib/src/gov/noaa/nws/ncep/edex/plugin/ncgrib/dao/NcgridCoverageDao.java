/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.edex.plugin.ncgrib.dao;

import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;

/**
 * Data access object for retrieving GridCoverage objects
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
public class NcgridCoverageDao extends CoreDao {

    protected static final float QUERY_TOLERANCE = 0.1f;

    public NcgridCoverageDao() {
        super(DaoConfig.forClass(NcgridCoverage.class));
    }

    public NcgridCoverageDao(DaoConfig config) {
        super(config);
    }

    protected QueryParam addQueryTolerance(String field, double value) {
        String between = String.valueOf(value - QUERY_TOLERANCE) + "--"
                + String.valueOf(value + QUERY_TOLERANCE);
        return new QueryParam(field, between, QueryOperand.BETWEEN);
    }

    @SuppressWarnings("unchecked")
    public NcgridCoverage queryByGridNumber(Integer number)
            throws DataAccessLayerException {
        List<NcgridCoverage> coverages = (List<NcgridCoverage>)this.queryBySingleCriteria("name",
                String.valueOf(number));
        if(coverages.isEmpty()){
            return null;
        }else{
            return coverages.get(0);
        }
    }
}
