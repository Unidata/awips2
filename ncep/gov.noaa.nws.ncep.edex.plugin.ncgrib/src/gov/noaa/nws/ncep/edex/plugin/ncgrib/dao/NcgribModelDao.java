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

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;

/**
 * Data access object for retrieving GribModel objects from the database
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
public class NcgribModelDao extends CoreDao {

    /**
     * Creates a new GribModelDao
     */
    public NcgribModelDao() {
        super(DaoConfig.forClass(NcgribModel.class));
    }

    /**
     * Checks the database to see if a model matching the provided model exists
     * already
     * 
     * @param model
     *            The model to check
     * @return The model object from the database.
     * @throws DataAccessLayerException
     *             If problems occur while querying
     */
    public NcgribModel checkModel(NcgribModel model)
            throws DataAccessLayerException {
        if (model.getId() == null) {
            model.generateId();
        }
        return (NcgribModel) this.queryById(model.getId());
    }
}
