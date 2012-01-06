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
package com.raytheon.edex.plugin.ldadprofiler.dao;

import java.sql.SQLException;
import java.util.List;

import com.raytheon.edex.plugin.ldadprofiler.common.ProfilerLdadObs;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Data access object for accessing LDAD Profiler records in the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/07/09                   vkorolev    Initial creation
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

public class LdadProfilerDao extends PointDataPluginDao<ProfilerLdadObs> {
	
	public LdadProfilerDao(String pluginName) throws PluginException {
		super(pluginName);
	}
	
	public LdadProfilerDao() throws PluginException, SQLException {
		this("ldadprofiler");
	}
	/**
	 * @param dataURI
	 * @return
	 */
	public ProfilerLdadObs queryByDataURI(String dataURI) {
		ProfilerLdadObs report = null;
		List<?> obs = null;
		try {
			obs = queryBySingleCriteria("dataURI", dataURI);
		} catch (DataAccessLayerException e) {
			e.printStackTrace();
		}
		if ((obs != null) && (obs.size() > 0)) {
			report = (ProfilerLdadObs) obs.get(0);
		}
		return report;
	}

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(ProfilerLdadObs p) {
        return "ldadprofiler.h5";
    }

    @Override
    public ProfilerLdadObs newObject() {
        return new ProfilerLdadObs();
    }
}	


