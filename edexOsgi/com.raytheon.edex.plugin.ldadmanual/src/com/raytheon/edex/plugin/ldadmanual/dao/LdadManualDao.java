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
package com.raytheon.edex.plugin.ldadmanual.dao;

import java.util.List;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Data access object for accessing LDAD MANUAL records in the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/30/09                   vkorolev    Initial creation
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

public class LdadManualDao extends DefaultPluginDao {

	/**
	 * Creates a new Dao
	 * 
	 * @throws PluginException
	 */
	public LdadManualDao(String pluginName) throws PluginException {
		super(pluginName);
	}

	/**
	 * Retrieves an ldadMesonet report using the datauri .
	 * 
	 * @param dataURI
	 *            The dataURI to match against.
	 * @return The report record if it exists.
	 */
	public ManualLdadRecord queryByDataURI(String dataURI) {
		ManualLdadRecord report = null;
		List<?> obs = null;
		try {
			obs = queryBySingleCriteria("dataURI", dataURI);
		} catch (DataAccessLayerException e) {
			e.printStackTrace();
		}
		if ((obs != null) && (obs.size() > 0)) {
			report = (ManualLdadRecord) obs.get(0);
		}
		return report;
	}
}
