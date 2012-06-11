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

package com.raytheon.edex.plugin.grib;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.DefaultPluginInitializer;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;

/**
 * Initializer implementation for the grib plugin
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
public class GribInitializer extends DefaultPluginInitializer {

	/** The logger */
	protected transient Log logger = LogFactory.getLog(getClass());

	/**
	 * Creates a new GribInitializer instance
	 * 
	 * @param pluginName
	 *            "grib"
	 */
	public GribInitializer(String pluginName) {
		super(pluginName);
	}

	@Override
	public void initializePlugin() throws Exception {
		super.initializePlugin();
		logger.info("Initializing grib plugin");
		GribModelLookup.getInstance();
		GribSpatialCache.getInstance();
		logger.info("Grib plugin initialized");
	}

}
