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

package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import com.raytheon.edex.plugin.DefaultPluginInitializer;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.spatial.NcgribSpatialCache;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribModelLookup;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribTableLookup;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgridTranslator;

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
public class NcgribInitializer extends DefaultPluginInitializer {

    /**
     * Creates a new GribInitializer instance
     * @param pluginName "ncgrib"
     */
    public NcgribInitializer(String pluginName) {
        super(pluginName);
    }

    @Override
    public void initializePlugin() throws Exception {
        super.initializePlugin(); 
        NcgribTableLookup.getInstance();
        NcgribSpatialCache.getInstance();
        NcgribModelLookup.getInstance();
        NcgridTranslator.getInstance();
    }

}
