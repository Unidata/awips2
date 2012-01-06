/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
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

package gov.noaa.nws.ncep.edex.util.ncgrib;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Factory class for retrieving level names and parameter names based on
 * information extracted from a grib file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/26/07      381         bphillip    initial check in
 *    
 * 
 * </pre>
 */
public class NcGribPropertiesFactory {

    private XMLConfiguration lookup;

    /** The instance */
    private static NcGribPropertiesFactory instance;

    private int undefinedCounter = 0;

    /**
     * Gets the singleton instance of GribPropertiesFactory
     * 
     * @return The singleton instance of GribPropertiesFactory
     */
    public synchronized static NcGribPropertiesFactory getInstance() {
        if (instance == null) {
            instance = new NcGribPropertiesFactory();
        }
        return instance;
    }

    /**
     * Constructs a new GribPropertiesFactory
     */
    public NcGribPropertiesFactory() {
        String resDir = PropertiesFactory.getInstance().getEnvProperties()
                .getEnvValue("RESFOLDER");
        lookup = new XMLConfiguration();
        try {
            lookup.load(resDir + "/tablelookup.xml");
        } catch (ConfigurationException e) {
            e.printStackTrace();
        }

    }

    /**
     * Retrieves the appropriate grib1 parameter table <br>
     * Refer to the parameter tables located in the res/parameters directory of
     * this plugin
     * 
     * @param center
     *            The orignating center
     * @param subCenter
     *            The originating sub-center
     * @param tableVersion
     *            The table version
     * @return The parameter configuration
     * @throws Exception
     *             If a parameter table does not exist for this
     *             center/sub-center/tableversion combination
     */
    public String getConfig(int center, int subCenter, int tableVersion)
            throws DecoderException {

        // Attempt to retrieve the correct table
        String tableName = lookup.getString("center_" + String.valueOf(center)
                + ".subcenter_" + String.valueOf(subCenter) + ".version_"
                + String.valueOf(tableVersion));

        // No table was found. Look for a default table to use with this
        // center/subcenter.
        if (tableName == null) {
            tableName = lookup.getString("center_" + String.valueOf(center)
                    + ".subcenter_-1.version_" + String.valueOf(tableVersion));
        }

        // If no default table is defined, decoding cannot continue
        if (tableName == null) {
            throw new DecoderException(
                    "Parameter table does not exist for Center: " + center
                            + " Sub-Center: " + subCenter + " Version: "
                            + tableVersion);
        }

        return tableName;
    }

    public int getUndefinedCounter() {
        return undefinedCounter++;
    }

}
