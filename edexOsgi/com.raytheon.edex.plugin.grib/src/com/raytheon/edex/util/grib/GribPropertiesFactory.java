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

package com.raytheon.edex.util.grib;

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
public class GribPropertiesFactory {

    private XMLConfiguration lookup;

    /** The instance */
    private static GribPropertiesFactory instance;

    private int undefinedCounter = 0;

    /**
     * Gets the singleton instance of GribPropertiesFactory
     * 
     * @return The singleton instance of GribPropertiesFactory
     */
    public synchronized static GribPropertiesFactory getInstance() {
        if (instance == null) {
            instance = new GribPropertiesFactory();
        }
        return instance;
    }

    /**
     * Constructs a new GribPropertiesFactory
     */
    public GribPropertiesFactory() {
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
