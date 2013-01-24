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

package com.raytheon.edex.plugin.grib.util;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Utility class for looking up Grib 1 Parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/9/10       4758        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Grib1ParameterLookup {
    private static final String grib1ParamPath = "grid" + File.separator
            + "grib1ParameterConvTable.xml";

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static Grib1ParameterLookup instance;

    /** The map of Grib 1 parameters */
    private Map<String, Grib1Parameter> parameterMap;

    /**
     * Gets the singleton instance of the Grib1ParameterLookup
     * 
     * @return The singleton instance
     */
    public static synchronized Grib1ParameterLookup getInstance() {
        if (instance == null) {
            instance = new Grib1ParameterLookup();
        }
        return instance;
    }

    /**
     * Private constructor
     */
    private Grib1ParameterLookup() {
        parameterMap = new HashMap<String, Grib1Parameter>();
        try {
            initParameterMap();
        } catch (GribException e) {
            logger.error("Unable to initialize grib 1 parameter list!", e);
        }
    }

    /**
     * Gets a grib 1 parameter
     * 
     * @param center
     *            The center
     * @param grib1TableVersion
     *            The table version
     * @param grib1Value
     *            The table value
     * @return The Grib1Parameter
     */
    public Grib1Parameter getParameter(int center, int grib1TableVersion,
            int grib1Value) {
        return parameterMap.get(getGrib1Hash(center, grib1TableVersion,
                grib1Value));
    }

    /**
     * Initializes the parameter map
     * 
     * @throws GribException
     *             If problems occur while creating the map
     */
    private void initParameterMap() throws GribException {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathManager.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        LocalizationContext commonStaticSite = pathManager.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);
        File baseParameterFile = pathManager.getFile(commonStaticBase,
                grib1ParamPath);
        File siteParameterFile = pathManager.getFile(commonStaticSite,
                grib1ParamPath);

        try {
            if (baseParameterFile.exists()) {
                Grib1ParameterSet parameterSet = (Grib1ParameterSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(baseParameterFile);
                for (Grib1Parameter param : parameterSet.getParameters()) {
                    parameterMap.put(getGrib1Hash(param), param);
                    param.generateId();
                }
            }
            if (siteParameterFile.exists()) {
                Grib1ParameterSet parameterSet = (Grib1ParameterSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(siteParameterFile);
                for (Grib1Parameter param : parameterSet.getParameters()) {
                    parameterMap.put(getGrib1Hash(param), param);
                    param.generateId();
                }
            }
        } catch (Exception e) {
            throw new GribException(
                    "Unable to unmarshal grib 1 parameters file", e);
        }
    }

    /**
     * Creates a map key using the center ID, table version, and table value
     * 
     * @param center
     *            The center ID
     * @param grib1TableVersion
     *            The table version
     * @param grib1Value
     *            The table value
     * @return The hash key
     */
    private String getGrib1Hash(int center, int grib1TableVersion,
            int grib1Value) {
        return String.valueOf(center) + "-" + String.valueOf(grib1TableVersion)
                + "-" + String.valueOf(grib1Value);
    }

    /**
     * Creates a map key using a Grib1Parameter
     * 
     * @param param
     *            The grib 1 parameter
     * @return The hash key
     */
    private String getGrib1Hash(Grib1Parameter param) {
        return getGrib1Hash(param.getCenter(), param.getGrib1TableVersion(),
                param.getGrib1Value());

    }
}
