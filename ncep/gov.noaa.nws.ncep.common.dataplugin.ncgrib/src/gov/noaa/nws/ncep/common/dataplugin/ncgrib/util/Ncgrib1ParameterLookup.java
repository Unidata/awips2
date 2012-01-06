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

package gov.noaa.nws.ncep.common.dataplugin.ncgrib.util;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.Ncgrib1Parameter;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

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
public class Ncgrib1ParameterLookup {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance */
    private static Ncgrib1ParameterLookup instance;

    /** The map of Grib 1 parameters */
    private Map<String, Ncgrib1Parameter> parameterMap;

    /**
     * Gets the singleton instance of the Grib1ParameterLookup
     * 
     * @return The singleton instance
     */
    public static synchronized Ncgrib1ParameterLookup getInstance() {
        if (instance == null) {
            instance = new Ncgrib1ParameterLookup();
        }
        return instance;
    }

    /**
     * Private constructor
     */
    private Ncgrib1ParameterLookup() {
        parameterMap = new HashMap<String, Ncgrib1Parameter>();
        try {
            initParameterMap();
        } catch (GribException e) {
            logger.error("Unable to initialize ncgrib 1 parameter list!", e);
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
    public Ncgrib1Parameter getParameter(int center, int grib1TableVersion,
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
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        String path = "";
        try {
            path = pathMgr
                    .getFile(
                            commonStaticBase,
                            "ncgrid" + File.separator
                                    + "ncgrib1ParameterConvTable.xml")
                    .getCanonicalPath();
            // System.out.println(" ncgrib1 path=" + path);
        } catch (IOException e) {
            throw new GribException("Error getting ncgrib1 model definitions",
                    e);
        }

        File parameterFile = new File(path);
        Ncgrib1ParameterSet parameterSet = null;
        try {
            if (parameterFile.exists()) {
                parameterSet = (Ncgrib1ParameterSet) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(parameterFile.getPath());
            } else {
                ArrayList<Ncgrib1Parameter> emptyList = new ArrayList<Ncgrib1Parameter>();
                parameterSet = new Ncgrib1ParameterSet();
                parameterSet.setParameters(emptyList);
            }
        } catch (Exception e) {
            throw new GribException(
                    "Unable to unmarshal ncgrib 1 parameters file");
        }

        CoreDao dao = new CoreDao(DaoConfig.DEFAULT);
        for (Ncgrib1Parameter param : parameterSet.getParameters()) {
            parameterMap.put(getGrib1Hash(param), param);
            param.generateId();
            dao.persist(param);
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
    private String getGrib1Hash(Ncgrib1Parameter param) {
        return getGrib1Hash(param.getCenter(), param.getGrib1TableVersion(),
                param.getGrib1Value());

    }
}
