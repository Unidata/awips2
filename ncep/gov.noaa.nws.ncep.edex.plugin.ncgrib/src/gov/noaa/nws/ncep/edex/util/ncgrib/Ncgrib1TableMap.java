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

package gov.noaa.nws.ncep.edex.util.ncgrib;

import java.io.File;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Contains a map grib 1 table aliases
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/19/10      #4634       bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "ncgribTableMap")
public class Ncgrib1TableMap implements ISerializableObject {
    private HashMap<String, String> map = new HashMap<String, String>();

    private static Ncgrib1TableMap instance;

    public static synchronized Ncgrib1TableMap getInstance() {
        if (instance == null) {
            initInstance();
        }
        return instance;
    }

    private Ncgrib1TableMap() {
    }

    private static void initInstance() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        String path = "";
        try {
            path = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "ncgrib1ParamTableMap.xml")
                    .getCanonicalPath();
            System.out.println(" ncgrib1 table map path=" + path);

            File tableFile = new File(path);

            if (tableFile.exists()) {
                instance = (Ncgrib1TableMap) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(tableFile.getPath());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public int[] getTableAlias(int center, int subcenter, int tableVersion) {
        int[] retVal = new int[3];

        // Checks the map using center subcenter and table version
        String mappedVal = map.get(String.valueOf(center) + " "
                + String.valueOf(subcenter) + " "
                + String.valueOf(tableVersion));

        // If not found, check using any subcenter
        if (mappedVal == null) {
            mappedVal = map.get(String.valueOf(center) + " -1 "
                    + String.valueOf(tableVersion));
        }

        // Get the default table if possible
        if (mappedVal == null) {
            mappedVal = map.get(String.valueOf(center) + " -1 -1");
        }

        // Table alias not found
        if (mappedVal == null) {
            retVal[0] = center;
            retVal[1] = subcenter;
            retVal[2] = tableVersion;
        } else {
            String[] tokens = mappedVal.split(" ");
            retVal[0] = Integer.parseInt(tokens[0]);
            retVal[1] = Integer.parseInt(tokens[1]);
            retVal[2] = Integer.parseInt(tokens[2]);
        }
        return retVal;
    }

    /**
     * @return the map
     */
    public HashMap<String, String> getMap() {
        return map;
    }

    /**
     * @param map
     *            the map to set
     */
    public void setMap(HashMap<String, String> map) {
        this.map = map;
    }
}
