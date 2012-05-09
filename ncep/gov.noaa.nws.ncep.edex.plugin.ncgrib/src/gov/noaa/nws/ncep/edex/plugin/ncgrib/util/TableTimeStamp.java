/**
 *  TableTimeStamp - A Java class to update some known 
 *  ncep grib2 vars.xml and vcrd.xml and ncgribmodel.xml.
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/2010  	276		   L. Lin      Initial creation
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ncgrib.util;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.edex.util.grib2vars.Grib2VarsTableLookup;
import gov.noaa.nws.ncep.edex.util.grib2vcrd.Grib2VcrdTableLookup;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribModelLookup;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class TableTimeStamp {

    private static long ncepVarsTimeStamp;

    private static long ncepVcrdTimeStamp;

    private static long ncepNcgribModelTimeStamp;

    public TableTimeStamp() {
    }

    public static synchronized void updateXmlTables() throws GribException {

        long ncepVarsFileTime = 0;
        long ncepVcrdFileTime = 0;
        long ncepGribModelFileTime = 0;

        /*
         * Gets all predefined found in the utility directory
         */
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        /* check NCEP VARS control file */
        String ncepVarsPath = "";

        try {
            ncepVarsPath = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "grib2vars.xml")
                    .getCanonicalPath();

        } catch (Exception e) {
            throw new GribException(
                    "Unable to unmarshal ncep grib2vars.xml file");
        }

        File ncepVarsTable = new File(ncepVarsPath);
        try {
            if (ncepVarsTable.exists()) {
                ncepVarsFileTime = ncepVarsTable.lastModified();
                if (ncepVarsFileTime != getNcepVarsTimeStamp()) {
                    System.out
                            .println("NCEP grib2vars.xml has been modified or the first time, so load it ...");
                    Grib2VarsTableLookup.readG2varsTable();
                    setNcepVarsTimeStamp(ncepVarsFileTime);
                }
            }
        } catch (Exception e) {
            throw new GribException("Unable to read ncep grib2vars file");
        }

        /* check NCEP VCRD control file */
        String ncepVcrdPath = "";

        try {
            ncepVcrdPath = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "grib2vcrd.xml")
                    .getCanonicalPath();

        } catch (Exception e) {
            throw new GribException(
                    "Unable to unmarshal ncep grib2vcrd.xml file");
        }

        File ncepVcrdTable = new File(ncepVcrdPath);
        try {
            if (ncepVcrdTable.exists()) {
                ncepVcrdFileTime = ncepVcrdTable.lastModified();
                if (ncepVcrdFileTime != getNcepVcrdTimeStamp()) {
                    System.out
                            .println("NCEP grib2vcrd.xml has been modified or the first time, so load it ...");
                    Grib2VcrdTableLookup.readG2vcrdTable();
                    setNcepVcrdTimeStamp(ncepVcrdFileTime);
                }
            }
        } catch (Exception e) {
            throw new GribException("Unable to read ncep grib2vcrd file");
        }

        /* check NCEP Grib Model control file */
        String ncepGridModelPath = "";

        try {
            ncepGridModelPath = pathMgr.getFile(commonStaticBase,
                    "ncgrid" + File.separator + "ncgribModels.xml")
                    .getCanonicalPath();

        } catch (Exception e) {
            throw new GribException(
                    "Unable to unmarshal ncep ncgribModels.xml file");
        }

        File ncepGribModelTable = new File(ncepGridModelPath);
        try {
            if (ncepGribModelTable.exists()) {
                ncepGribModelFileTime = ncepGribModelTable.lastModified();
                if (ncepGribModelFileTime != getNcepNcgribModelTimeStamp()) {
                    System.out
                            .println("NCEP ncgribModels.xml has been modified or the first time, so load it ...");
                    NcgribModelLookup.ReloadInstance();
                    setNcepNcgribModelTimeStamp(ncepGribModelFileTime);
                }
            }
        } catch (Exception e) {
            throw new GribException("Unable to read ncep grib2vcrd file");
        }

    }

    public static long getNcepVarsTimeStamp() {
        return ncepVarsTimeStamp;
    }

    public static void setNcepVarsTimeStamp(long ncepVarsTimeStamp) {
        TableTimeStamp.ncepVarsTimeStamp = ncepVarsTimeStamp;
    }

    public static long getNcepVcrdTimeStamp() {
        return ncepVcrdTimeStamp;
    }

    public static void setNcepVcrdTimeStamp(long ncepVcrdTimeStamp) {
        TableTimeStamp.ncepVcrdTimeStamp = ncepVcrdTimeStamp;
    }

    public static long getNcepNcgribModelTimeStamp() {
        return ncepNcgribModelTimeStamp;
    }

    public static void setNcepNcgribModelTimeStamp(long ncepNcgribModelTimeStamp) {
        TableTimeStamp.ncepNcgribModelTimeStamp = ncepNcgribModelTimeStamp;
    }

}
