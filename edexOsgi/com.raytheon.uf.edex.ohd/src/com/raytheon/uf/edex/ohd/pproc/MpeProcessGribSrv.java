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

package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.hydro.service.MpeGribProcessRequest;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ohd.MainMethod;

/**
 * Service implementation for executing the Mpe process_grib_files script This
 * saves the Best Estimate QPE as a grib file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2010            snaples     Initial creation
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */
public class MpeProcessGribSrv implements
        IRequestHandler<MpeGribProcessRequest> {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(MpeProcessGribSrv.class);

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    /**
     * The serialized incoming request contains the file names of the xmrg file
     * and the grib file to create. These are passed to the process_grib_files
     * script.
     */
    @Override
    public Object handleRequest(MpeGribProcessRequest request) throws Exception {
        String xmrg = request.getXmrg();
        String grib = request.getGrib();

        if ((xmrg == null) || (grib == null)) {
            throw new EdexException("Invalid argument sent to MpeProcessGrib");
        }
        int exitValue = 0;
        logger.info("Executing process_grib_files with arguments: xmrgfilename: "
                + xmrg + " gribfilename: " + grib);
        exitValue = MainMethod.runProgram("ksh",
                appsDefaults.getToken("pproc_bin") + "/process_grib_files",
                xmrg, grib);
        if (exitValue == 0) {
            logger.info("MpeProcessGrib execution successful");
        } else {
            throw new EdexException(
                    "MpeProcessGrib process terminated abnormally with exit code: "
                            + exitValue);
        }
        return exitValue;
    }

}
