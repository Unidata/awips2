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
package com.raytheon.uf.edex.plugin.mpe.test.config;

import java.nio.file.Path;

import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsPathField;

/**
 * Apps Defaults configuration information used during the Process Grib Files
 * validation test.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2016  4628       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class ProcessGribFilesTestConfig {

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.MPE_GRIB_DIR)
    private Path gribPath;

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.MPE_QPE_GRIB_SBN_DIR)
    private Path qpeGribSbnDirPath;

    public Path getGribPath() {
        return gribPath;
    }

    public void setGribPath(Path gribPath) {
        this.gribPath = gribPath;
    }

    public Path getQpeGribSbnDirPath() {
        return qpeGribSbnDirPath;
    }

    public void setQpeGribSbnDirPath(Path qpeGribSbnDirPath) {
        this.qpeGribSbnDirPath = qpeGribSbnDirPath;
    }
}