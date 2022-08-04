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
package com.raytheon.uf.edex.plugin.mpe.grib;

import java.nio.file.Path;

import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsBooleanField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsPathField;

/**
 * POJO containing configuration information utilized by the Process Grib Files
 * runner.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2016 5631       bkowal      Initial creation
 * Oct 19, 2016 5631       bkowal      Implemented {@link IProcessGribFilesConfig}.
 *
 * </pre>
 *
 * @author bkowal
 */

public class ProcessGribFilesConfig implements IProcessGribFilesConfig {

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.RFCWIDE_XMRG_DIR)
    private Path xmrgRfcWidePath;

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.MPE_QPE_SBN_DIR)
    private Path qpeSbnPath;

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.MPE_QPE_GRIB_SBN_DIR)
    private Path qpeGribSbnDirPath;

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.RFCWIDE_OUTPUT_DIR)
    private Path rfcWideOutputPath;

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.D2D_INPUT_DIR)
    private Path d2dInputPath;

    @AppsDefaultsPathField(property = MpeConstants.AppsDefaults.MPE_GRIB_DIR)
    private Path gribPath;

    @AppsDefaultsBooleanField(property = MpeConstants.AppsDefaults.MPE_SEND_QPE_TO_SBN, defaultValue = false)
    private boolean sendQpeToSbn;

    public ProcessGribFilesConfig() {
    }

    public Path getXmrgInputPath() {
        return getXmrgRfcWidePath();
    }

    public Path getGribOutputPath() {
        return getGribOutputPath();
    }

    public Path getXmrgRfcWidePath() {
        return xmrgRfcWidePath;
    }

    public void setXmrgRfcWidePath(Path xmrgRfcWidePath) {
        this.xmrgRfcWidePath = xmrgRfcWidePath;
    }

    public Path getQpeSbnPath() {
        return qpeSbnPath;
    }

    public void setQpeSbnPath(Path qpeSbnPath) {
        this.qpeSbnPath = qpeSbnPath;
    }

    public Path getQpeGribSbnDirPath() {
        return qpeGribSbnDirPath;
    }

    public void setQpeGribSbnDirPath(Path qpeGribSbnDirPath) {
        this.qpeGribSbnDirPath = qpeGribSbnDirPath;
    }

    public Path getRfcWideOutputPath() {
        return rfcWideOutputPath;
    }

    public void setRfcWideOutputPath(Path rfcWideOutputPath) {
        this.rfcWideOutputPath = rfcWideOutputPath;
    }

    public Path getD2dInputPath() {
        return d2dInputPath;
    }

    public void setD2dInputPath(Path d2dInputPath) {
        this.d2dInputPath = d2dInputPath;
    }

    public Path getGribPath() {
        return gribPath;
    }

    public void setGribPath(Path gribPath) {
        this.gribPath = gribPath;
    }

    public boolean isSendQpeToSbn() {
        return sendQpeToSbn;
    }

    public void setSendQpeToSbn(boolean sendQpeToSbn) {
        this.sendQpeToSbn = sendQpeToSbn;
    }
}