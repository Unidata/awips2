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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.grib;

import java.nio.file.Path;

import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsPathField;
import com.raytheon.uf.edex.plugin.mpe.grib.IProcessGribFilesConfig;

/**
 * Abstraction of xmrg to grib configuration for mosaics associated with the
 * "DSP" process identifier.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public abstract class AbstractDSPProcessGribFilesConfig
        implements IProcessGribFilesConfig {

    public static final String PROCESS_ID_MATCH = "DSP";

    @AppsDefaultsPathField(property = HPEFieldgenGribConstants.AppsDefaults.HPE_ERMOSAIC_GRIB_DIR)
    private Path gribOutputPath;

    public AbstractDSPProcessGribFilesConfig() {
    }

    @Override
    public Path getGribOutputPath() {
        return gribOutputPath;
    }

    public void setGribOutputPath(Path gribOutputPath) {
        this.gribOutputPath = gribOutputPath;
    }
}