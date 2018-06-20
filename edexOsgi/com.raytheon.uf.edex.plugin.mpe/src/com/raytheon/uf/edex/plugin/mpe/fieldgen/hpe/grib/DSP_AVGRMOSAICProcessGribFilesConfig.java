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

import com.raytheon.uf.common.ohd.AppsDefaultsDirKeys;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsPathField;

/**
 * Implementation of a {@link AbstractDSPProcessGribFilesConfig} for xmrg files
 * produced for the AVGRMOSAIC mosaic.
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

public class DSP_AVGRMOSAICProcessGribFilesConfig
        extends AbstractDSPProcessGribFilesConfig {

    public static final String XMRG_MATCH = "AVGRMOSAIC";

    @AppsDefaultsPathField(property = AppsDefaultsDirKeys.HPE_AVG_ERMOSAIC_DIR)
    private Path xmrgInputPath;

    public DSP_AVGRMOSAICProcessGribFilesConfig() {
    }

    @Override
    public Path getXmrgInputPath() {
        return xmrgInputPath;
    }
}