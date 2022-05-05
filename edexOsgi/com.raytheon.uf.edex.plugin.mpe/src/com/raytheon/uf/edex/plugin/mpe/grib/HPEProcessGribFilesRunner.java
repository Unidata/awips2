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

/**
 * Converts a xmrg file to a GRIB 1 file when the associated grib save flag is
 * enabled for a hpe fieldgen mosaic. Based on:
 * /files.native/awipsShare/hydroapps/precip_proc/bin/process_hpe_grib_files.
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

public class HPEProcessGribFilesRunner extends AbstractGribFilesRunner {

    public HPEProcessGribFilesRunner(String xmrgInputFile,
            String gribOutputName,
            Class<? extends IProcessGribFilesConfig> configClass,
            String gribOutputPathProperty_tmp) {
        super(xmrgInputFile, gribOutputName, configClass,
                gribOutputPathProperty_tmp);
    }

}