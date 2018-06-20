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

import com.raytheon.uf.edex.plugin.mpe.grib.IProcessGribFilesConfig;

/**
 * Simple POJO used to keep track of a {@link IProcessGribFilesConfig} and the
 * associated grib output directory Apps Defaults property. An instance of this
 * POJO will generally be mapped to a {@link HPEProcessGribFilesConfigLookupKey}
 * .
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPEFieldgenGribConfigWrapper {

    private final Class<? extends IProcessGribFilesConfig> configClass;

    /*
     * Note: this property only exists to handle the parallel run case. TODO:
     * Once the Java no longer needs to run in parallel with the C++, this
     * property and all usage can be removed.
     */
    private final String gribOutputProperty_tmp;

    public HPEFieldgenGribConfigWrapper(
            final Class<? extends IProcessGribFilesConfig> configClass,
            final String gribOutputProperty_tmp) {
        this.configClass = configClass;
        this.gribOutputProperty_tmp = gribOutputProperty_tmp;
    }

    public Class<? extends IProcessGribFilesConfig> getConfigClass() {
        return configClass;
    }

    public String getGribOutputProperty_tmp() {
        return gribOutputProperty_tmp;
    }
}