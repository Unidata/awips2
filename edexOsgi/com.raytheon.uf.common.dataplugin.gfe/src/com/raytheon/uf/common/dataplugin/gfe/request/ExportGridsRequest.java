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
package com.raytheon.uf.common.dataplugin.gfe.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to export the specified site's GFE grids for the central server's
 * rsync process to download.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip     Initial creation
 * Apr 29, 2013  #1761     dgilling     Make mode field an Enum.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class ExportGridsRequest extends AbstractGfeRequest {

    public enum ExportGridsMode {
        CRON("-c"), MANUAL("-m"), GRIB2("-g");

        private final String cmdLineArg;

        private ExportGridsMode(String cmdLineArg) {
            this.cmdLineArg = cmdLineArg;
        }

        public String getCmdLineArg() {
            return cmdLineArg;
        }
    }

    @DynamicSerializeElement
    private String site;

    @DynamicSerializeElement
    private ExportGridsMode mode;

    public ExportGridsRequest() {

    }

    public ExportGridsRequest(String site, ExportGridsMode mode) {
        this.site = site;
        this.mode = mode;
    }

    /**
     * @return the site
     */
    public String getSite() {
        return site;
    }

    /**
     * @param site
     *            the site to set
     */
    public void setSite(String site) {
        this.site = site;
    }

    /**
     * @return the mode
     */
    public ExportGridsMode getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(ExportGridsMode mode) {
        this.mode = mode;
    }
}
