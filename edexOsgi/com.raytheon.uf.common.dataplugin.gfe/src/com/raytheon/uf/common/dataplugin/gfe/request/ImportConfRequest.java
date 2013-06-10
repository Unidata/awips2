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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip     Initial creation
 * Mar 20, 2013   1447     dgilling     Add support for service backup
 *                                      troubleshooting mode from A1.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class ImportConfRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private String primarySite;

    @DynamicSerializeElement
    private String failedSite;

    @DynamicSerializeElement
    private boolean trMode;

    public ImportConfRequest() {

    }

    public ImportConfRequest(String primarySite, String failedSite,
            boolean trMode) {
        this.primarySite = primarySite;
        this.failedSite = failedSite;
        this.trMode = trMode;
    }

    /**
     * @return the primarySite
     */
    public String getPrimarySite() {
        return primarySite;
    }

    /**
     * @param primarySite
     *            the primarySite to set
     */
    public void setPrimarySite(String primarySite) {
        this.primarySite = primarySite;
    }

    /**
     * @return the failedSite
     */
    public String getFailedSite() {
        return failedSite;
    }

    /**
     * @param failedSite
     *            the failedSite to set
     */
    public void setFailedSite(String failedSite) {
        this.failedSite = failedSite;
    }

    public void setTrMode(boolean trMode) {
        this.trMode = trMode;
    }

    public boolean isTrMode() {
        return trMode;
    }

}
