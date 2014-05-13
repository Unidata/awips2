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
package com.raytheon.edex.textdb.dbapi.impl;

import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2008        1538 jkorman     Initial creation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class WMOReportData {

    private WMOHeader wmoHeader;

    private AFOSProductId afosProdId;

    private String reportData;

    /**
     * 
     * @param header
     * @param prodId
     * @param report
     */
    public WMOReportData(WMOHeader header, AFOSProductId prodId, String report) {
        wmoHeader = header;
        copyAFOSProdId(prodId);
        reportData = report;
    }

    /**
     * @return the wmoHeader
     */
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(WMOHeader wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the afosProdId
     */
    public AFOSProductId getAfosProdId() {
        return afosProdId;
    }

    /**
     * @param afosProdId
     *            the afosProdId to set
     */
    public void setAfosProdId(AFOSProductId afosProdId) {
        copyAFOSProdId(afosProdId);
    }

    /**
     * @return the reportData
     */
    public String getReportData() {
        return reportData;
    }

    /**
     * @param reportData
     *            the reportData to set
     */
    public void setReportData(String reportData) {
        this.reportData = reportData;
    }

    private void copyAFOSProdId(AFOSProductId prodId) {
        if (prodId != null) {
            afosProdId = new AFOSProductId(prodId.getCcc(), prodId.getNnn(),
                    prodId.getXxx());
        } else {
            afosProdId = null;
        }
    }

}
