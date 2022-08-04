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
package com.raytheon.uf.common.activetable;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * VTEC Change container for VTECTableChangeNotification
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014            randerso    Initial creation
 * Mar 25, 2014 #2884      randerso    Added xxxid to VTECChange
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class VTECChange {
    @DynamicSerializeElement
    private String site;

    @DynamicSerializeElement
    private String pil;

    @DynamicSerializeElement
    private String phensig;

    @DynamicSerializeElement
    private String xxxid;

    public VTECChange() {
    }

    public VTECChange(String site, String pil, String phensig, String xxxid) {
        this.site = site;
        this.pil = pil;
        this.phensig = phensig;
        this.xxxid = xxxid;
    }

    public String getSite() {
        return site;
    }

    public String getPil() {
        return pil;
    }

    public String getPhensig() {
        return phensig;
    }

    public String getXxxid() {
        return xxxid;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public void setPil(String pil) {
        this.pil = pil;
    }

    public void setPhensig(String phensig) {
        this.phensig = phensig;
    }

    public void setXxxid(String xxxid) {
        this.xxxid = xxxid;
    }

    @Override
    public String toString() {
        return String.format("(Site:%s, Pil:%s, PhenSig:%s, xxxID:%s)", site,
                pil, phensig, xxxid);
    }

}