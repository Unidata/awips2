package com.raytheon.uf.edex.datadelivery.harvester.config;

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

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * DD OGC Agent
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Sept, 2012   1038      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class OGCAgent extends Agent {

    /**
     * name path used for WFS
     */
    @XmlElement(name = "wfs")
    @DynamicSerializeElement
    private String wfs = "wfs";

    /**
     * name path used for WMS
     */
    @XmlElement(name = "wms")
    @DynamicSerializeElement
    private String wms = "wms";

    /**
     * name path used for WCS
     */
    @XmlElement(name = "wcs")
    @DynamicSerializeElement
    private String wcs = "wcs";

    public String getWcs() {
        return wcs;
    }

    public String getWfs() {
        return wfs;
    }

    public String getWms() {
        return wms;
    }

    public void setWcs(String wcs) {
        this.wcs = wcs;
    }

    public void setWfs(String wfs) {
        this.wfs = wfs;
    }

    public void setWms(String wms) {
        this.wms = wms;
    }

}
