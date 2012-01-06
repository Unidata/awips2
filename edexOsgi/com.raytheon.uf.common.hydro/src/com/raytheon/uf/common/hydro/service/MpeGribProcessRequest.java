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
package com.raytheon.uf.common.hydro.service;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Execute MPE Grib service through the ThriftClient
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2010            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */
@DynamicSerialize
public class MpeGribProcessRequest implements IServerRequest {
    @DynamicSerializeElement
    private String xmrg;

    @DynamicSerializeElement
    private String grib;

    public MpeGribProcessRequest() {

    }

    public MpeGribProcessRequest(String xmrg, String grib) {
        this.setXmrg(xmrg);
        this.setGrib(grib);
    }

    /**
     * @param xmrg
     *            set the xmrg file name
     */
    public void setXmrg(String xmrg) {
        this.xmrg = xmrg;
    }

    /**
     * @return the xmrg file name
     */
    public String getXmrg() {
        return xmrg;
    }

    /**
     * @param grib
     *            set the grib file name
     */
    public void setGrib(String grib) {
        this.grib = grib;
    }

    /**
     * @return the grib file name
     */
    public String getGrib() {
        return grib;
    }

}
