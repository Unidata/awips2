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
package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A class that describes a DHR radar record that needs to be ingested.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 09, 2012            dgilling    Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class HPEDhrMessage {

    @DynamicSerializeElement
    private byte[] data;

    @DynamicSerializeElement
    private String radarId;

    @DynamicSerializeElement
    private String dtype;

    @DynamicSerializeElement
    private String dt;

    public byte[] getData() {
        return data;
    }

    /**
     * For use by Thrift serialization/deserialization only. Do not use.
     */
    public HPEDhrMessage() {

    }

    public HPEDhrMessage(byte[] data, String radid, String dtype, String dt) {
        this.data = data;
        this.radarId = radid;
        this.dtype = dtype;
        this.dt = dt;
    }

    public void setData(byte[] data) {
        this.data = data;
    }

    public String getRadarId() {
        return radarId;
    }

    public void setRadarId(String radarId) {
        this.radarId = radarId;
    }

    public String getDtype() {
        return dtype;
    }

    public void setDtype(String dtype) {
        this.dtype = dtype;
    }

    public String getDt() {
        return dt;
    }

    public void setDt(String dt) {
        this.dt = dt;
    }
}
