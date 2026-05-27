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
package com.raytheon.uf.common.dataplugin.radar.response;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response object to a GetRadarHDF5Request.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2014  3393       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@DynamicSerialize
public class GetRadarDataRecordResponse {

    @DynamicSerializeElement
    private RadarDataRecord[] data;

    /**
     * Constructor.
     */
    public GetRadarDataRecordResponse() {
        super();
    }

    /**
     * Constructor.
     *
     * @param datas
     *            The response data.
     */
    public GetRadarDataRecordResponse(RadarDataRecord... datas) {
        super();
        this.data = datas;
    }

    /**
     * @return the data
     */
    public RadarDataRecord[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(RadarDataRecord[] data) {
        this.data = data;
    }

}
