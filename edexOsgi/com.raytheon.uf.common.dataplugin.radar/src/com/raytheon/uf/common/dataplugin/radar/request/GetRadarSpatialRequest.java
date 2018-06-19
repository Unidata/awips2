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
package com.raytheon.uf.common.dataplugin.radar.request;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request for a {@link RadarStation}. A RadarStation can be queried by either
 * the ICAO or a latitude and longitude. If querying by the latitude and
 * longitude, the closest RadarStation will be returned.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 18, 2009 #3922      rjpeter     Initial creation
 * May 11, 2017 #6266      nabowle     Add icao.
 *
 * </pre>
 *
 * @author rjpeter
 */
@DynamicSerialize
public class GetRadarSpatialRequest implements IServerRequest {

    @DynamicSerializeElement
    private double lat;

    @DynamicSerializeElement
    private double lon;

    @DynamicSerializeElement
    private String icao;

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    /**
     * Longitude in degrees -180 -> 180
     *
     * @return
     */
    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;

        if (this.lon > 180) {
            this.lon -= 360;
        }
    }

    public String getIcao() {
        return icao;
    }

    public void setIcao(String id) {
        this.icao = id;
    }

}
