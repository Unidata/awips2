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
package com.raytheon.uf.edex.plugin.vil.common;

import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.raytheon.uf.edex.plugin.vil.VILGenerator;
import com.raytheon.uf.edex.plugin.vil.VILURIFilter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Vil Config, config for VIl processes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/14/2009   2037      dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class VILConfig {

    /** VIL */
    private String vilURI = null;

    /** D VIL */
    private String dvilURI = null;

    /** cuurent ET */
    private String etURI = null;

    /** current EET */
    private String eetURI = null;

    /** VIL Rec */
    private RadarRecord vilRec = null;

    /** D VIL Rec */
    private RadarRecord dvilRec = null;

    /** ET Rec */
    private RadarRecord etRec = null;

    /** EET Rec */
    private RadarRecord eetRec = null;

    /** Our generator reference */
    private VILGenerator vilgen = null;

    /** Spatial Info object */
    private RadarStation spatialInfo = null;

    /** the latitude/longitude */
    private Coordinate latlon = null;

    /** the icao */
    private String icao = null;

    /** dimensions of the data */
    public static final int dimensions = 2;

    /** run or don't run QPF **/
    public boolean mode = false;

    public VILConfig(URIGenerateMessage genMessage, VILGenerator generator)
            throws Exception {

        this.vilgen = generator;
        vilURI = genMessage.getURI(VILURIFilter.vil);
        dvilURI = genMessage.getURI(VILURIFilter.dvil);
        etURI = genMessage.getURI(VILURIFilter.et);
        eetURI = genMessage.getURI(VILURIFilter.eet);

        try {
            vilRec = ScanCommonUtils.getRadarRecord(vilURI);

            if (getVil().getOperationalMode() == 2) {
                setMode(true);
                setIcao(getVil().getIcao());
                setSpatialInfo(getVil().getSpatialObject());
                setLatLon(getVil().getLatitude(), getVil().getLongitude());

                dvilRec = ScanCommonUtils.getRadarRecord(dvilURI);
                etRec = ScanCommonUtils.getRadarRecord(etURI);
                eetRec = ScanCommonUtils.getRadarRecord(eetURI);
            }
        } catch (Exception e) {
            throw new Exception("VILConfig:  VIL cannot run....");
        }
    }

    /**
     * To run or not to run
     * 
     * @param mode
     */
    public void setMode(boolean mode) {
        this.mode = mode;
    }

    /**
     * get whether to run or not
     * 
     * @return
     */
    public boolean getMode() {
        return mode;
    }

    public CompositeProductGenerator getGenerator() {
        return vilgen;
    }

    /**
     * sets the icao for these SCAN values
     * 
     * @param icao
     */
    public void setIcao(String icao) {
        this.icao = icao;
    }

    /**
     * gets the icao for this SCAN
     * 
     * @return
     */
    public String getIcao() {
        return icao;
    }

    /**
     * gets the spatial object
     * 
     * @return
     */
    public RadarStation getSpatialInfo() {
        return spatialInfo;
    }

    /**
     * sets the spatial object
     */
    public void setSpatialInfo(RadarStation spatialinfo) {

        this.spatialInfo = spatialinfo;
    }

    /**
     * Gets the Lat/Lon coord
     * 
     * @return
     */
    public Coordinate getLatLon() {
        return latlon;
    }

    /**
     * set the center lat lon
     * 
     * @param lat
     * @param lon
     */
    public void setLatLon(double lat, double lon) {
        latlon = new Coordinate(lon, lat);
    }

    public RadarRecord getVil() {
        return vilRec;
    }

    public RadarRecord getDVil() {
        return dvilRec;
    }

    public RadarRecord getEt() {
        return etRec;
    }

    public RadarRecord getEet() {
        return eetRec;
    }

}
