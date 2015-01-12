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
package com.raytheon.uf.edex.plugin.preciprate.common;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.preciprate.PrecipRateGenerator;
import com.raytheon.uf.edex.plugin.preciprate.PrecipRateURIFilter;
import com.raytheon.uf.edex.plugin.preciprate.PrecipRateURIGenerateMessage;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Config holder object for radar parameters used to filter for precipitation
 * rates
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???          ???         ???        Initial creation
 * Aug 26, 2014 3503       bclement    removed unused maxExtent field
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PrecipRateConfig {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrecipRateConfig.class);

    /** generator ref **/
    private PrecipRateGenerator generator = null;

    /** DHR URI **/
    private String dhrURI = null;

    /** DHR record **/
    private RadarRecord dhrRec = null;

    /** Spatial Info object */
    private RadarStation spatialInfo = null;

    /** the icao */
    private String icao = null;

    /** Whether or not to run FFMP **/
    private boolean mode = false;

    /** The geometry for the radar **/
    private GridGeometry2D radarGeometry = null;

    /** create a date formatter for SQL **/
    private SimpleDateFormat dateFmt = new SimpleDateFormat(
            "MMM dd yy HH:mm:ss");

    /** FFMP record reftime as string **/
    private String dateString = null;

    /** FFMP record reftime **/
    private Date date = null;

    /** the latitude/longitude */
    private Coordinate latlon = null;

    public PrecipRateConfig(PrecipRateURIGenerateMessage genMessage,
            PrecipRateGenerator generator) throws Exception {
        this.generator = generator;

        dhrURI = genMessage.getURI(PrecipRateURIFilter.dhr);
        dhrRec = ScanCommonUtils.getRadarRecord(dhrURI);

        if (getDHR().getOperationalMode() == 2) {

            setDate(getDHR().getDataTime().getRefTime());
            dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
            setDateString(dateFmt.format(getDate()));

            setIcao(genMessage.getIcao());
            setMode(true);
            setSpatialInfo(getDHR().getSpatialObject());
            setLatLon(getDHR().getLatitude(), getDHR().getLongitude());
        } else {
            statusHandler.handle(Priority.INFO,
                    "PrecipRate " + genMessage.getIcao()
                            + " not run. Not in Precip Mode. RADAR OP Mode: "
                            + getDHR().getOperationalMode());
        }
    }

    /**
     * set runnable or not
     * 
     * @param mode
     */
    public void setMode(boolean mode) {
        this.mode = mode;
    }

    /**
     * can we run Preciprate?
     * 
     * @return
     */
    public boolean getMode() {
        return mode;
    }

    /**
     * Get DHR
     * 
     * @return
     */
    public RadarRecord getDHR() {
        return dhrRec;
    }

    /**
     * gets The FFMP generator
     * 
     * @return
     */
    public PrecipRateGenerator getGenerator() {
        return generator;
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
     * sets the DHR date
     * 
     * @param date
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * returns a formatted date for DHR
     * 
     * @return
     */
    public Date getDate() {
        return date;
    }

    /**
     * sets the DHR date
     * 
     * @param date
     */
    public void setDateString(String dateString) {
        this.dateString = dateString;
    }

    /**
     * returns a formatted date for DHR
     * 
     * @return
     */
    public String getDateString() {
        return dateString;
    }

    public GridGeometry2D getRadarGeometry() {
        if (radarGeometry == null) {
            ProjectedCRS crs = getDHR().getCRS();
            GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
            generalEnvelope.setCoordinateReferenceSystem(crs);

            double maxExtent = getDHR().getGateResolution()
                    * getDHR().getNumBins()
                    * Math.cos(Math.toRadians(getDHR().getTrueElevationAngle()));

            generalEnvelope.setRange(0, -maxExtent, maxExtent);
            generalEnvelope.setRange(1, -maxExtent, maxExtent);

            radarGeometry = new GridGeometry2D(new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { getDHR().getNumBins(),
                            getDHR().getNumRadials() }, false), generalEnvelope);
        }
        return radarGeometry;
    }

}
