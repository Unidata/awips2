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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.nc;

import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsFloatField;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsStringField;

/**
 * Used to load Apps defaults properties required for Field Gen Mosaic NetCDF
 * generation.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class NcConfig {

    @AppsDefaultsStringField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_LOC, required = true)
    private String location;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_SWLAT, required = true)
    private float swLat;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_SWLON, required = true)
    private float swLon;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_SELAT, required = true)
    private float seLat;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_SELON, required = true)
    private float seLon;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_NELAT, required = true)
    private float neLat;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_NELON, required = true)
    private float neLon;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_NWLAT, required = true)
    private float nwLat;

    @AppsDefaultsFloatField(property = HPENcConstants.AppsDefaults.ST3_NETCDF_NWLON, required = true)
    private float nwLon;

    public NcConfig() {
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public float getSwLat() {
        return swLat;
    }

    public void setSwLat(float swLat) {
        this.swLat = swLat;
    }

    public float getSwLon() {
        return swLon;
    }

    public void setSwLon(float swLon) {
        this.swLon = swLon;
    }

    public float getSeLat() {
        return seLat;
    }

    public void setSeLat(float seLat) {
        this.seLat = seLat;
    }

    public float getSeLon() {
        return seLon;
    }

    public void setSeLon(float seLon) {
        this.seLon = seLon;
    }

    public float getNeLat() {
        return neLat;
    }

    public void setNeLat(float neLat) {
        this.neLat = neLat;
    }

    public float getNeLon() {
        return neLon;
    }

    public void setNeLon(float neLon) {
        this.neLon = neLon;
    }

    public float getNwLat() {
        return nwLat;
    }

    public void setNwLat(float nwLat) {
        this.nwLat = nwLat;
    }

    public float getNwLon() {
        return nwLon;
    }

    public void setNwLon(float nwLon) {
        this.nwLon = nwLon;
    }
}