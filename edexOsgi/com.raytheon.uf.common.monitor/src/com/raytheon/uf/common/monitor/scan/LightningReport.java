package com.raytheon.uf.common.monitor.scan;

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
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import org.locationtech.jts.geom.Coordinate;

/**
 * A report of lightning
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial Creation
 * Oct 02, 2013 2361       njensen     Removed XML annotations
 * </pre>
 * 
 * 
 */

@DynamicSerialize
public class LightningReport {

    @DynamicSerializeElement
    private int totalPosStrikes = 0;

    @DynamicSerializeElement
    private int totalCGStrikes = 0;

    @DynamicSerializeElement
    private int cgRate = 0;

    @DynamicSerializeElement
    private double percentPos = 0.0;

    @DynamicSerializeElement
    private double lat = 0.0;

    @DynamicSerializeElement
    private double lon = 0.0;

    public LightningReport() {

    }

    public LightningReport(Coordinate location) {
        this.lat = location.y;
        this.lon = location.x;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public double getLat() {
        return lat;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public double getLon() {
        return lon;
    }

    public int getTotalPosStrikes() {
        return totalPosStrikes;
    }

    public void setTotalPosStrikes(int totalPosStrikes) {
        this.totalPosStrikes = totalPosStrikes;
    }

    public int getTotalCGStrikes() {
        return totalCGStrikes;
    }

    public void setTotalCGStrikes(int totalCGStrikes) {
        this.totalCGStrikes = totalCGStrikes;
    }

    public int getCgRate() {
        return cgRate;
    }

    public void setCgRate(int cgRate) {
        this.cgRate = cgRate;
    }

    public double getPercentPos() {
        return percentPos;
    }

    public void setPercentPos(double percentPos) {
        this.percentPos = percentPos;
    }
}
