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

/**
 * 
 */
package com.raytheon.edex.util;

import java.io.Serializable;
import java.text.DecimalFormat;
import java.text.NumberFormat;

/**
 * @author pheaberl
 * 
 */
public class WorldFile implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final NumberFormat worldNumFormat = new DecimalFormat(
            "#.000");

    private double xInc = -1;

    private double yInc = -1;

    private double rotRow = -1;

    private double rotCol = -1;

    private double ulLon = -1;

    private double ulLat = -1;

    /**
     * 
     * 
     */
    public WorldFile() {

    }

    /**
     * @param inc
     * @param inc2
     * @param rotRow
     * @param rotCol
     * @param ulLon
     * @param ulLat
     */
    public WorldFile(double inc, double inc2, double rotRow, double rotCol,
            double ulLon, double ulLat) {
        super();
        xInc = inc;
        yInc = inc2;
        this.rotRow = rotRow;
        this.rotCol = rotCol;
        this.ulLon = ulLon;
        this.ulLat = ulLat;
    }

    public double getRotCol() {
        return rotCol;
    }

    public void setRotCol(double rotCol) {
        this.rotCol = rotCol;
    }

    public double getRotRow() {
        return rotRow;
    }

    public void setRotRow(double rotRow) {
        this.rotRow = rotRow;
    }

    public double getUlLat() {
        return ulLat;
    }

    public void setUlLat(double ulLat) {
        this.ulLat = ulLat;
    }

    public double getUlLon() {
        return ulLon;
    }

    public void setUlLon(double ulLon) {
        this.ulLon = ulLon;
    }

    public double getXInc() {
        return xInc;
    }

    public void setXInc(double inc) {
        xInc = inc;
    }

    public double getYInc() {
        return yInc;
    }

    public void setYInc(double inc) {
        yInc = inc;
    }

    public byte[] getBytes() {
        StringBuffer stringBuffer = new StringBuffer();

        stringBuffer.append(worldNumFormat.format(this.getXInc()) + "\n");
        stringBuffer.append(worldNumFormat.format(this.getRotRow()) + "\n");
        stringBuffer.append(worldNumFormat.format(this.getRotCol()) + "\n");
        stringBuffer.append(worldNumFormat.format(this.getYInc()) + "\n");
        stringBuffer.append(worldNumFormat.format(this.getUlLon()) + "\n");
        stringBuffer.append(worldNumFormat.format(this.getUlLat()) + "\n");

        return stringBuffer.toString().getBytes();
    }

}
