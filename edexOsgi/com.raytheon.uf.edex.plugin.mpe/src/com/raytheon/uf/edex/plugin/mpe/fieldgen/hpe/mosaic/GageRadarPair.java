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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic;

/**
 * POJO to store gage radar pair information. Based on: gage_radar_pair_struct
 * in /rary.ohd.pproc/inc/mpe_fieldgen.h.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class GageRadarPair {

    private String lid;

    private int hrapX;

    private int hrapY;

    private double gageValue;

    private double radarValue;

    public GageRadarPair() {
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public int getHrapX() {
        return hrapX;
    }

    public void setHrapX(int hrapX) {
        this.hrapX = hrapX;
    }

    public int getHrapY() {
        return hrapY;
    }

    public void setHrapY(int hrapY) {
        this.hrapY = hrapY;
    }

    public double getGageValue() {
        return gageValue;
    }

    public void setGageValue(double gageValue) {
        this.gageValue = gageValue;
    }

    public double getRadarValue() {
        return radarValue;
    }

    public void setRadarValue(double radarValue) {
        this.radarValue = radarValue;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("GageRadarPair [");
        sb.append("lid=").append(lid);
        sb.append(", hrapX=").append(hrapX);
        sb.append(", hrapY=").append(hrapY);
        sb.append(", gageValue=").append(gageValue);
        sb.append(", radarValue=").append(radarValue);
        sb.append("]");
        return sb.toString();
    }
}