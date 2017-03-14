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
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Vertically Integrated Liquid results.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/02/2009   2037       dhladky     Initial Creation.
 * 10/02/2013   2361       njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class VILReport {

    @DynamicSerializeElement
    private int totalLgt = 0;

    @DynamicSerializeElement
    private int totalMdt = 0;

    @DynamicSerializeElement
    private int totalHvy = 0;

    @DynamicSerializeElement
    private int ovhdLgt = 0;

    @DynamicSerializeElement
    private int ovhdMdt = 0;

    @DynamicSerializeElement
    private int ovhdHvy = 0;

    @DynamicSerializeElement
    private Coordinate cellCoor = null;

    @DynamicSerializeElement
    private Coordinate siteCoor = null;

    private String cellName = null;

    private String siteName = null;

    public VILReport(String cellName, Coordinate cellCoor, String siteName,
            Coordinate siteCoor) {
        setCellName(cellName);
        setCellCoor(cellCoor);
        setSiteName(siteName);
        setSiteCoor(siteCoor);
    }

    /**
     * serializable construct
     */
    public VILReport() {

    }

    public int getTotalLgt() {
        return totalLgt;
    }

    public void setTotalLgt(int totalLgt) {
        this.totalLgt = totalLgt;
    }

    public int getTotalMdt() {
        return totalMdt;
    }

    public void setTotalMdt(int totalMdt) {
        this.totalMdt = totalMdt;
    }

    public int getTotalHvy() {
        return totalHvy;
    }

    public void setTotalHvy(int totalHvy) {
        this.totalHvy = totalHvy;
    }

    public int getOvhdLgt() {
        return ovhdLgt;
    }

    public void setOvhdLgt(int ovhdLgt) {
        this.ovhdLgt = ovhdLgt;
    }

    public int getOvhdMdt() {
        return ovhdMdt;
    }

    public void setOvhdMdt(int ovhdMdt) {
        this.ovhdMdt = ovhdMdt;
    }

    public int getOvhdHvy() {
        return ovhdHvy;
    }

    public void setOvhdHvy(int ovhdHvy) {
        this.ovhdHvy = ovhdHvy;
    }

    public Coordinate getCellCoor() {
        return cellCoor;
    }

    public void setCellCoor(Coordinate cellCoor) {
        this.cellCoor = cellCoor;
    }

    public Coordinate getSiteCoor() {
        return siteCoor;
    }

    public void setSiteCoor(Coordinate siteCoor) {
        this.siteCoor = siteCoor;
    }

    public String getCellName() {
        return cellName;
    }

    public void setCellName(String cellName) {
        this.cellName = cellName;
    }

    public String getSiteName() {
        return siteName;
    }

    public void setSiteName(String siteName) {
        this.siteName = siteName;
    }
}
