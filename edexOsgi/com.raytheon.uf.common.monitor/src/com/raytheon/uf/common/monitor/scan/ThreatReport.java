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
package com.raytheon.uf.common.monitor.scan;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * CWA ThreatReport per SCAN grid box
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/06/2009   2037       dhladky     Initial Creation.
 * 10/02/2013   2361       njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class ThreatReport {

    @DynamicSerializeElement
    private boolean threat = false;

    @DynamicSerializeElement
    private int condition = -1;

    @DynamicSerializeElement
    private boolean cellWithinTenNm = false;

    @DynamicSerializeElement
    private boolean highRefWithinTenNm = false;

    @DynamicSerializeElement
    private boolean coincidentVil = false;

    @DynamicSerializeElement
    private boolean vilWithinTenNm = false;

    @DynamicSerializeElement
    private boolean strongVilCell = false;

    @DynamicSerializeElement
    private boolean ltgActiveCell = false;

    @DynamicSerializeElement
    private double ovhdVilPct = 0;

    @DynamicSerializeElement
    private int totalCvr = 0;

    @DynamicSerializeElement
    private double lgtPct = 0;

    @DynamicSerializeElement
    private double mdtPct = 0;

    @DynamicSerializeElement
    private double hvyPct = 0;

    @DynamicSerializeElement
    private double areaPct = 0;

    @DynamicSerializeElement
    private int cgCountThirtyNm = 0;

    @DynamicSerializeElement
    private int cgNearSite = 0;

    @DynamicSerializeElement
    private int cgRateTenNm = 0;

    @DynamicSerializeElement
    private String vilMessage = null;

    @DynamicSerializeElement
    private String lgtMessage = null;

    @DynamicSerializeElement
    private String tstormMessage = null;

    @DynamicSerializeElement
    private String threatMessage = null;

    /**
     * public constructor
     */
    public ThreatReport() {

    }

    public boolean isThreat() {
        return threat;
    }

    public void setThreat(boolean threat) {
        this.threat = threat;
    }

    public int getCondition() {
        return condition;
    }

    public void setCondition(int condition) {
        this.condition = condition;
    }

    public boolean isCellWithinTenNm() {
        return cellWithinTenNm;
    }

    public void setCellWithinTenNm(boolean cellWithinTenNm) {
        this.cellWithinTenNm = cellWithinTenNm;
    }

    public boolean isHighRefWithinTenNm() {
        return highRefWithinTenNm;
    }

    public void setHighRefWithinTenNm(boolean highRefWithinTenNm) {
        this.highRefWithinTenNm = highRefWithinTenNm;
    }

    public boolean isCoincidentVil() {
        return coincidentVil;
    }

    public void setCoincidentVil(boolean coincidentVil) {
        this.coincidentVil = coincidentVil;
    }

    public boolean isVilWithinTenNm() {
        return vilWithinTenNm;
    }

    public void setVilWithinTenNm(boolean vilWithinTenNm) {
        this.vilWithinTenNm = vilWithinTenNm;
    }

    public boolean isStrongVilCell() {
        return strongVilCell;
    }

    public void setStrongVilCell(boolean strongVilCell) {
        this.strongVilCell = strongVilCell;
    }

    public boolean isLtgActiveCell() {
        return ltgActiveCell;
    }

    public void setLtgActiveCell(boolean ltgActiveCell) {
        this.ltgActiveCell = ltgActiveCell;
    }

    public double getOvhdVilPct() {
        return ovhdVilPct;
    }

    public void setOvhdVilPct(double ovhdVilPct) {
        this.ovhdVilPct = ovhdVilPct;
    }

    public int getTotalCvr() {
        return totalCvr;
    }

    public void setTotalCvr(int totalCvr) {
        this.totalCvr = totalCvr;
    }

    public double getLgtPct() {
        return lgtPct;
    }

    public void setLgtPct(double lgtPct) {
        this.lgtPct = lgtPct;
    }

    public double getMdtPct() {
        return mdtPct;
    }

    public void setMdtPct(double mdtPct) {
        this.mdtPct = mdtPct;
    }

    public double getHvyPct() {
        return hvyPct;
    }

    public void setHvyPct(double hvyPct) {
        this.hvyPct = hvyPct;
    }

    public double getAreaPct() {
        return areaPct;
    }

    public void setAreaPct(double areaPct) {
        this.areaPct = areaPct;
    }

    public int getCgCountThirtyNm() {
        return cgCountThirtyNm;
    }

    public void setCgCountThirtyNm(int cgCountThirtyNm) {
        this.cgCountThirtyNm = cgCountThirtyNm;
    }

    public int getCgNearSite() {
        return cgNearSite;
    }

    public void setCgNearSite(int cgNearSite) {
        this.cgNearSite = cgNearSite;
    }

    public int getCgRateTenNm() {
        return cgRateTenNm;
    }

    public void setCgRateTenNm(int cgRateTenNm) {
        this.cgRateTenNm = cgRateTenNm;
    }

    public String getVilMessage() {
        return vilMessage;
    }

    public void setVilMessage(String vilMessage) {
        this.vilMessage = vilMessage;
    }

    public String getLgtMessage() {
        return lgtMessage;
    }

    public void setLgtMessage(String lgtMessage) {
        this.lgtMessage = lgtMessage;
    }

    public String getTstormMessage() {
        return tstormMessage;
    }

    public void setTstormMessage(String tstormMessage) {
        this.tstormMessage = tstormMessage;
    }

    public String getThreatMessage() {
        return threatMessage;
    }

    public void setThreatMessage(String threatMessage) {
        this.threatMessage = threatMessage;
    }

    /**
     * debugger only
     */
    public String toString() {
        return getThreatMessage() + " " + getTstormMessage() + " "
                + getLgtMessage();
    }

}
