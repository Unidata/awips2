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
package com.raytheon.uf.viz.monitor.safeseas.threshold;

public class SSDispMonSwellData
{
    private String areaID;
    
    /*
     * Swell Period is ranked from low to high by default
     */
    private boolean rankSwellPeriodHigh = false;
    
    private double priSwellHeightR;
    private double priSwellHeightY;
    private double priSwellPeriodR;
    private double priSwellPeriodY;
    private double priSwellDirFromR;
    private double priSwellDirFromY;
    private double priSwellDirToR;
    private double priSwellDirToY;
    
    private double secSwellHeightR;
    private double secSwellHeightY;
    private double secSwellPeriodR;
    private double secSwellPeriodY;
    private double secSwellDirFromR;
    private double secSwellDirFromY;
    private double secSwellDirToR;
    private double secSwellDirToY;
    
    public SSDispMonSwellData()
    {        
    }

    public String getAreaID() {
        return areaID;
    }

    public void setAreaID(String areaID) {
        this.areaID = areaID;
    }

    public double getPriSwellHeightR() {
        return priSwellHeightR;
    }

    public void setPriSwellHeightR(double priSwellHeightR) {
        this.priSwellHeightR = priSwellHeightR;
    }

    public double getPriSwellHeightY() {
        return priSwellHeightY;
    }

    public void setPriSwellHeightY(double priSwellHeightY) {
        this.priSwellHeightY = priSwellHeightY;
    }

    public double getPriSwellPeriodR() {
        return priSwellPeriodR;
    }

    public void setPriSwellPeriodR(double priSwellPeriodR) {
        this.priSwellPeriodR = priSwellPeriodR;
    }

    public double getPriSwellPeriodY() {
        return priSwellPeriodY;
    }

    public void setPriSwellPeriodY(double priSwellPeriodY) {
        this.priSwellPeriodY = priSwellPeriodY;
    }

    public double getPriSwellDirFromR() {
        return priSwellDirFromR;
    }

    public void setPriSwellDirFromR(double priSwellDirFromR) {
        this.priSwellDirFromR = priSwellDirFromR;
    }

    public double getPriSwellDirFromY() {
        return priSwellDirFromY;
    }

    public void setPriSwellDirFromY(double priSwellDirFromY) {
        this.priSwellDirFromY = priSwellDirFromY;
    }

    public double getPriSwellDirToR() {
        return priSwellDirToR;
    }

    public void setPriSwellDirToR(double priSwellDirToR) {
        this.priSwellDirToR = priSwellDirToR;
    }

    public double getPriSwellDirToY() {
        return priSwellDirToY;
    }

    public void setPriSwellDirToY(double priSwellDirToY) {
        this.priSwellDirToY = priSwellDirToY;
    }

    public double getSecSwellHeightR() {
        return secSwellHeightR;
    }

    public void setSecSwellHeightR(double secSwellHeightR) {
        this.secSwellHeightR = secSwellHeightR;
    }

    public double getSecSwellHeightY() {
        return secSwellHeightY;
    }

    public void setSecSwellHeightY(double secSwellHeightY) {
        this.secSwellHeightY = secSwellHeightY;
    }

    public double getSecSwellPeriodR() {
        return secSwellPeriodR;
    }

    public void setSecSwellPeriodR(double secSwellPeriodR) {
        this.secSwellPeriodR = secSwellPeriodR;
    }

    public double getSecSwellPeriodY() {
        return secSwellPeriodY;
    }

    public void setSecSwellPeriodY(double secSwellPeriodY) {
        this.secSwellPeriodY = secSwellPeriodY;
    }

    public double getSecSwellDirFromR() {
        return secSwellDirFromR;
    }

    public void setSecSwellDirFromR(double secSwellDirFromR) {
        this.secSwellDirFromR = secSwellDirFromR;
    }

    public double getSecSwellDirFromY() {
        return secSwellDirFromY;
    }

    public void setSecSwellDirFromY(double secSwellDirFromY) {
        this.secSwellDirFromY = secSwellDirFromY;
    }

    public double getSecSwellDirToR() {
        return secSwellDirToR;
    }

    public void setSecSwellDirToR(double secSwellDirToR) {
        this.secSwellDirToR = secSwellDirToR;
    }

    public double getSecSwellDirToY() {
        return secSwellDirToY;
    }

    public void setSecSwellDirToY(double secSwellDirToY) {
        this.secSwellDirToY = secSwellDirToY;
    }

    public void updateData(SSDispMonSwellData newData)
    {
        priSwellHeightR = newData.getPriSwellHeightR();
        priSwellHeightY = newData.getPriSwellHeightY();
        priSwellPeriodR = newData.getPriSwellPeriodR();
        priSwellPeriodY = newData.getPriSwellPeriodY();
        priSwellDirFromR = newData.getPriSwellDirFromR();
        priSwellDirFromY = newData.getPriSwellDirFromY();
        priSwellDirToR = newData.getPriSwellDirToR();
        priSwellDirToY = newData.getPriSwellDirToY();
        
        secSwellHeightR = newData.getSecSwellHeightR();
        secSwellHeightY = newData.getSecSwellHeightY();
        secSwellPeriodR = newData.getSecSwellPeriodR();
        secSwellPeriodY = newData.getSecSwellPeriodY();
        secSwellDirFromR = newData.getSecSwellDirFromR();
        secSwellDirFromY = newData.getSecSwellDirFromY();
        secSwellDirToR = newData.getSecSwellDirToR();
        secSwellDirToY = newData.getSecSwellDirToY();
    }

	public void setRankSwellPeriodHigh(boolean rankSwellPeriodHigh) {
		this.rankSwellPeriodHigh = rankSwellPeriodHigh;
	}

	public boolean isRankSwellPeriodHigh() {
		return rankSwellPeriodHigh;
	}
}
