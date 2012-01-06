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
package com.raytheon.uf.edex.plugin.fog.common;

public class FogGeometry {
    
    public FogGeometry() {
        
    }
    
    public int[] getMinIndex() {
        return minIndex;
    }

    public void setMinIndex(int[] minIndex) {
        this.minIndex = minIndex;
    }

    public int[] getMaxIndex() {
        return maxIndex;
    }

    public void setMaxIndex(int[] maxIndex) {
        this.maxIndex = maxIndex;
    }

    public int getNx() {
        return nx;
    }

    public void setNx(int nx) {
        this.nx = nx;
    }

    public int getNy() {
        return ny;
    }

    public void setNy(int ny) {
        this.ny = ny;
    }

    private int[] minIndex = null;
    
    private int[] maxIndex = null;
    
    /** number of x's for this monitor area **/
    private int nx = 0;
    
    /** number of y's for this monitor area **/
    private int ny = 0;

}
