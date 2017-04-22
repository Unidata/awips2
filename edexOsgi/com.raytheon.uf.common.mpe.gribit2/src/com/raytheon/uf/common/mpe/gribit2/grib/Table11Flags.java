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
package com.raytheon.uf.common.mpe.gribit2.grib;

/**
 * POJO containing the Table 11 flags that are both used to generate the BDS and
 * that are directly part of the PDS. Reference the ibdsfl array in:
 * /rary.ohd.pproc.gribit/TEXT/engrib.f.
 * 
 * TODO: consider utilizing enums instead of integer constants. Not currently a
 * priority due to the fact that all but two of the flags have been hard-coded
 * in legacy gribit.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class Table11Flags {

    /**
     * data: 0 - grid point data, 1 - spherical harmonic coefficients
     */
    private int data = 0;

    /**
     * packing: 0 - simple, 1 - second order
     */
    private int packing;

    /**
     * original data type: 0 - floating point values, 1 - integer
     */
    private int originalDataType;

    /**
     * octet 14: 0 - no additional flags, 1 - contains flag bits 5-12
     */
    private int octet14 = 0;

    /*
     * reserved field at ibdsfl(5); set to 0. Anything beyond this point is only
     * relevant if octect14 is set to 1.
     */

    /**
     * each grid point: 0 - single datum, 1 - matrix of values
     */
    private int gridPointType = 0;

    /**
     * secondary bit maps: 0 - none, 1 - secondary bit maps present
     */
    private int secondaryBitMaps = 0;

    /**
     * second order values have: 0 - constant width, 1 - different widths
     */
    private int secondOrderValuesWidth = 0;

    /*
     * 4 uninitialized fields - ibdsfl(9) through ibdsfl(12)
     */

    public Table11Flags() {
    }

    public int getData() {
        return data;
    }

    public void setData(int data) {
        this.data = data;
    }

    public int getPacking() {
        return packing;
    }

    public void setPacking(int packing) {
        this.packing = packing;
    }

    public int getOriginalDataType() {
        return originalDataType;
    }

    public void setOriginalDataType(int originalDataType) {
        this.originalDataType = originalDataType;
    }

    public int getOctet14() {
        return octet14;
    }

    public void setOctet14(int octet14) {
        this.octet14 = octet14;
    }

    public int getGridPointType() {
        return gridPointType;
    }

    public void setGridPointType(int gridPointType) {
        this.gridPointType = gridPointType;
    }

    public int getSecondaryBitMaps() {
        return secondaryBitMaps;
    }

    public void setSecondaryBitMaps(int secondaryBitMaps) {
        this.secondaryBitMaps = secondaryBitMaps;
    }

    public int getSecondOrderValuesWidth() {
        return secondOrderValuesWidth;
    }

    public void setSecondOrderValuesWidth(int secondOrderValuesWidth) {
        this.secondOrderValuesWidth = secondOrderValuesWidth;
    }
}