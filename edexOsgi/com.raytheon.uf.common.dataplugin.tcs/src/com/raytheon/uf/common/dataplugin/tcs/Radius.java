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
package com.raytheon.uf.common.dataplugin.tcs;


/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2010            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class Radius {

    private int KT_FT;

    private int NE;

    private int SE;

    private int SW;

    private int NW;

    private char KFUnit;

    public Radius() {
        this.KFUnit = 'x';
        this.KT_FT = -1;
        this.NE = -1;
        this.SE = -1;
        this.SW = -1;
        this.NW = -1;
    }

    public Radius(char KFUnit, int KT_FT, int NE, int SE, int SW, int NW) {
        this.KFUnit = KFUnit;
        this.KT_FT = KT_FT;
        this.NE = NE;
        this.SE = SE;
        this.SW = SW;
        this.NW = NW;
    }

    public int getKT_FT() {
        return KT_FT;
    }

    public void setKT_FT(int kT_FT) {
        KT_FT = kT_FT;
    }

    public int getNE() {
        return NE;
    }

    public void setNE(int nE) {
        NE = nE;
    }

    public int getSE() {
        return SE;
    }

    public void setSE(int sE) {
        SE = sE;
    }

    public int getSW() {
        return SW;
    }

    public void setSW(int sW) {
        SW = sW;
    }

    public int getNW() {
        return NW;
    }

    public void setNW(int nW) {
        NW = nW;
    }

    public char getKFUnit() {
        return KFUnit;
    }

    public void setKFUnit(char kFUnit) {
        KFUnit = kFUnit;
    }

    public String toString() {
        return KT_FT + " " + KFUnit + " " + NE + "NE " + SE + "SE " + SW
                + "SW " + NW + "NW";
    }
}
