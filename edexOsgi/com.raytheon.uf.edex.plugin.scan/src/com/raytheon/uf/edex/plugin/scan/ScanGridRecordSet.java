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
package com.raytheon.uf.edex.plugin.scan;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;

/**
 * Container for grid records that are potentially used by SCAN
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2014            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ScanGridRecordSet {

    private GridRecord cape;

    private GridRecord heli;

    private GridRecord u500;

    private GridRecord u700;

    private GridRecord v700;

    private GridRecord gh500;

    private GridRecord gh1000;

    public GridRecord getCape() {
        return cape;
    }

    public void setCape(GridRecord cape) {
        this.cape = cape;
    }

    public GridRecord getHeli() {
        return heli;
    }

    public void setHeli(GridRecord heli) {
        this.heli = heli;
    }

    public GridRecord getU500() {
        return u500;
    }

    public void setU500(GridRecord u500) {
        this.u500 = u500;
    }

    public GridRecord getU700() {
        return u700;
    }

    public void setU700(GridRecord u700) {
        this.u700 = u700;
    }

    public GridRecord getV700() {
        return v700;
    }

    public void setV700(GridRecord v700) {
        this.v700 = v700;
    }

    public GridRecord getGh500() {
        return gh500;
    }

    public void setGh500(GridRecord gh500) {
        this.gh500 = gh500;
    }

    public GridRecord getGh1000() {
        return gh1000;
    }

    public void setGh1000(GridRecord gh1000) {
        this.gh1000 = gh1000;
    }

}
