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
package com.raytheon.uf.common.dataplugin.warning.util;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;

/**
 * Information about the county the geometry belongs to
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2010            mschenke     Initial creation
 * Jul 21, 2016 DR 18159  Qinglu Lin   Added copyEntry().
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CountyUserData implements Cloneable {

    public GeospatialData entry;

    public String gid;

    public CoordinateReferenceSystem dataProjection;

    /**
     * @param entry
     * @param gid
     */
    public CountyUserData(GeospatialData entry, String gid) {
        this.entry = entry;
        this.gid = gid;
    }

    @Override
    public String toString() {
        return gid;
    }

    public CountyUserData copyEntry() {
        CountyUserData cud;
        try {
            cud = (CountyUserData)super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException("Error invoking super.clone().", e);
        }
        cud.entry = (GeospatialData) entry.copyAttributes();
        return cud;
    }

}
