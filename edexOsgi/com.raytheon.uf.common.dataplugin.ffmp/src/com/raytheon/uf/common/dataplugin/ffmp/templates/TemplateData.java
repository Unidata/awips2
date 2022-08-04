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
package com.raytheon.uf.common.dataplugin.ffmp.templates;

import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import org.locationtech.jts.geom.Geometry;

/**
 * Holds the maps of template data. Maps extracted from FFMPTemplates.java.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2018 6641       njensen     Extracted from FFMPTemplates.java.
 *
 * </pre>
 *
 * @author njensen
 */

public class TemplateData {

    /**
     * A map where huc is the String key. The Long key is pfafId. If huc is ALL,
     * ? is FFMPBasinMetaData. Otherwise ? is List<Long> where the map is
     * aggregate pfafs to child pfafs.
     */
    protected Map<String, LinkedHashMap<Long, ?>> hucPfafMap = new HashMap<>();

    /**
     * a map of pfaf to virtual gage basins metadata
     */
    protected Map<Long, List<FFMPVirtualGageBasinMetaData>> vgbsInParentPfaf;

    /**
     * a map of county to virtual gage basins meta data
     */
    protected Map<String, List<FFMPVirtualGageBasinMetaData>> vgbsInCounty;

    /**
     * a map of lid to virtual gage basin metadata
     */
    protected LinkedHashMap<String, FFMPVirtualGageBasinMetaData> lidToVGBMap;

    protected SoftReference<Map<Long, Geometry>> cwaRawGeometries;

    public LinkedHashMap<Long, ?> getHucPfafMap(String huc) {
        return hucPfafMap.get(huc);
    }

    public void putInHucPfafMap(String huc, LinkedHashMap<Long, ?> map) {
        hucPfafMap.put(huc, map);
    }

    public List<FFMPVirtualGageBasinMetaData> getVgbsInParentPfaf(Long pfaf) {
        return vgbsInParentPfaf.get(pfaf);
    }

    public List<FFMPVirtualGageBasinMetaData> getVgbsInCounty(String county) {
        return vgbsInCounty.get(county);
    }

    public LinkedHashMap<String, FFMPVirtualGageBasinMetaData> getLidToVGBMap() {
        return lidToVGBMap;
    }

    public void setLidToVGBMap(
            LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map) {
        lidToVGBMap = map;
    }

    public void setVgbsInParentPfaf(
            Map<Long, List<FFMPVirtualGageBasinMetaData>> map) {
        vgbsInParentPfaf = map;
    }

    public void setVgbsInCounty(
            Map<String, List<FFMPVirtualGageBasinMetaData>> map) {
        vgbsInCounty = map;
    }

    public SoftReference<Map<Long, Geometry>> getCwaRawGeometries() {
        return cwaRawGeometries;
    }

    public void setCwaRawGeometries(SoftReference<Map<Long, Geometry>> map) {
        cwaRawGeometries = map;
    }

}
