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
package com.raytheon.uf.edex.plugin.modelsounding;

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;

/**
 * Associates a PointDataContainer with a list of plugin data objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2013       2161 bkowal      Initial creation.
 * Feb 09, 2013 4101       rjpeter     Added key.
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class ModelSoundingStorageContainer {
    private final PointDataContainer pdc;

    private final List<PluginDataObject> pdos;

    private final String key;

    /**
     * 
     */
    public ModelSoundingStorageContainer(PointDataContainer pdc, String key) {
        this.pdos = new LinkedList<PluginDataObject>();
        this.pdc = pdc;
        this.key = key;
    }

    public PointDataContainer getPdc() {
        return pdc;
    }

    public List<PluginDataObject> getPdos() {
        return pdos;
    }

    public void addPdo(PluginDataObject pdo) {
        this.pdos.add(pdo);
    }

    public void addPdos(List<PluginDataObject> pdos) {
        this.pdos.addAll(pdos);
    }

    public String getKey() {
        return this.key;
    }

    public int size() {
    	return pdos.size();
    }
}