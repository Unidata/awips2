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
package com.raytheon.uf.edex.ogc.registry;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.ogc.common.db.ICollectorAddon;
import com.raytheon.uf.edex.ogc.common.db.CollectorAddonFactory;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;

/**
 * Provides access to a single CollectorAddon instance through the factory
 * interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2013            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      Adapted to AWIPS
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class RegistryCollectorAddonProvider<D extends SimpleDimension, L extends SimpleLayer<D>, R extends PluginDataObject>
        implements CollectorAddonFactory<D, L, R> {

    private final ICollectorAddon<D, L, R> addon;

    /**
     * @param addon
     */
    public RegistryCollectorAddonProvider(ICollectorAddon<D, L, R> addon) {
        this.addon = addon;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.CollectorAddonFactory#create()
     */
    @Override
    public ICollectorAddon<D, L, R> create() {
        return addon;
    }

}
