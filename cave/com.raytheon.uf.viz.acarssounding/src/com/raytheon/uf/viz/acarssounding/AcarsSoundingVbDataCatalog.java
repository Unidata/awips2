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
package com.raytheon.uf.viz.acarssounding;

import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.datacatalog.PointDataCatalog;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * Volume browser catalog which enables acars sounding data to work.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2013 2269       bsteffen    Initial javadoc
 * Aug 19, 2013 2269       bsteffen    Fix MDCRS data and switch acars to use
 *                                     nsharp.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class AcarsSoundingVbDataCatalog extends PointDataCatalog {

    @Override
    protected String[] getPlugins(ViewMenu setting) {
        return new String[] { "acarssounding" };
    }

    @Override
    protected AbstractRequestableResourceData getResourceData(
            IDataCatalogEntry catalogEntry, ResourceType resourceType) {
        if (resourceType == ResourceType.SOUNDING
                && catalogEntry.getSelectedData().getSourcesKey()
                        .equals("acarssounding")) {
            return new AcarsSndNSharpResourceData();
        }
        return super.getResourceData(catalogEntry, resourceType);
    }

}
