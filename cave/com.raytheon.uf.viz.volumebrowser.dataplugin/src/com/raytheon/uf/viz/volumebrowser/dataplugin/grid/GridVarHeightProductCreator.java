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
package com.raytheon.uf.viz.volumebrowser.dataplugin.grid;

import java.util.Collection;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.volumebrowser.dataplugin.grid.GridEnsembleHelper.GridProductCreator;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightRenderableDisplay;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.ProductCreator;
import com.raytheon.viz.volumebrowser.loader.VarHeightProductCreator;

/**
 * 
 * {@link ProductCreator} for loading grid data on a
 * {@link VarHeightRenderableDisplay}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridVarHeightProductCreator extends VarHeightProductCreator
        implements GridProductCreator {

    @Override
    public Collection<ResourcePair> getResourcesToLoad(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        return GridEnsembleHelper.getResourcesToLoad(this, dataCatalog,
                catalogEntry, displayType);
    }

    @Override
    public Collection<ResourcePair> getBaseResourcesToLoad(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        return super.getResourcesToLoad(dataCatalog, catalogEntry, displayType);
    }

}
