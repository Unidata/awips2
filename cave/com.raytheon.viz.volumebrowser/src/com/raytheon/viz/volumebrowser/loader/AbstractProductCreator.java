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
package com.raytheon.viz.volumebrowser.loader;

import java.util.Arrays;
import java.util.Collection;

import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.core.D2DLoadProperties;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.SpaceTimeMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * 
 * Base implementation of {@link ProductCreator} that breaks the creation into
 * distinct tasks that can be easily implemented or overridden.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * Oct 22, 2018  7483     bsteffen  Handle SPACE mode selection on resources
 *                                  instead of editors.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public abstract class AbstractProductCreator implements ProductCreator {

    @Override
    public AbstractRenderableDisplay loadProduct(IDataCatalog dataCatalog,
            IDataCatalogEntry catalogEntry, DisplayType displayType) {
        AbstractRenderableDisplay display = createNewRenderableDisplay(
                catalogEntry.getDialogSettings(),
                catalogEntry.getSelectedData());
        Collection<ResourcePair> resources = getResourcesToLoad(dataCatalog,
                catalogEntry, displayType);
        handleSpaceTimeSelection(catalogEntry, resources);
        display.getDescriptor().getResourceList().addAll(resources);
        return display;
    }

    protected void handleSpaceTimeSelection(IDataCatalogEntry catalogEntry,
            Collection<ResourcePair> resources) {
        SpaceTimeMenu spaceTimeSelection = catalogEntry.getDialogSettings()
                .getSpaceTimeSelection();
        if (spaceTimeSelection == SpaceTimeMenu.SPACE) {
            /*
             * In space mode a majority of the frames loaded are used for
             * looping through SPACE so hardly any times are visible. Use
             * prognosis loop to allow the user to select a time so they are not
             * limited to only the latest time.
             */
            LoadMode loadMode = LoadMode.PROG_LOOP;
            for (ResourcePair pair : resources) {
                PerspectiveSpecificLoadProperties pProp = pair
                        .getLoadProperties().getPerspectiveProperty();
                D2DLoadProperties dProp = null;
                if (pProp instanceof D2DLoadProperties) {
                    dProp = (D2DLoadProperties) pProp;
                } else {
                    dProp = new D2DLoadProperties();
                    pair.getLoadProperties().setPerspectiveProperty(dProp);
                }
                dProp.setLoadMode(loadMode);
                loadMode = LoadMode.FCST_TIME_MATCH;
            }
        }
    }

    protected Collection<ResourcePair> getResourcesToLoad(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        AbstractRequestableResourceData resourceData = createNewResourceData(
                dataCatalog, catalogEntry, displayType);
        resourceData
                .setMetadataMap(dataCatalog.getProductParameters(catalogEntry));

        ResourcePair pair = new ResourcePair();
        pair.setResourceData(resourceData);
        pair.setLoadProperties(createNewLoadProperties(displayType));
        pair.setProperties(new ResourceProperties());
        return Arrays.asList(pair);
    }

    protected LoadProperties createNewLoadProperties(DisplayType displayType) {
        return new LoadProperties();
    }

    protected abstract AbstractRenderableDisplay createNewRenderableDisplay(
            VolumeBrowserDialogSettings dialogSettings,
            SelectedData selectedData);

    protected abstract AbstractRequestableResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType);

}
