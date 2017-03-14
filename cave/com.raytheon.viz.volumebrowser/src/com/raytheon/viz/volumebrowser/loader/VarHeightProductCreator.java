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

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightDescriptor;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightRenderableDisplay;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * 
 * Creates a {@link VarHeightRenderableDisplay} containing a
 * {@link VarHeightDescriptor} with a {@link VarHeightResourceData}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class VarHeightProductCreator extends AbstractProductCreator {

    @Override
    protected VarHeightRenderableDisplay createNewRenderableDisplay(VolumeBrowserDialogSettings dialogSettings, SelectedData selectedData) {
        VarHeightRenderableDisplay display = new VarHeightRenderableDisplay();
        VarHeightDescriptor descriptor = display.getDescriptor();
        descriptor.setRenderableDisplay(display);
        descriptor.setHeightScale(dialogSettings.getHeightScaleSelection());
        return display;
    }

    @Override
    protected VarHeightResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        VarHeightResourceData resourceDaata = new VarHeightResourceData();
        resourceDaata.setPoint(PointLineUtil.getPointCoordinate(catalogEntry));
        resourceDaata.setParameter(catalogEntry.getSelectedData().getFieldsKey());
        resourceDaata.setParameterName(catalogEntry.getSelectedData().getFieldsText());
        resourceDaata.setPointLetter(PointLineUtil.getPointLetter(catalogEntry));
        resourceDaata.setSource(catalogEntry.getSelectedData().getSourcesText());
        return resourceDaata;
    }

}
