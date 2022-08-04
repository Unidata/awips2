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

import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor.TimeDirection;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightRenderableDisplay;
import com.raytheon.uf.viz.xy.timeheight.rsc.TimeHeightResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.LeftRightMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * 
 * Creates a {@link TimeHeightRenderableDisplay} containing a
 * {@link TimeHeightResourceData}.
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
public class TimeHeightProductCreator extends AbstractProductCreator {

    @Override
    protected TimeHeightRenderableDisplay createNewRenderableDisplay(
            VolumeBrowserDialogSettings dialogSettings,
            SelectedData selectedData) {
        TimeHeightRenderableDisplay display = new TimeHeightRenderableDisplay();
        TimeHeightDescriptor descriptor = display.getDescriptor();
        descriptor.setRenderableDisplay(display);
        descriptor.setHeightScale(dialogSettings.getHeightScaleSelection());
        LeftRightMenu leftRightMenu = dialogSettings
                .getTimeDirectionSelection();
        if (leftRightMenu == LeftRightMenu.LEFT) {
            descriptor.setTimeDirection(TimeDirection.RIGHT_TO_LEFT);
        } else {
            descriptor.setTimeDirection(TimeDirection.LEFT_TO_RIGHT);
        }
        return display;
    }

    @Override
    protected TimeHeightResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        SelectedData selectedData = catalogEntry.getSelectedData();
        TimeHeightResourceData resourceData = new TimeHeightResourceData();
        resourceData.setPoint(PointLineUtil.getPointCoordinate(catalogEntry));
        resourceData.setParameter(selectedData.getFieldsKey());
        resourceData.setParameterName(selectedData.getFieldsText());
        resourceData.setPointLetter(PointLineUtil.getPointLetter(catalogEntry));
        resourceData.setSource(selectedData.getSourcesText());
        return resourceData;
    }

    @Override
    protected LoadProperties createNewLoadProperties(DisplayType displayType) {
        return new GridLoadProperties(displayType);
    }

}
