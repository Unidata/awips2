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
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesRenderableDisplay;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData.AxisParameter;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * 
 * Creates a {@link TimeSeriesRenderableDisplay} containing a
 * {@link TimeSeriesResourceData}.
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
public class TimeSeriesProductCreator extends AbstractProductCreator{

    @Override
    protected TimeSeriesRenderableDisplay createNewRenderableDisplay(VolumeBrowserDialogSettings dialogSettings, SelectedData selectedData) {
        return new TimeSeriesRenderableDisplay();
    }

    @Override
    protected TimeSeriesResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        SelectedData selectedData = catalogEntry.getSelectedData();
        TimeSeriesResourceData resourceData = new TimeSeriesResourceData();
        resourceData.setCoordinate(PointLineUtil.getPointCoordinate(catalogEntry));
        resourceData.setPointLetter(PointLineUtil.getPointLetter(catalogEntry));
        resourceData.setSource(selectedData.getSourcesText());

        AxisParameter yParameter = new AxisParameter();
        yParameter.code = selectedData.getFieldsKey();
        yParameter.name = selectedData.getFieldsText();

        resourceData.setYParameter(yParameter);
        resourceData.setLevelKey(selectedData.getPlanesKey());
        return resourceData;
    }

}
