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
package com.raytheon.uf.viz.d2d.nsharp.vb;

import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.d2d.nsharp.rsc.BufruaNSharpResourceData;
import com.raytheon.uf.viz.d2d.nsharp.rsc.D2DNSharpResourceData;
import com.raytheon.uf.viz.d2d.nsharp.rsc.GoesSndNSharpResourceData;
import com.raytheon.uf.viz.d2d.nsharp.rsc.GribNSharpResourceData;
import com.raytheon.uf.viz.d2d.nsharp.rsc.MdlSndNSharpResourceData;
import com.raytheon.uf.viz.d2d.nsharp.rsc.PoesSndNSharpResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.AbstractProductCreator;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDisplay;

/**
 * 
 * Create various {@link D2DNSharpResourceData}s for using nsharp with the
 * volume browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer       Description
 * ------------- -------- -------------- ---------------------------------------
 * Aug 03, 2015  3861     bsteffen       Initial Creation
 * Mar 01, 2016  14647    mgamazayhikov  Added argument to
 *                                       GribNSharpResourceData constructor.
 * Aug 27, 2018  7409     bsteffen       Set point info for modelsounding.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class NSharpProductCreator extends AbstractProductCreator {

    @Override
    protected D2DNSharpResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        String sourceKey = catalogEntry.getSelectedData().getSourcesKey();
        /*
         * Some point types don't need the point info because the station info
         * will be used instead.
         */
        boolean needsPointInfo = true;
        D2DNSharpResourceData result = null;
        switch (sourceKey) {
        case "bufrua":
            result = new BufruaNSharpResourceData();
            needsPointInfo = false;
            break;
        case "modelsoundingETA":
            result = new MdlSndNSharpResourceData("NAMSND");
            break;
        case "modelsoundingGFS":
            result = new MdlSndNSharpResourceData("GFSSND");
            break;
        case "poessounding":
            result = new PoesSndNSharpResourceData();
            needsPointInfo = false;
            break;
        case "goessounding":
            result = new GoesSndNSharpResourceData();
            needsPointInfo = false;
            break;
        default:
            result = new GribNSharpResourceData(
                    catalogEntry.getSelectedData().getSourcesKey(),
                    catalogEntry.getSelectedData().getSourcesText());
        }
        if (needsPointInfo) {
            result.setCoordinate(
                    PointLineUtil.getPointCoordinate(catalogEntry));
            String pointName = catalogEntry.getSelectedData().getPlanesKey();
            result.setPointName(pointName);
        }
        return result;
    }

    @Override
    protected AbstractRenderableDisplay createNewRenderableDisplay(
            VolumeBrowserDialogSettings dialogSettings,
            SelectedData selectedData) {
        NsharpSkewTPaneDisplay display = new NsharpSkewTPaneDisplay();
        display.setDescriptor(new NsharpSkewTPaneDescriptor());
        return display;
    }

}
