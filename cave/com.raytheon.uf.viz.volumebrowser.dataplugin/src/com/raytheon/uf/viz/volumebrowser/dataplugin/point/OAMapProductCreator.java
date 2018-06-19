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
package com.raytheon.uf.viz.volumebrowser.dataplugin.point;

import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.objectiveanalysis.rsc.OAResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.AbstractMapProductCreator;
import com.raytheon.viz.volumebrowser.loader.ProductCreator;

/**
 * 
 * {@link ProductCreator} for loading objective analysis data on a
 * {@link MapRenderableDisplay}.
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
public class OAMapProductCreator extends AbstractMapProductCreator {

    @Override
    protected OAResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        String sourceText = catalogEntry.getSelectedData().getSourcesText();

        OAResourceData rscData = new OAResourceData();
        if (sourceText.equals("RaobOA")) {
            BinOffset binOffset = new BinOffset(3600, 3600);
            rscData.setBinOffset(binOffset);
        } else if (sourceText.equals("MetarOA")) {
            BinOffset binOffset = new BinOffset(1800, 1800);
            rscData.setBinOffset(binOffset);
        }
        rscData.setParameter(catalogEntry.getSelectedData().getFieldsKey());
        rscData.setParameterName(catalogEntry.getSelectedData().getFieldsText());
        rscData.setSource(sourceText);
        String levelKey = catalogEntry.getSelectedData().getPlanesKey();
        rscData.setLevelKey(levelKey);
        rscData.setRetrieveData(false);
        rscData.setUpdatingOnMetadataOnly(true);
        return rscData;
    }

}
