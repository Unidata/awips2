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
package com.raytheon.uf.viz.gisdatastore.actions;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource;
import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource.IRubberBandSelectionListener;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Crop GIS resource action
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2012      #1326 randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class CropAction extends AbstractRightClickAction {
    public CropAction() {
        super(AS_PUSH_BUTTON);
    }

    @Override
    public String getText() {
        if (this.getSelectedRsc() instanceof DataStoreResource) {
            final DataStoreResource rsc = (DataStoreResource) this
                    .getSelectedRsc();
            if (rsc.isCropped()) {
                return "Uncrop Shape";
            }
        }
        return "Crop Shape";
    }

    @Override
    public void run() {
        if (this.getSelectedRsc() instanceof DataStoreResource) {
            final DataStoreResource rsc = (DataStoreResource) this
                    .getSelectedRsc();
            if (rsc.isCropped()) {
                rsc.uncrop();
            } else {
                rsc.addRubberBandSelectionListener(new IRubberBandSelectionListener() {
                    @Override
                    public void rubberBandSelectionChanged(PixelExtent extent) {
                        rsc.removeRubberBandSelectionListener(this);
                        rsc.crop(extent);
                    }
                });
                rsc.activateRubberBandBox();
            }
        }
    }

}
