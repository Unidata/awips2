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
package com.raytheon.uf.viz.xy.map;

import java.io.File;

import org.eclipse.swt.layout.FormData;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;

/**
 * Interface renderable displays who contain an inset map should implement
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 08, 2009           mschenke    Initial creation
 * Oct 22, 2013  2491     bsteffen    Unmarshal with Bundle.unmarshalBundle.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IInsetMapContainer {

    /**
     * Return the location of where the inset map should be placed using an swt
     * FormData object
     * 
     * @return the location
     */
    public FormData getInsetMapLocation();

    /** Utility class for loading the inset map from a bundle */
    public static class InsetMapUtil {

        private static final transient IUFStatusHandler statusHandler = UFStatus
                .getHandler(IInsetMapContainer.class);

        public static IRenderableDisplay loadInsetMap(
                IRenderableDisplay parentDisplay) {
            File bundle = PathManagerFactory.getPathManager().getStaticFile(
                    "insetmap" + File.separator + "inset.xml");
            try {
                Bundle b = Bundle.unmarshalBundle(bundle);
                InsetMapRenderableDisplay display = (InsetMapRenderableDisplay) b
                        .getDisplays()[0];
                display.getDescriptor().getResourceList()
                        .instantiateResources(display.getDescriptor(), true);
                display.setExtent(new PixelExtent(0, 1000, 0, 1000));
                display.setParentDisplay(parentDisplay);
                return display;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            return null;
        }
    }
}
