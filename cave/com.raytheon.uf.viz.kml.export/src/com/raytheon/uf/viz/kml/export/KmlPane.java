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
package com.raytheon.uf.viz.kml.export;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;

/**
 * VizDisplayPane but for KML!
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlPane {

    private List<ResourcePair> resourcesToExport;

    private AbstractRenderableDisplay display;

    private Rectangle bounds;

    private KmlGraphicsTarget target;

    private DataTime displayedTime;

    public KmlPane(AbstractRenderableDisplay display, Rectangle bounds) {
        this.display = display;
        this.bounds = bounds;
    }

    public List<ResourcePair> getResources(boolean includeMaps,
            boolean includeHidden) {
        List<ResourcePair> rscList = new ArrayList<ResourcePair>();
        for (ResourcePair rp : display.getDescriptor().getResourceList()) {
            if (!rp.getResourceData().equals(rp.getResourceData())) {
                // A special check for those special resources which will never
                // work with KML because they don't properly implement equals.
                // ... like GFE
                continue;
            } else if (rp.getProperties().isSystemResource()) {
                continue;
            } else if (!includeMaps && rp.getProperties().isMapLayer()) {
                continue;
            } else if (!includeHidden && !rp.getProperties().isVisible()) {
                continue;
            }
            rscList.add(rp);
        }
        return rscList;
    }

    public void setDisplay(AbstractRenderableDisplay display) {
        this.display = display;
    }

    public void setTarget(KmlGraphicsTarget target) {
        this.target = target;
    }

    public List<ResourcePair> getResourcesToExport() {
        return resourcesToExport;
    }

    public void setResourcesToExport(List<ResourcePair> resourcesToExport) {
        this.resourcesToExport = resourcesToExport;
    }

    public AbstractRenderableDisplay getDisplay() {
        return display;
    }

    public Rectangle getBounds() {
        return bounds;
    }

    public KmlGraphicsTarget getTarget() {
        return target;
    }

    public DataTime getDisplayedTime() {
        return displayedTime;
    }

    public void setDisplayedTime(DataTime displayedTime) {
        this.displayedTime = displayedTime;
    }

}
