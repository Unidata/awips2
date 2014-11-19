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
package gov.noaa.gsd.viz.ensemble.display.rsc.histogram;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Ensemble default input handler for sampling. Is a modification of the
 * SamplingInputAdapter.
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July, 2014      5056     jing       Initial creation
 * </pre>
 * @param <T>
 */
public class EnsSamplingInputAdapter<T extends EnsSamplingResource> extends
        InputAdapter {

    private T resource;

    public EnsSamplingInputAdapter(T resource) {
        this.resource = resource;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        IDisplayPaneContainer container = resource.getResourceContainer();
        Coordinate c = container.translateClick(x, y);
        if (c != null) {
            resource.sampleCoord = new ReferencedCoordinate(c);
        } else {
            resource.sampleCoord = null;
        }
        if (resource.isSampling()) {
            resource.issueRefresh();
        }
        return false;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        return handleMouseMove(x, y);
    }

    public boolean handleMouseExit(Event event) {
        resource.sampleCoord = null;
        if (resource.isSampling()) {
            resource.issueRefresh();
        }
        return false;
    }

    public boolean handleMouseEnter(Event event) {
        return handleMouseMove(event.x, event.y);
    }
}
