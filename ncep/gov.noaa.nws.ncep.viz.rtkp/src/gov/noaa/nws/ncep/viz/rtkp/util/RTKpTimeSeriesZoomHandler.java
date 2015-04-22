/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.util;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.xy.timeseries.util.TimeSeriesZoomHandler;

/**
 * This class has been extended from raytheon's TimeSeriesZoomHandler class in
 * order to override the handleMouseWheel() behavior so that zooming is disabled
 * for RTKp graphs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/02/14     #4078       Shova Gurung Initial Creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RTKpTimeSeriesZoomHandler extends TimeSeriesZoomHandler {

    public RTKpTimeSeriesZoomHandler(IRenderableDisplay display) {
        super(display);
    }

    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
        return false;
    }

}
