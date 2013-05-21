package gov.noaa.nws.ncep.viz.common.display;

import java.text.SimpleDateFormat;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       Ticket#	   Engineer	    Description
 * ----------------------------------------------------------
 * 01/21/12     #972       Greg Hull    Created from NCMapDescriptor
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public interface INatlCntrsDescriptor extends IDescriptor {

    public static SimpleDateFormat FRAME_DATE_FORMAT = new SimpleDateFormat("EEE yyMMdd/HHmm");

    public abstract Boolean isAutoUpdate();

    public abstract void setAutoUpdate(Boolean autoUpdate);

    public abstract Boolean getSuspendZoom();

    public abstract void setSuspendZoom(Boolean suspendZoom);


    public abstract void setFrameTimesForResource(AbstractVizResource<?, ?> rsc,
            DataTime[] frameTimes);

    public abstract void updateDataTimes(DataTime[] dataTimes);

    public abstract String getValidTime(int currentIndex);
}