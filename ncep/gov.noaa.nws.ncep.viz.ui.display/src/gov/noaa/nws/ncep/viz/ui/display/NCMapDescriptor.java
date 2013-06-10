/* NCMapDescriptor
 * 
 * Date Created (19 March 2010)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 
 */
package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;

import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * MapDescriptor for National Centers
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       Ticket#	   Engineer	    Description
 * ----------------------------------------------------------
 * 9-Mar-2010   238, 239    Archana     Initial Creation
 * 8-Apr-2010   239         Archana     Removed the redundant conversion
 *                                      of the frame's valid time to GMT. 
 *                                      Updated the Javadoc to reflect the missing ':' 
 *                                      character in the displayed time.
 * 20-May-2010  239         Archana     Updated the method getFrameDateFormat() to
 *                                      set the timezone to GMT.     
 * 01-Sep-2010  307         Greg Hull   add autoUpdate       
 * 22-Oct-2010  307         Greg Hull   updateDataTimes()                                                                   
 * 20-Jan-2011              Chin Chen   Fixed a race condition bug in handleDataTimeIndex()
 * 07/15/11                    C Chen   fixed frame number not updated while looping problem
 * 11/11/11                 Greg Hull   rm frameChangeListener in place of Raytheon's, synchronize the listener
 * 01/21/12    #972         Greg Hull   implement new INatlCntrsDescriptor
 * 
 * </pre>
 * 
 * @author Archana
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NCMapDescriptor")
@XmlRootElement
public class NCMapDescriptor extends MapDescriptor implements INatlCntrsDescriptor {

    @XmlElement
    private Boolean autoUpdate = false;

    @XmlElement
    private Boolean suspendZoom = false;

    public Boolean isAutoUpdate() {
        return autoUpdate;
    }

    public void setAutoUpdate(Boolean autoUpdate) {
        this.autoUpdate = autoUpdate;
        if (getTimeMatcher() != null) {
            // getTimeMatcher().setAutoUpdate( autoUpdate );
        }
    }

    public Boolean getSuspendZoom() {
        return suspendZoom;
    }

    public void setSuspendZoom(Boolean suspendZoom) {
        this.suspendZoom = suspendZoom;
    }

    public NCMapDescriptor() throws VizException {
        super();
    }

    public NCMapDescriptor(CoordinateReferenceSystem crs, Coordinate ll,
            Coordinate ur) throws VizException {
        super(crs, ll, ur);
    }

    // would rather have add the descriptor to the timeMatcher here but this
    // creates a cyclical dependency between display and resources.
    @Override
    public void setTimeMatcher(AbstractTimeMatcher timeMatcher) {
        // if( this.timeMatcher != null ) {
        // ((NCTimeMatcher)this.timeMatcher).removeDescriptor( this );
        // }
        //
        super.setTimeMatcher(timeMatcher);

        // ((AbstractDescriptor) descriptor).getTimeMatchingMap().put(
        // this, frameTimes.toArray( new DataTime[0] ) );

        // timeMatcher.addDescriptor();
    }

    @Override
    public void synchronizeTimeMatching(IDescriptor other) {

    }

    public void setFrameTimesForResource(AbstractVizResource<?, ?> rsc,
            DataTime[] frameTimes) {
        timeMatchingMap.put(rsc, frameTimes);
    }

    // notify all of the resources in the resourceList that the timeline has
    // changed so
    // that they can update there frameDataMap.
    //
    // TODO : currently we can't reference classes in the resources project
    // since this
    // causes a cyclical dependency. This works around the problem by calling
    // the update()
    // method which is defined in AbstractResourceData (not a gov class) but a
    // better solution
    // would involve resolving the cyclical dependency issue and creating a
    // method in
    // AbstractNatlCntrsRequestableResourceData.
    //
    public void updateDataTimes(DataTime[] dataTimes) {

        setDataTimes(dataTimes); // this also resets bad frames.

        try {
            //
            for (ResourcePair rp : resourceList) {
                if (rp.getResourceData() instanceof AbstractRequestableResourceData) {

                    if (rp.getResource() != null) {
                        timeMatchingMap.put(rp.getResource(), dataTimes);
                    }

                    // HACK ALERT : currently there is a cyclical dependency bug
                    // that prevents the display project from referencing the
                    // resources project. So we will use java reflection to get
                    // around this
                    //
                    Method[] mthds = rp.getResource().getClass().getMethods();

                    for (Method m : mthds) {
                        // System.out.println( m.getName() );
                        if (m.getName().equals("updateTimeline")) {
                            if (m.getReturnType() == Boolean.class
                                    && m.getParameterTypes().length == 0) {
                                m.invoke(rp.getResource());
                                break;
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

    /***
     * Provides the time of the frame, given the index of the frame in the
     * DataTime array
     * 
     * <pre>
     * The time of the frame is displayed in the format: 
     * Day yyMMdd/HHmm
     * Example: Fri 100402/0505
     * The time displayed is in GMT
     * </pre>
     * 
     * @param currentIndex
     *            - The index of the frame whose time needs to be determined
     * @return The valid time for the frame
     */
    public String getValidTime(int currentIndex) {
        Calendar cal;
        String strTimeFrame = "";
        DataTime[] frameTimeArray = super.getFrames();

        if (frameTimeArray != null) {

            cal = frameTimeArray[currentIndex].getValidTime();
            strTimeFrame = new String(getFrameDateFormat()
                    .format(cal.getTime()));

        }

        return strTimeFrame.toUpperCase();
    }

    public static SimpleDateFormat getFrameDateFormat() {
        FRAME_DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
        return FRAME_DATE_FORMAT;
    }

    // only 1 instance
    @Override
    public void addFrameChangedListener(IFrameChangedListener listener) {
        synchronized (listener) {
            if (!listeners.contains(listener)) {
                super.addFrameChangedListener(listener);
            }
        }
    }
}
