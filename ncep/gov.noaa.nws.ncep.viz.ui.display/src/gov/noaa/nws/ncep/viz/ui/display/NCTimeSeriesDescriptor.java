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

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;

/**
 * Time Series descriptor, needed so loading bundles know what editor to load
 * with this descriptor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/21/2014   #1136      qzhou       Initial creation
 * 05/21/2014   #1136      qzhou       Added override getCurrentFrame, getFrameCount
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "NCTimeSeriesDescriptor")
@XmlRootElement
public class NCTimeSeriesDescriptor extends XyGraphDescriptor implements
        INatlCntrsDescriptor {

    @XmlElement
    private Boolean autoUpdate = false;

    @XmlElement
    private Boolean suspendZoom = false;

    public NCTimeSeriesDescriptor() {
        super();
    }

    public NCTimeSeriesDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new NCTimeSeriesGraph(this);
    }

    @Override
    public Boolean isAutoUpdate() {
        return autoUpdate;
    }

    @Override
    public void setAutoUpdate(Boolean autoUpdate) {
        this.autoUpdate = autoUpdate;
        if (getTimeMatcher() != null) {
            // getTimeMatcher().setAutoUpdate( autoUpdate );
        }
    }

    @Override
    public Boolean getSuspendZoom() {
        return suspendZoom;
    }

    @Override
    public void setSuspendZoom(Boolean suspendZoom) {
        this.suspendZoom = suspendZoom;
    }

    // would rather have add the descriptor to the timeMatcher here but this
    // creates a cyclical dependency between display and resources.
    @Override
    public void setTimeMatcher(AbstractTimeMatcher timeMatcher) {
        // if( this.timeMatcher != null ) {
        // ((NCTimeMatcher)this.timeMatcher).removeDescriptor( this );
        // }
        super.setTimeMatcher(timeMatcher);
        // ((AbstractDescriptor) descriptor).getTimeMatchingMap().put(
        // this, frameTimes.toArray( new DataTime[0] ) );

        // timeMatcher.addDescriptor();
    }

    @Override
    public void synchronizeTimeMatching(IDescriptor other) {

    }

    @Override
    public void setFrameTimesForResource(AbstractVizResource<?, ?> rsc,
            DataTime[] frameTimes) {
        timeMatchingMap.put(rsc, frameTimes);
    }

    @Override
    public int getCurrentFrame() {

        return 0;
    }

    @Override
    public int getFrameCount() {

        return 1;
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
    @Override
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
    @Override
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
