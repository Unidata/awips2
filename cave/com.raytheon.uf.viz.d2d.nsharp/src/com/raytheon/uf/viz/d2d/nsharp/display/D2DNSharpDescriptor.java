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
package com.raytheon.uf.viz.d2d.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource.ElementStateProperty;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.FrameCoordinator;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;

/**
 * 
 * A serializeable descriptor for nsharp
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class D2DNSharpDescriptor extends NsharpSkewTDescriptor {

    public D2DNSharpDescriptor() {
        super();
        setTimeMatcher(new D2DTimeMatcher());
        // NSharp does not handle frame changes in the descriptor so it needs a
        // custom frame coordinator to handle the events using the frame
        // information in the resource.
        frameCoordinator = new FrameCoordinator(this) {

            @Override
            public void changeFrame(LoopProperties loopProperties) {
                if (loopProperties == null || !loopProperties.isLooping()) {
                    return;
                }
                NsharpSkewTResource skewRsc = getSkewtResource();
                List<ElementStateProperty> frames = skewRsc
                        .getDataTimelineList();
                if (frames == null || frames.isEmpty()) {
                    return;
                }
                // Determine wait time based off of direction
                long waitTime = loopDirection > 0 ? loopProperties
                        .getFwdFrameTime() : loopProperties.getRevFrameTime();
                // use special wait time for first and last frame
                if (frames.get(frames.size() - 1).getElementDescription()
                        .equals(skewRsc.getPickedStnInfoStr())) {
                    waitTime = loopProperties.getLastFrameDwell();
                } else if (frames.get(0).getElementDescription()
                        .equals(skewRsc.getPickedStnInfoStr())) {
                    waitTime = loopProperties.getFirstFrameDwell();
                }
                // Tell the loop propertied how long to wait
                loopProperties.drawAfterWait(waitTime);

                // Tell the resource to handle looping
                if (loopProperties.isShouldDraw()) {
                    skewRsc.setLoopingDataTimeLine(loopProperties);
                }
            }

            @Override
            public void changeFrame(
                    IFrameCoordinator.FrameChangeOperation operation,
                    IFrameCoordinator.FrameChangeMode mode) {
                IDescriptor.FrameChangeOperation dop = IDescriptor.FrameChangeOperation
                        .valueOf(operation.name());
                IDescriptor.FrameChangeMode dmode = IDescriptor.FrameChangeMode
                        .valueOf(mode.name());
                // Just hand this off to the resource.
                getSkewtResource().setSteppingDataTimeLine(dop, dmode);
            }
        };
    }

    @Override
    public void checkDrawTime(LoopProperties loopProperties) {
        // Do nothing, this is a deprecated method that should never get called
        // but for some reason ncep continues writing new code that calls this
        // and it continues to conflict with the frame coordinator object.
    }

}
