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
        frameCoordinator = new FrameCoordinator(this) {

            @Override
            public void changeFrame(LoopProperties loopProperties) {
                super.changeFrame(loopProperties);
                // Copied code NsharpSkewTDescriptor, this entire class should
                // go away once NsharpSkewTDescriptor is fixed
                if (loopProperties == null || getFrames() == null) {
                    // System.out.println("NsharpSkewTDescriptor checkDrawTime called but jump ");
                    return;
                }

                if (loopProperties.isLooping() && loopProperties.isShouldDraw()) {
                    NsharpSkewTResource skewRsc = getSkewtResource();

                    skewRsc.setLoopingDataTimeLine(loopProperties);
                    // System.out.println("NsharpSkewTDescriptor handleDataTimeIndex handled!!!!!! ");
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
                D2DNSharpDescriptor.this.changeFrame(dop, dmode);
            }
        };
    }
}
