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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDisplay;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.core.ID2DRenderableDisplay;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.sun.jna.ptr.FloatByReference;

/**
 * 
 * A Serializeable display for nsharp
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
@XmlRootElement
public class D2DNSharpDisplay extends NsharpSkewTDisplay implements
        ID2DRenderableDisplay {

    private static WeakReference<NsharpSkewTResource> activeNativeResource;

    private double density = 1.0;

    private double magnification = 1.0;

    public D2DNSharpDisplay() {
        super();
        // Super forces the Default NShaprSkewT Descriptor
        setDescriptor(new D2DNSharpDescriptor());
        VizWorkbenchManager.getInstance().getCurrentWindow().getActivePage()
                .addPartListener(D2DNSharpPartListener.getInstance());
    }

    @Override
    public void clear(IDisplayPane parentPane) {
        super.clear(parentPane);
        if (getContainer() instanceof IEditorPart) {
            IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            page.closeEditor((IEditorPart) getContainer(), false);
        }
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        /**
         * This whole if statement exists to manage the data stored in the
         * native library, this is not a good way of handling this, it should be
         * handled by a separate manager or something.T
         */
        NsharpSkewTResource skewRsc = getDescriptor().getSkewtResource();
        if ((activeNativeResource == null || activeNativeResource.get() != skewRsc)
                && skewRsc.getSoundingLys() != null) {
            // TODO we need a better way of tracking what is in native.
            List<NcSoundingLayer> soundingLys = skewRsc.getSoundingLys();
            NsharpNative nsharpNative = skewRsc.getNsharpNative();

            // re-populate snd data to nsharp native code lib for later
            // calculating
            nsharpNative.populateSndgData(soundingLys);

            if (soundingLys != null && soundingLys.size() > 0) {
                // set initial hodohouseC
                FloatByReference dummy1 = new FloatByReference(-999);
                FloatByReference dummy2 = new FloatByReference(-999);
                FloatByReference wdir = new FloatByReference(-999);
                FloatByReference wspd = new FloatByReference(-999);
                FloatByReference Surfpressure = new FloatByReference(-999);
                nsharpNative.nsharpLib
                        .get_surface(Surfpressure, dummy1, dummy2);
                if (nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
                    nsharpNative.nsharpLib.mean_wind(Surfpressure.getValue(),
                            nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib
                                    .msl(6000.0F)), dummy1, dummy2, wdir, wspd);
                    if (nsharpNative.nsharpLib.qc(wdir.getValue()) == 1
                            && nsharpNative.nsharpLib.qc(wspd.getValue()) == 1) {
                        // ----- Plot 30/75 Storm Motion Vector -----small light
                        // pink circle
                        float dir = (wdir.getValue() + 30.0f) % 360;
                        float spd = wspd.getValue() * 0.75f;
                        // System.out.println(spd + " "+ wspd.getValue());
                        nsharpNative.nsharpLib.set_storm(spd, dir);
                    }
                }
            }
            activeNativeResource = new WeakReference<NsharpSkewTResource>(
                    skewRsc);
        }
        super.paint(target, paintProps);
    }

    @Override
    public double getMagnification() {
        return magnification;
    }

    @Override
    public double getDensity() {
        return density;
    }

    @Override
    public void setMagnification(double magnification) {
        this.magnification = magnification;
    }

    @Override
    public void setDensity(double density) {
        this.density = density;
    }

    @Override
    public String getScale() {
        return "NSharp";
    }

    @Override
    public void setScale(String scale) {
        if (!getScale().equals(scale)) {
            throw new UnsupportedOperationException(
                    "NSharp display does not allow you to change scale to: "
                            + scale);
        }
    }

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        globals.put(VizConstants.FRAMES_ID, new Integer(getDescriptor()
                .getNumberOfFrames()));
        globals.put(VizConstants.DENSITY_ID, new Double(density));
        globals.put(VizConstants.MAGNIFICATION_ID, new Double(magnification));
        globals.put(VizConstants.LOADMODE_ID, ((D2DTimeMatcher) getDescriptor()
                .getTimeMatcher()).getLoadMode());
        return globals;
    }

}
