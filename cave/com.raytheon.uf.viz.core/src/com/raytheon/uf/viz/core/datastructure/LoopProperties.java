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
package com.raytheon.uf.viz.core.datastructure;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

/**
 * This class is a container for the loop properties
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	 Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 30, 2007           randerso    Initial Creation.
 * Oct 22, 2013  2491     bsteffen    Remove ISerializableObject
 * Jun 23, 2014  3307     njensen     Fix xml serialization of looping field
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LoopProperties {

    public enum LoopMode {
        Forward, Backward, Cycle
    }

    /** frame time increment in ms */
    public static final int FRAME_STEP = 100;

    /** maximum frame time in ms */
    public static final int MAX_FRAME_TIME = 950;

    /** value at which looping should stop */
    public static final int NOT_LOOPING = 1050;

    /** minimum frame time in ms */
    public static final int MIN_FRAME_TIME = 50;

    /** default frame time */
    public static final int DEFAULT_FRAME_TIME = 250;

    /** dwell time increment in ms */
    public static final int DWELL_STEP = 100;

    /** maximum dwell time in ms */
    public static final int MAX_DWELL_TIME = 2500;

    /** minimum dwell time in ms */
    public static final int MIN_DWELL_TIME = 0;

    /** frame time for forward animation in ms */
    @XmlElement
    private int fwdFrameTime = DEFAULT_FRAME_TIME;

    /** frame time for reverse animation in ms */
    @XmlElement
    private int revFrameTime = NOT_LOOPING;

    /** first frame dwell time in ms */
    @XmlElement
    private int firstFrameDwell = 700;

    /** last frame dwell time in ms */
    @XmlElement
    private int lastFrameDwell = 1500;

    /** flag indicating if currently looping */
    // @XmlElement is on get method instead
    private boolean isLooping = false;

    /** current loop mode */
    @XmlElement
    private LoopMode mode = LoopMode.Forward;

    private long lastDrawnTime = 0;

    private long currentDrawTime = 0;

    public int getFwdFrameTime() {
        return fwdFrameTime;
    }

    public void setFwdFrameTime(int fwdFrameTime) {
        this.fwdFrameTime = fwdFrameTime;
    }

    public int getRevFrameTime() {
        return revFrameTime;
    }

    public void setRevFrameTime(int revFrameTime) {
        this.revFrameTime = revFrameTime;
    }

    public int getFirstFrameDwell() {
        return firstFrameDwell;
    }

    public void setFirstFrameDwell(int firstFrameDwell) {
        this.firstFrameDwell = firstFrameDwell;
    }

    public int getLastFrameDwell() {
        return lastFrameDwell;
    }

    public void setLastFrameDwell(int lastFrameDwell) {
        this.lastFrameDwell = lastFrameDwell;
    }

    public LoopMode getMode() {
        return mode;
    }

    public void setMode(LoopMode mode) {
        this.mode = mode;
    }

    @XmlElement
    public boolean isLooping() {
        return isLooping;
    }

    public void setLooping(boolean isLooping) {
        this.isLooping = isLooping;

        if (PlatformUI.isWorkbenchRunning()) {
            ICommandService service = (ICommandService) PlatformUI
                    .getWorkbench().getService(ICommandService.class);

            service.refreshElements("com.raytheon.viz.ui.tools.looping.loop",
                    null);
        }
    }

    @Override
    public String toString() {
        return "LoopProperties{" + "\n   fwdFrameTime=" + fwdFrameTime
                + "\n   revFrameTime=" + revFrameTime + "\n   firstFrameDwell="
                + firstFrameDwell + "\n   lastFrameDwell=" + lastFrameDwell
                + "\n   mode=" + mode + "\n   isLooping=" + isLooping + "\n}";
    }

    /**
     * @return the shouldDraw
     */
    public boolean isShouldDraw() {
        return isLooping && lastDrawnTime == currentDrawTime;
    }

    /**
     * Sets the current time to be used when determining the amount of time we
     * have waited. This should be set once per paint so that multiple
     * descriptors will all loop at the same time
     */
    public void setCurrentDrawTime(long currentDrawTime) {
        this.currentDrawTime = currentDrawTime;
    }

    /**
     * If waitTime has elapsed since the last draw then isShouldDraw will become
     * true.
     * 
     * @param waitTime
     */
    public void drawAfterWait(long waitTime) {
        if (currentDrawTime - lastDrawnTime > waitTime) {
            lastDrawnTime = currentDrawTime;
        }
    }
}
