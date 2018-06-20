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
package com.raytheon.uf.viz.image.export.options;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.imageio.ImageIO;

import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;

/**
 * Configuration options that control how an image is exported.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 20, 2014  2312     bsteffen    Initial creation
 * Mar 10, 2014  2867     bsteffen    Better frame range validation.
 * Oct 28, 2014  3767     bsteffen    Change default name to screenCapture.png
 * Dec 04, 2014  DR16713  jgerth      Support for date/time selection
 * Jul 07, 2015  4607     bsteffen    Add georeferenced option.
 * Jan 18, 2016  ----     mjames@ucar Save images to /awips2/export/<username> rather than
 *                                    /awips2/eclipse (and avoid guessing that /home/awips exists)
 * Apr 04, 2016  ----     mjames@ucar Reconfig Animate/Current button, add 
 *                                    timestamp to all image filenames.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ImageExportOptions {

    public static enum DateTimeSelection {
        DATETIME, SEQUENTIAL;
    }

    public static enum FrameSelection {
        ALL, CURRENT, USER;
    }

    public static enum ImageFormat {

        SEQUENCE("All Images") {
            @Override
            public String[] getExtensions() {
                return ImageIO.getWriterFileSuffixes();
            }
        },
        ANIMATION("Animated Images(*.gif)") {
            @Override
            public String[] getExtensions() {
                return new String[] { "gif" };
            }
        };

        private final String description;

        private ImageFormat(String description) {
            this.description = description;
        }

        public String getDescription() {
            return description;
        }

        public abstract String[] getExtensions();

    }

    String timeStamp = new SimpleDateFormat("yyyy.MM.dd.HH.mm.ss").format(new Date()); 

    private File fileLocation = new File("/awips2/export/" + System.getProperty("user.name") + "/screenCapture-" + timeStamp +  ".png");

    private ImageFormat imageFormat = ImageFormat.SEQUENCE;

    private FrameSelection frameSelection = FrameSelection.CURRENT;

    private DateTimeSelection dateTimeSelection = DateTimeSelection.SEQUENTIAL;

    private int firstFrameIndex = 0;

    private int lastFrameIndex = 0;

    private int minFrameIndex = 0;

    private int maxFrameIndex = 0;

    /** first frame dwell time in ms */
    private int firstFrameDwell = 700;

    /** last frame dwell time in ms */
    private int lastFrameDwell = 1500;

    /** frame time for animation in ms */
    private int frameDelay = LoopProperties.DEFAULT_FRAME_TIME;

    private boolean georeferencable = false;

    private boolean georeference = false;

    public File getFileLocation() {
        return fileLocation;
    }

    public void setFileLocation(File fileLocation) {
        this.fileLocation = fileLocation;
    }

    public ImageFormat getImageFormat() {
        return imageFormat;
    }

    public void setImageFormat(ImageFormat imageFormat) {
        this.imageFormat = imageFormat;
    }

    public DateTimeSelection getDateTimeSelection() {
        return dateTimeSelection;
    }

    public void setDateTimeSelection(DateTimeSelection dts) {
        this.dateTimeSelection = dts;
    }

    public FrameSelection getFrameSelection() {
        return frameSelection;
    }

    public void setFrameSelection(FrameSelection frameSelection) {
        this.frameSelection = frameSelection;
    }

    public int getFirstFrameIndex() {
        return firstFrameIndex;
    }

    public void setFirstFrameIndex(int firstFrameIndex) {
        this.firstFrameIndex = firstFrameIndex;
    }

    public int getLastFrameIndex() {
        return lastFrameIndex;
    }

    public void setLastFrameIndex(int lastFrameIndex) {
        this.lastFrameIndex = lastFrameIndex;
    }

    public int getFirstFrameDwell() {
        return firstFrameDwell;
    }

    public void setFirstFrameDwell(int firstFrameDwell) {
        this.firstFrameDwell = firstFrameDwell;
    }

    public int getMinFrameIndex() {
        return minFrameIndex;
    }

    public void setMinFrameIndex(int minFrameIndex) {
        this.minFrameIndex = minFrameIndex;
    }

    public int getMaxFrameIndex() {
        return maxFrameIndex;
    }

    public void setMaxFrameIndex(int maxFrameIndex) {
        this.maxFrameIndex = maxFrameIndex;
    }

    public int getLastFrameDwell() {
        return lastFrameDwell;
    }

    public void setLastFrameDwell(int lastFrameDwell) {
        this.lastFrameDwell = lastFrameDwell;
    }

    public int getFrameDelay() {
        return frameDelay;
    }

    public void setFrameDelay(int frameDelay) {
        this.frameDelay = frameDelay;
    }

    public boolean isGeoreferencable() {
        return georeferencable;
    }

    public void setGeoreferencable(boolean georeferencable) {
        this.georeferencable = georeferencable;
    }

    public boolean isGeoreference() {
        return georeference;
    }

    public void setGeoreference(boolean georeference) {
        this.georeference = georeference;
    }

    public void populate(IDisplayPaneContainer container) {
        populate(container.getLoopProperties());
        IDescriptor descriptor = container.getActiveDisplayPane()
                .getDescriptor();
        int frameCount = descriptor
                .getFramesInfo().getFrameCount();
        lastFrameIndex = Math.max(frameCount - 1, 0);
        maxFrameIndex = lastFrameIndex;
        CoordinateReferenceSystem crs = descriptor.getCRS();
        georeferencable = (crs instanceof GeographicCRS || crs instanceof ProjectedCRS);
        georeferencable = georeferencable
                & container.getDisplayPanes().length == 1;
    }

    public void populate(LoopProperties loopProperties) {
        this.firstFrameDwell = loopProperties.getFirstFrameDwell();
        this.lastFrameDwell = loopProperties.getLastFrameDwell();
        this.frameDelay = loopProperties.getFwdFrameTime();
    }
}
