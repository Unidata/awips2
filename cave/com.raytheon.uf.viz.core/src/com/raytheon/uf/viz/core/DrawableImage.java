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
package com.raytheon.uf.viz.core;

import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.drawables.IImage;

/**
 * Drawable Image, contains coverage and IImage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawableImage {

    private IImage image;

    private PixelCoverage coverage;

    private RasterMode mode = RasterMode.SYNCHRONOUS;

    public DrawableImage(IImage image, PixelCoverage coverage) {
        this.image = image;
        this.coverage = coverage;
    }

    public IImage getImage() {
        return image;
    }

    public PixelCoverage getCoverage() {
        return coverage;
    }

    public RasterMode getMode() {
        return mode;
    }

    public void setMode(RasterMode mode) {
        this.mode = mode;
    }

    public void setCoverage(PixelCoverage coverage) {
        this.coverage = coverage;
    }

    public void dispose() {
        image.dispose();
        if (coverage != null) {
            coverage.dispose();
        }
    }
}
