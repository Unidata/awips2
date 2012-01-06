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
package com.raytheon.viz.skewt.ui;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public abstract class AbstractSkewTBackground implements IRenderable {
    protected Rectangle rectangle;

    private WGraphics world;

    protected IFont smallFont;

    protected double magnification;

    /**
     * Determine if a point is contained in this background area
     * 
     * @param c
     *            point of interest
     * @return true if point is contained
     */
    public boolean contains(Coordinate c) {
        return this.rectangle.contains((int) c.x, (int) c.y);
    }

    public Rectangle getRectangle() {
        return this.rectangle;
    }

    public WGraphics getWorld() {
        synchronized (AbstractSkewTBackground.class) {
            if (this.world == null) {
                this.world = computeWorld();
            }
        }
        return this.world;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(double magnification) {
        this.magnification = magnification;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        smallFont = target.initializeFont(
                target.getDefaultFont().getFontName(),
                (float) (10 * magnification), null);

        paintInternal(target, paintProps);

        smallFont.dispose();
    }

    protected abstract void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException;

    protected abstract WGraphics computeWorld();
}
