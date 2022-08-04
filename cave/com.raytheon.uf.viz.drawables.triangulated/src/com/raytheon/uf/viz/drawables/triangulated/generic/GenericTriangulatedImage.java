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
package com.raytheon.uf.viz.drawables.triangulated.generic;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.drawables.triangulated.ITriangulatedImage;

/**
 * Generic implementation of {@link ITriangulatedImage} that uses a
 * {@link TriangleFlattener} to convert the triangles into a grid for use with
 * an {@link IColormappedImage}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 18, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GenericTriangulatedImage implements ITriangulatedImage {

    private final TriangleFlattener flattener;

    private final IColormappedImage delegate;

    public GenericTriangulatedImage(IColormappedImage image,
            TriangleFlattener flattener) {
        this.flattener = flattener;
        this.delegate = image;
        this.delegate.setInterpolated(true);
    }

    @Override
    public void setBrightness(float brightness) {
            delegate.setContrast(brightness);
    }

    @Override
    public void setContrast(float contrast) {
            delegate.setContrast(contrast);
    }

    @Override
    public double getDataValue(double x, double y) {
        int[] imageCoord = flattener.convertToImageSpace(x, y);
        if (imageCoord == null) {
            // coordinate is outside image area.
            return Double.NaN;
        }
        return delegate.getValue(imageCoord[0], imageCoord[1]);
    }

    @Override
    public void dispose() {
        delegate.dispose();
    }

    @Override
    public void stage() throws VizException {
        delegate.stage();
    }

    public DrawableImage getImage() throws VizException {
        return new DrawableImage(delegate, flattener.getPixelCoverage());
    }


}
