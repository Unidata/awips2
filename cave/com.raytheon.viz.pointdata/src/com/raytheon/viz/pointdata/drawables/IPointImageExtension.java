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
package com.raytheon.viz.pointdata.drawables;

import java.util.Collection;

import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * An Extension for drawing images at a single point with a constant screen size
 * that is independent of the zoom level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public interface IPointImageExtension {

    public static class PointImage {

        // The image to draw at this point
        private IImage image;

        // anchor position x in target coordinates
        private double x;

        // anchor position y in target coordinates
        private double y;

        // height in screen pixels, if this is not provided the image height is
        // used
        private Double height = null;

        // width in screen pixels, if this is not provided the image width is
        // used
        private Double width = null;

        // The alignment relative to x,y
        private HorizontalAlignment horizontalAlignment = HorizontalAlignment.CENTER;

        // The alignment relative to x,y
        private VerticalAlignment verticalAlignment = VerticalAlignment.MIDDLE;

        // optional field, used to make prettier dispalys in some targets(kml)
        private String siteId = null;

        public PointImage() {

        }

        public PointImage(IImage image, double x, double y) {
            this.image = image;
            this.x = x;
            this.y = y;
        }

        public PointImage(IImage image, Coordinate c) {
            this.image = image;
            this.x = c.x;
            this.y = c.y;
        }

        public Double getHeight() {
            return height;
        }

        public void setHeight(Double height) {
            this.height = height;
        }

        public Double getWidth() {
            return width;
        }

        public void setWidth(Double width) {
            this.width = width;
        }

        public HorizontalAlignment getHorizontalAlignment() {
            return horizontalAlignment;
        }

        public void setHorizontalAlignment(
                HorizontalAlignment horizontalAlignment) {
            this.horizontalAlignment = horizontalAlignment;
        }

        public VerticalAlignment getVerticalAlignment() {
            return verticalAlignment;
        }

        public void setVerticalAlignment(VerticalAlignment verticalAlignment) {
            this.verticalAlignment = verticalAlignment;
        }

        public IImage getImage() {
            return image;
        }

        public void setImage(IImage image) {
            this.image = image;
        }

        public double getX() {
            return x;
        }

        public void setX(double x) {
            this.x = x;
        }

        public double getY() {
            return y;
        }

        public void setY(double y) {
            this.y = y;
        }

        public void setLocation(Coordinate c) {
            this.x = c.x;
            this.y = c.y;
        }

        public void setLocation(double x, double y) {
            this.x = x;
            this.y = y;
        }

        public String getSiteId() {
            return siteId;
        }

        public void setSiteId(String siteId) {
            this.siteId = siteId;
        }

        public boolean hasSiteId() {
            return siteId != null;
        }

    }

    public void drawPointImages(PaintProperties paintProps,
            PointImage... images) throws VizException;

    public void drawPointImages(PaintProperties paintProps,
            Collection<PointImage> images) throws VizException;
}
