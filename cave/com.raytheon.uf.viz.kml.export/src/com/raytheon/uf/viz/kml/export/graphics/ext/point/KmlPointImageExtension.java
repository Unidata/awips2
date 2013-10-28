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
package com.raytheon.uf.viz.kml.export.graphics.ext.point;

import java.awt.image.RenderedImage;
import java.util.Arrays;
import java.util.Collection;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.graphics.ext.KmlRasterImage;
import com.raytheon.uf.viz.kml.export.graphics.ext.KmlRasterImageExtension;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;

import de.micromata.opengis.kml.v_2_2_0.Coordinate;
import de.micromata.opengis.kml.v_2_2_0.Icon;
import de.micromata.opengis.kml.v_2_2_0.IconStyle;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.Point;
import de.micromata.opengis.kml.v_2_2_0.Style;

/**
 * 
 * Renders images at points as placemark icons in KML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlPointImageExtension extends GraphicsExtension<KmlGraphicsTarget>
        implements IPointImageExtension {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlRasterImageExtension.class);

    @Override
    public void drawPointImages(PaintProperties paintProps,
            PointImage... images) throws VizException {
        drawPointImages(paintProps, Arrays.asList(images));
    }

    @Override
    public void drawPointImages(PaintProperties paintProps,
            Collection<PointImage> images) throws VizException {
        target.addGenerator(new Generator(images));
    }

    @Override
    public int getCompatibilityValue(KmlGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    private static class Generator extends KmlFeatureGenerator {

        private final Collection<PointImage> images;

        public Generator(Collection<PointImage> images) {
            this.images = images;
        }

        @Override
        public void addFeature(KmlOutputManager outputManager) {
            if (images.size() > 1) {
                outputManager = outputManager.createFolder("Points");
            }
            for (PointImage image : images) {
                try {
                    Placemark placemark = new Placemark();
                    placemark.setName(image.getSiteId());
                    Coordinate loc = transformToLatLon(image.getX(),
                            image.getY());
                    Point point = placemark.createAndSetPoint();
                    point.addToCoordinates(loc.getLongitude(),
                            loc.getLatitude());
                    Style style = new Style();
                    style.createAndSetLabelStyle().setScale(0);

                    IconStyle iconStyle = style.createAndSetIconStyle();
                    iconStyle.setScale(options.getPlotIconScale());
                    double heading = 180 + MapUtil.rotation(
                            new com.vividsolutions.jts.geom.Coordinate(loc
                                    .getLongitude(), loc.getLatitude()),
                            gridGeometry);
                    iconStyle.setHeading(heading);
                    Icon icon = iconStyle.createAndSetIcon();

                    RenderedImage rImage = null;
                    if (options.isFillPlotBackground()) {
                        rImage = ((KmlRasterImage) image.getImage())
                                .getImage(backgroundColor);
                    } else {
                        rImage = ((KmlRasterImage) image.getImage()).getImage();
                    }
                    String siteId = image.getSiteId();
                    if (siteId == null) {
                        icon.setHref(outputManager.addImage(rImage));
                    } else {
                        siteId = siteId.replaceAll("\\?", "QuestionMark");
                        icon.setHref(outputManager.addImage(rImage,
                                "pointIcons/" + siteId + ".png"));
                    }
                    placemark.setStyleUrl(outputManager.getStyleUrl(style));
                    outputManager.addFeature(placemark);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (TransformException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (FactoryException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }
}
