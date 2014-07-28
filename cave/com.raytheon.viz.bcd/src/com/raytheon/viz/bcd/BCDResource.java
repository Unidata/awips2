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

package com.raytheon.viz.bcd;

import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.util.ArrayList;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.vadriver.VA_Advanced;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.WireframeCache;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Reads in a "D2D"-native BCD resource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 *    1/10/08       562         bphillip    Modified to handle .bcx files
 *    02/11/09                  njensen     Refactored to new rsc architecture
 *    07/31/12      DR 14935    D. Friedman Handle little-endian files
 *    Jul 28, 2014  3397        bclement    switched to non deprecated version of createWireframeShape()
 *                                          now closes on FileInputStream instead of FileChannel in initInternal()
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class BCDResource extends
        AbstractMapResource<BCDResourceData, MapDescriptor> implements
        IResourceDataChanged {

    /** The wireframe object */
    private IWireframeShape wireframeShape;

    private IGraphicsTarget lastTarget;

    private GeneralGridGeometry gridGeometry;

    /** Whether the resource is ready to be drawn */
    private boolean ready = false;

    private boolean isBCX = false;

    private int maxLen = 0;

    private IFont font;

    private ArrayList<BcxLabel> labels;

    private class BcxLabel {

        public Coordinate coord = new Coordinate();

        public double[] pixel;

        public double distance;

        public String label;

        public BcxLabel(String label, Coordinate coord,
                IMapDescriptor descriptor) {
            this.label = label;
            this.coord = coord;
            this.pixel = descriptor.worldToPixel(new double[] { coord.x,
                    coord.y });

        }
    }

    protected BCDResource(BCDResourceData resourceData, LoadProperties props) {
        super(resourceData, props);
        resourceData.addChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        WireframeCache.getInstance().unregisterWireframe(
                resourceData.getFilename(), gridGeometry);

        wireframeShape = null; // drop the reference

        lastTarget = null;
        resourceData.removeChangeListener(this);
        if (font != null) {
            font.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        FileInputStream fis = null;
        try {

            if (this.resourceData.getFilename().endsWith("bcx")) {
                isBCX = true;
                labels = new ArrayList<BcxLabel>();
            }
            wireframeShape = WireframeCache.getInstance().checkWireframe(
                    resourceData.getFilename(), descriptor.getGridGeometry());

            if (wireframeShape == null) {

                this.gridGeometry = descriptor.getGridGeometry();
                wireframeShape = target.createWireframeShape(true, descriptor,
                        0.0f);
                // wireframeShape = target.createWireframeShape(true,
                // mapDescriptor);
                File file = new File(resourceData.getFilename());
                if (!file.isAbsolute()) {
                    file = PathManagerFactory.getPathManager().getStaticFile(
                            FileUtil.join(VizApp.getMapsDir(),
                                    resourceData.getFilename()));
                }
                if (file == null || file.exists() == false) {
                    throw new VizException("Could not find bcd file",
                            new FileNotFoundException(String.valueOf(file)));
                }
                fis = new FileInputStream(file);
                FileChannel fc = fis.getChannel();

                ByteBuffer buffer = fc.map(MapMode.READ_ONLY, 0, file.length());

                // Determine byte order of data
                if (buffer.remaining() >= 4) {
                    // Whether BCX or not, first value is an int.
                    // Note: Different from A1 which tests >31 or >500
                    if (buffer.getInt(0) > Short.MAX_VALUE)
                        buffer.order(ByteOrder.LITTLE_ENDIAN);
                }

                double minLat = Double.MAX_VALUE;
                double minLon = Double.MAX_VALUE;
                double maxLat = Double.MIN_VALUE;
                double maxLon = Double.MIN_VALUE;

                double[] labelPixel = new double[2];
                while (buffer.hasRemaining()) {

                    String label = null;
                    if (isBCX) {
                        // Read the Text
                        int length = buffer.getInt();
                        byte[] dst = new byte[length];
                        buffer.get(dst, 0, length);
                        label = new String(dst);
                        if (label.length() > maxLen) {
                            maxLen = label.length();
                        }
                    }

                    // Read a record
                    int n0 = buffer.getInt();

                    Coordinate[] pts = new Coordinate[n0];

                    @SuppressWarnings("unused")
                    float lat1 = buffer.getFloat();
                    @SuppressWarnings("unused")
                    float lat2 = buffer.getFloat();
                    @SuppressWarnings("unused")
                    float lon1 = buffer.getFloat();
                    @SuppressWarnings("unused")
                    float lon2 = buffer.getFloat();

                    int i0 = 0;
                    while (i0 < n0) {
                        float lat0 = buffer.getFloat();
                        float lon0 = buffer.getFloat();

                        minLat = Math.min(lat0, minLat);
                        minLon = Math.min(lon0, minLon);
                        maxLat = Math.max(lat0, maxLat);
                        maxLon = Math.max(lat0, maxLon);

                        pts[i0] = new Coordinate(lon0, lat0);
                        i0++;
                    }
                    wireframeShape.addLineSegment(pts);
                    if (isBCX) {
                        labels.add(new BcxLabel(label, pts[0], descriptor));
                        wireframeShape.addLabel(label, labelPixel);
                    }
                }

                if (isBCX) {
                    VA_Advanced distanceCalc = new VA_Advanced();
                    Coordinate[] coords = new Coordinate[labels.size()];
                    Integer[] goodness = new Integer[labels.size()];
                    Double[] dst = new Double[labels.size()];
                    for (int j = 0; j < labels.size(); j++) {
                        coords[j] = labels.get(j).coord;
                        goodness[j] = -1;
                        dst[j] = 0d;
                    }
                    Double[] distances = distanceCalc.getVaAdvanced(coords,
                            goodness, dst);

                    for (int j = 0; j < labels.size(); j++) {
                        labels.get(j).distance = distances[j];
                    }

                }
                WireframeCache.getInstance().registerWireframe(
                        resourceData.getFilename(),
                        descriptor.getGridGeometry(), this.wireframeShape);
            }

        } catch (Exception e) {
            throw new VizException("Error loading BCD", e);

        } finally {
            try {
                if (fis != null) {
                    fis.close();
                }
            } catch (IOException e) {
                // ignore
            }

        }
        ready = true;

        this.lastTarget = target;
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
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (ready && isBCX) {
            int displayWidth = (int) (descriptor.getMapWidth() * paintProps
                    .getZoomLevel());

            double metersPerPixel = displayWidth
                    / paintProps.getCanvasBounds().width;

            double magnification = getCapability(MagnificationCapability.class)
                    .getMagnification();
            double density = getCapability(DensityCapability.class)
                    .getDensity();

            if (font == null) {
                font = target.initializeFont(target.getDefaultFont()
                        .getFontName(), (float) (10 * magnification), null);
            }
            Rectangle2D charSize = target.getStringBounds(font, "N");
            double charWidth = charSize.getWidth();

            double minSepDist = metersPerPixel / 1000.0 / density * charWidth;

            if (maxLen <= 5) {
                minSepDist *= maxLen;
            } else {
                minSepDist *= 5.0 * Math.sqrt(maxLen / 5.0);
            }

            IView view = paintProps.getView();

            for (BcxLabel p : labels) {
                if (p.pixel == null) {
                    continue;
                }

                if (view.isVisible(p.pixel) && p.distance >= minSepDist) {

                    target.drawString(
                            font,
                            p.label,
                            p.pixel[0],
                            p.pixel[1],
                            0.0,
                            IGraphicsTarget.TextStyle.NORMAL,
                            getCapability(ColorableCapability.class).getColor(),
                            HorizontalAlignment.CENTER, null);
                }
            }
        }

        if (ready && getCapability(OutlineCapability.class).isOutlineOn()) {
            target.drawWireframeShape(wireframeShape,
                    getCapability(ColorableCapability.class).getColor(),
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        WireframeCache.getInstance().unregisterWireframe(
                resourceData.getFilename(), gridGeometry);

        this.initInternal(this.lastTarget);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                if (font != null) {
                    font.dispose();
                    font = null;
                }
            }
        }
    }
}