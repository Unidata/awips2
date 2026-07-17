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
package com.raytheon.uf.viz.image.export.job;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.api.coverage.grid.GridCoverage;
import org.geotools.api.coverage.grid.GridCoverageWriter;
import org.geotools.api.geometry.Bounds;
import org.geotools.api.referencing.operation.MathTransform;
import org.geotools.api.referencing.operation.TransformException;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.processing.Operations;
import org.geotools.gce.geotiff.GeoTiffFormat;
import org.geotools.geometry.Position2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Save images in the {@link GeoTiffFormat}.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 07, 2015  4607     bsteffen    Initial Creation
 * May 07, 2024  2037231  aford       Upgrade GeoTools to 31
 *
 * </pre>
 * 
 * @author bsteffen
 * @version 1
 */
public class SaveGeotiffJob extends AbstractSaveImagesJob {

    private GridCoverageFactory factory;

    private GeoTiffFormat format;

    private Bounds sourceEnvelope;

    private GridGeometry2D targetGeometry;

    public SaveGeotiffJob(ImageExportOptions options,
            LinkedHashMap<DataTime, BufferedImage> dtbiHash) {
        super(options, dtbiHash);
    }

    @Override
    protected boolean initialize() {
        factory = new GridCoverageFactory();
        format = new GeoTiffFormat();
        IDisplayPane pane = EditorUtil.getActiveVizContainer()
                .getActiveDisplayPane();
        IExtent extent = pane.getRenderableDisplay().getView().getExtent();
        GeneralGridGeometry paneGeom = pane.getDescriptor().getGridGeometry();
        MathTransform grid2crs = paneGeom.getGridToCRS();
        Position2D min = new Position2D(extent.getMinX(), extent.getMinY());
        Position2D max = new Position2D(extent.getMaxX(), extent.getMaxY());
        try {
            grid2crs.transform(min, min);
            grid2crs.transform(max, max);
            sourceEnvelope = new ReferencedEnvelope(min.x, max.x, min.y, max.y,
                    paneGeom.getCoordinateReferenceSystem());
            Bounds targetEnv = CRS.transform(sourceEnvelope,
                    DefaultGeographicCRS.WGS84);
            Rectangle b = pane.getRenderableDisplay().getBounds();
            targetGeometry = new GridGeometry2D(new GridEnvelope2D(0, 0,
                    b.width * 3 / 2, b.height * 3 / 2), targetEnv);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to determine geospatial boundary.", e);
            return false;
        }
        return true;
    }

    @Override
    protected void writeImage(File file, BufferedImage image)
            throws IOException {
        GridCoverage coverage = factory.create(file.getName(), image,
                sourceEnvelope);
        try {
            coverage = (GridCoverage) Operations.DEFAULT.resample(coverage,
                    targetGeometry.getCoordinateReferenceSystem(),
                    targetGeometry, null);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error reprojecting image.",
                    e);
            return;
        }
        GridCoverageWriter writer = format.getWriter(file);
        writer.write(coverage, null);
        writer.dispose();
    }

    @Override
    protected void writeAnimation(IProgressMonitor monitor) throws IOException {
        throw new UnsupportedOperationException("GeoTIFF cannot be animated.");
    }

}
