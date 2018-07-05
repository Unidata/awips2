package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * The matrix descriptor is needed in support of the VizMatrixEditor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2017            polster     Initial creation
 *
 * </pre>
 *
 * @author polster
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class MatrixDescriptor extends MapDescriptor {

    public MatrixDescriptor() throws VizException {
        super();
    }

    public MatrixDescriptor(CoordinateReferenceSystem crs, Coordinate llCoord,
            Coordinate urCoord) throws VizException {
        super(crs, llCoord, urCoord);
    }

    public MatrixDescriptor(GeneralGridGeometry gridGeometry) {
        super(gridGeometry);
    }

}
