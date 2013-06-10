/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import java.awt.Graphics2D;
import java.awt.geom.Path2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.io.DataInput;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.PolygonElement;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;


/**
 * @author bhebbard
 *
 */
public class NcPolygonElement extends PolygonElement implements INcCommand {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	static List<double[]> currentDraw = new ArrayList<double[]>();

	private GeometryFactory gf;

	/**
	 * @param ec
	 * @param eid
	 * @param l
	 * @param in
	 * @throws IOException
	 */
    public NcPolygonElement(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
		gf = new GeometryFactory();  //TODO  move!?
		// TODO Auto-generated constructor stub
    }

	@Override
	public void paint(IGraphicsTarget target, PaintProperties paintProps,
			IDescriptor descriptor, ImageBuilder ib)
			throws VizException {
		
		if (ib.shadedShapeReady) return;  //  work already done

	    PathIterator pi = this.polygon.getPathIterator(null);
	    
	    while (pi.isDone() == false) {
		      //processCurrentSegment(pi, shadedShape, ib);
		      processCurrentSegment(pi, ib.shadedShape, ib);
		      pi.next();
		    }
	    
		if (currentDraw.size() > 1) {
			//  Just in case SEG_CLOSE missing at end 
			//terminatePolygon(shadedShape, ib);
			terminatePolygon(ib.shadedShape, ib);
		}
		currentDraw.clear();
		
		//shadedShape.compile();
		
		//ib.shadedShapes.add(shadedShape);
	
	}

	private void processCurrentSegment(PathIterator pi, IShadedShape shadedShape,
			ImageBuilder ib) {
    	double[] coordinates = new double[6];
    	
    	int type = pi.currentSegment(coordinates);
    	switch (type) {
    	case PathIterator.SEG_MOVETO:
    		//System.out.println("move to " + coordinates[0] + ", " + coordinates[1]);
    		currentDraw.clear();
    		currentDraw.add( ib.scalePoint(coordinates) );
    		break;
    	case PathIterator.SEG_LINETO:
    		//System.out.println("line to " + coordinates[0] + ", " + coordinates[1]);
    		currentDraw.add( ib.scalePoint(coordinates) );
    		break;
    	case PathIterator.SEG_QUADTO:
    		//System.out.println("quadratic to " + coordinates[0] + ", " + coordinates[1] + ", "
    		//		+ coordinates[2] + ", " + coordinates[3]);
    		//TODO -- error / not supported
    		break;
    	case PathIterator.SEG_CUBICTO:
    		//System.out.println("cubic to " + coordinates[0] + ", " + coordinates[1] + ", "
    		//		+ coordinates[2] + ", " + coordinates[3] + ", " + coordinates[4] + ", " + coordinates[5]);
    		//TODO -- error / not supported
    		break;
    	case PathIterator.SEG_CLOSE:
			terminatePolygon(shadedShape, ib);
    		break;
    	default:
    		break;
    	}
	}
	

	private void terminatePolygon(IShadedShape shadedShape, ImageBuilder ib) {
		if (currentDraw.size() > 1) {
			Coordinate[] coords = new Coordinate[currentDraw.size()];
			for (int j=0; j<currentDraw.size(); j++) {
				coords[j] = new Coordinate(currentDraw.get(j)[0],
						1000.000 - //TODO why?
						currentDraw.get(j)[1]);
			}
			LineString[] lineStrings = new LineString[] { gf.createLineString(coords) };
			shadedShape.addPolygon(lineStrings, ib.currentFillColor);
		}
		currentDraw.clear();
	}

}

