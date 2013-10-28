package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 09/2013			    	Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import java.awt.Color;
import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.FillDisplayElement;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;
import gov.noaa.nws.ncep.ui.pgen.display.LineDisplayElement;

/*
 * Chin Note: This class extends from PGEN's DisplayElementFactory. Its purpose is only for 
 * Nsharp to draw wind barb in SkewT display. As skewT has its own log scale coordination.
 * The key word, CHANGED, is used to mark changes from original PGEN code. 
 */
public class NsharpDisplayElementFactory extends DisplayElementFactory {

	public NsharpDisplayElementFactory(IGraphicsTarget target,
			IDescriptor iDescriptor) {
		super(target, iDescriptor);
		// TODO Auto-generated constructor stub
	}

    @SuppressWarnings("deprecation")
	private ArrayList<IDisplayable> createWindBarb(IVector vect) {
        double sfactor = deviceScale * vect.getSizeScale() * 10.;
        IWireframeShape mask = null;
        Color bgColor = new Color(0,0,0);    // default black
        
	    /*
         * Create the List to be returned, and wireframe shape
         */
        ArrayList<IDisplayable> slist = new ArrayList<IDisplayable>();
        IWireframeShape barb = target.createWireframeShape(false, iDescriptor);
        IShadedShape flags = target.createShadedShape(false, iDescriptor, false);
        if ( vect.hasBackgroundMask() ) {
        	mask = target.createWireframeShape(false, iDescriptor);
			RGB bg = backgroundColor.getColor(BGColorMode.EDITOR);
			bgColor = new Color(bg.red, bg.green, bg.blue);
        }
        
	    /*
	     * Get color for creating displayables.
	     */
	    Color dspClr = getDisplayColor( vect.getColor() );

        /*
         * CHANGED: these 2 lines changed 
         */
        double[] start = { vect.getLocation().x, vect.getLocation().y, 0.0 };
        //double[] start = iDescriptor.worldToPixel(tmp);
        
        /*
         * If calm wind, draw circle
         */
        if ( vect.getSpeed() < 0.5 ) {
        	double[][] pts = calculateCircle(start,sfactor*0.1);
        	if ( vect.hasBackgroundMask() ) {
            	mask.addLineSegment(pts);
                mask.compile();
                slist.add( new LineDisplayElement(mask, bgColor, vect.getLineWidth() + (float)deviceScale) );
        	}
        	barb.addLineSegment(pts);
            barb.compile();
            slist.add( new LineDisplayElement(barb, dspClr, vect.getLineWidth()) );
            return slist;
        }
     
        /*
         *  Compute the number of flags, whole barbs and half barbs
         *  needed to represent the wind speed.
         */
        int speed = (int)Math.floor(vect.getSpeed() + 2.5);
        int numflags = speed / 50;
        int remainder = speed % 50;
        int numbarbs = remainder / 10;
        remainder = remainder % 10;
        int halfbarbs = remainder / 5;
        
        double MAX_SEGMENTS = 6.0;       //   Maximum number of segments on original size barb
        int numsegs = (2 * numflags) + numbarbs + halfbarbs;
        double segmentSpacing = sfactor / MAX_SEGMENTS;
        double windLength = segmentSpacing * Math.max( MAX_SEGMENTS, numsegs);
        double barbLength = sfactor /3.0;
        
        /* 
         * find the end point of the wind barb
         */
        //CHANGED:this line changed
        double angle = -90.0 + vect.getDirection();
        double[] end = new double[3];
        end[0] = start[0] + (windLength * Math.cos(Math.toRadians(angle)) );
        end[1] = start[1] + (windLength * Math.sin(Math.toRadians(angle)) );
        end[2] = 0.0;
        //System.out.println("X0="+start[0]+" Y0="+start[1]+" X1="+end[0]+" Y1="+ end[1]);
        
        barb.addLineSegment(new double[][] {start, end});
        if ( vect.hasBackgroundMask() ) mask.addLineSegment(new double[][] {start, end});
        
		/*
		 *  Create a LengthIndexedLine used to reference points along the path
		 *  at specific distances
		 */
        LineString[] ls = toLineString(new Coordinate[] {new Coordinate(start[0],start[1]),
        		                                         new Coordinate(end[0], end[1]) } );
		LengthIndexedLine lil = new LengthIndexedLine(ls[0]);
		double currentLoc = lil.getEndIndex();     //  start from tail end

		//TODO - orientation issues
		double BARB_ANGLE = 70.0;
        double barbAngle = angle + BARB_ANGLE;
        if ( vect.getLocation().y < 0.0 ) barbAngle = angle - BARB_ANGLE;
        double cosineBarbAngle = Math.cos(Math.toRadians(barbAngle));
        double sineBarbAngle = Math.sin(Math.toRadians(barbAngle));
        
		/*
		 * Process flags
		 */
		for ( int j=0; j<numflags; j++) {
			Coordinate coords[] = new Coordinate[4];
			coords[0] = lil.extractPoint(currentLoc);
			coords[1] = lil.extractPoint(currentLoc-segmentSpacing);
			double xtip = coords[1].x + ( barbLength * cosineBarbAngle );
			double ytip = coords[1].y + ( barbLength * sineBarbAngle );
			coords[2] = new Coordinate(xtip,ytip);
			coords[3] = coords[0];
			LineString[] oneFlag = toLineString(coords);
			flags.addPolygonPixelSpace(oneFlag, new RGB( dspClr.getRed(), dspClr.getGreen(), dspClr.getBlue()));
	        if ( vect.hasBackgroundMask() ) mask.addLineSegment(toDouble(coords));
			currentLoc -= 2 * segmentSpacing;
		}
		
		/*
		 * Process barbs
		 */
		for ( int j=0; j<numbarbs; j++) { 
			Coordinate coords[] = new Coordinate[2];
			coords[0] = lil.extractPoint(currentLoc);
			double xtip = coords[0].x + ( barbLength * cosineBarbAngle );
			double ytip = coords[0].y + ( barbLength * sineBarbAngle );
			coords[1] = new Coordinate(xtip,ytip);
			double[][] pts = toDouble(coords);
			barb.addLineSegment(pts);
	        if ( vect.hasBackgroundMask() ) mask.addLineSegment(pts);
			currentLoc -= segmentSpacing;
		}
		
		/*
		 * Process half barbs
		 */
		for ( int j=0; j<halfbarbs; j++) { 
			Coordinate coords[] = new Coordinate[2];
			coords[0] = lil.extractPoint(currentLoc);
			double xtip = coords[0].x + ( 0.5 * barbLength * cosineBarbAngle );
			double ytip = coords[0].y + ( 0.5 * barbLength * sineBarbAngle );
			coords[1] = new Coordinate(xtip,ytip);
			double[][] pts = toDouble(coords);
			barb.addLineSegment(pts);
	        if ( vect.hasBackgroundMask() ) mask.addLineSegment(pts);
			currentLoc -= segmentSpacing;
		}
		
    	if ( vect.hasBackgroundMask() ) {
            mask.compile();
            slist.add( new LineDisplayElement(mask, bgColor, vect.getLineWidth() + (float)deviceScale) );
    	}
    	
		/*
         * 
        */
		flags.compile();
		FillDisplayElement fde = new FillDisplayElement(flags, vect.getColor().getAlpha());
        slist.add(fde);
        
		/*
		 * add shaft wireframe to return list
		 */
        barb.compile();
        slist.add( new LineDisplayElement(barb, dspClr, vect.getLineWidth()) );
		
        return slist;
    }

	@Override
	//CHANGED from original PGEN code to just handle wind barbs creation
	public ArrayList<IDisplayable> createDisplayElements(IVector vect,
			PaintProperties paintProps) {
		setScales(paintProps);
    	
        ArrayList<IDisplayable> slist = createWindBarb(vect);
        return slist;
	}

}
