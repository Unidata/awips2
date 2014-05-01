/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.operation.overlay.OverlayOp;

/**
 * @author sgilbert
 *
 */
public class AdvisoryUtils {

	public static HashMap<TropicalCycloneAdvisory, String> createSegmentMap(
			TropicalCycloneAdvisory previous, TropicalCycloneAdvisory next) {

		HashMap<TropicalCycloneAdvisory, String> advmap = new HashMap<TropicalCycloneAdvisory, String>();
		
		if ( !(previous.getSegment() instanceof BreakpointPair) ||
			 !(next.getSegment() instanceof BreakpointPair) ) return advmap;
		
		Breakpoint prevBkpt1 = previous.getSegment().getBreakpoints().get(0);
		Breakpoint prevBkpt2 = previous.getSegment().getBreakpoints().get(1);
		Breakpoint currBkpt1 = next.getSegment().getBreakpoints().get(0);
		Breakpoint currBkpt2 = next.getSegment().getBreakpoints().get(1);
		
		BreakpointManager bm = BreakpointManager.getInstance();
		String coastName = bm.findCoastName(prevBkpt1);
		if ( ! coastName.equals( bm.findCoastName(currBkpt1) ) ) return advmap; 
			
		int pindex1 = bm.coastIndexOf(prevBkpt1);
		int pindex2 = bm.coastIndexOf(prevBkpt2);
		int cindex1 = bm.coastIndexOf(currBkpt1);
		int cindex2 = bm.coastIndexOf(currBkpt2);
		
		/*
		 * Going to use 1 dimensional geometries ( Y coord always set to 0 ) to
		 * find the segments in common and not.
		 */
		GeometryFactory gf = new GeometryFactory();
		LineString geom0 = gf.createLineString( new Coordinate[] { new Coordinate(pindex1,0), new Coordinate(pindex2,0) });
		LineString geom1 = gf.createLineString( new Coordinate[] { new Coordinate(cindex1,0), new Coordinate(cindex2,0) });
		//System.out.println("GEOM0: "+geom0);
		//System.out.println("GEOM1: "+geom1);
		/*
		 * Find the segment common to both.  This segment is a "Continuation"
		 */
		Geometry result = OverlayOp.overlayOp(geom0, geom1, OverlayOp.INTERSECTION);
		if ( ! result.isEmpty() ) {
			//System.out.println("RESULT"+result);
			int index1 = (int)Math.rint(result.getCoordinates()[0].x);
			int index2 = (int)Math.rint(result.getCoordinates()[1].x);
			BreakpointPair newPair = bm.getBreakpointPair(coastName, index1, index2);
			TropicalCycloneAdvisory adv = previous.copy();
			adv.setSegment(newPair);
			advmap.put(adv, TCVMessage.ACTION_CONT);
		}
		
		/*
		 * Find the segments in previous, but not in next.  These segments are "Cancel"
		 */
		result = OverlayOp.overlayOp(geom0, geom1, OverlayOp.DIFFERENCE);
		if ( ! result.isEmpty() ) {
		//	if ( result instanceof GeometryCollection ) {
				for ( int j=0; j<result.getNumGeometries(); j++ ) {
					Geometry g = result.getGeometryN(j);
					//System.out.println("G"+g);
					int index1 = (int)Math.rint(g.getCoordinates()[0].x);
					int index2 = (int)Math.rint(g.getCoordinates()[1].x);
					BreakpointPair newPair = bm.getBreakpointPair(coastName, index1, index2);
					TropicalCycloneAdvisory adv = previous.copy();
					adv.setSegment(newPair);
					advmap.put(adv, TCVMessage.ACTION_CANCEL);
				}
		//	}
		//	else {
		//		TropicalCycloneAdvisory adv = createAdvisory();
		//		advmap.put(adv, TCVMessage.ACTION_CANCEL);
		//	}
		}

		/*
		 * Find the segments in next, but not in previous.  These segments are "NEW"
		 */
		result = OverlayOp.overlayOp(geom1, geom0, OverlayOp.DIFFERENCE);
		if ( ! result.isEmpty() ) {
			for ( int j=0; j<result.getNumGeometries(); j++ ) {
				Geometry g = result.getGeometryN(j);
				//System.out.println("G"+g);
				int index1 = (int)Math.rint(g.getCoordinates()[0].x);
				int index2 = (int)Math.rint(g.getCoordinates()[1].x);
				BreakpointPair newPair = bm.getBreakpointPair(coastName, index1, index2);
				TropicalCycloneAdvisory adv = previous.copy();
				adv.setSegment(newPair);
				advmap.put(adv, TCVMessage.ACTION_NEW);
			}
		}

		return advmap;
	}

	public static List<TropicalCycloneAdvisory> segmentAdvisory(
			TropicalCycloneAdvisory tcadv, Set<Breakpoint> bkptSet) {
		
		List<TropicalCycloneAdvisory> alist = new ArrayList<TropicalCycloneAdvisory>();
		
		BreakpointManager bm = BreakpointManager.getInstance();
		String coast = bm.findCoastName(tcadv.getSegment().getBreakpoints().get(0));
		
		Breakpoint bkpt1 = tcadv.getSegment().getBreakpoints().get(0);
		Breakpoint bkpt2 = tcadv.getSegment().getBreakpoints().get(1);
		int index1 = bm.coastIndexOf(bkpt1);
		int index2 = bm.coastIndexOf(bkpt2);
		
		TreeSet<Integer> indexes = new TreeSet<Integer>();
		indexes.add( new Integer(index1) );
		indexes.add( new Integer(index2) );
		
		for ( Breakpoint b : bkptSet ) {
			if ( coast.equals( bm.findCoastName(b) ) ) {
				int idx = bm.coastIndexOf(b);
				if ( (idx>index1) && (idx<index2) ) indexes.add( new Integer(idx) );
			}
		}
		
		if ( indexes.size() > 2 ) {
			Integer[] idxs = indexes.toArray(new Integer[indexes.size()]);
			for (int j=0; j<indexes.size()-1; j++ ) {
				BreakpointPair newPair = bm.getBreakpointPair(coast, idxs[j], idxs[j+1]);
				TropicalCycloneAdvisory adv = tcadv.copy();
				adv.setSegment(newPair);
				alist.add(adv);
			}
		}
		
		return alist;
	}

}
