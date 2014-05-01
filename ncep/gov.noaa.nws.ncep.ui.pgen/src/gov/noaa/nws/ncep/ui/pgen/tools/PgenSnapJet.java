/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenSnapJet
 * 
 * 22 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;
import java.util.Iterator;

import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.JetAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.CurveFitter;
import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.IJetTools;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet.JetHash;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet.JetText;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

/**
 * Implements snap functions for jet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		#135		B. Yin   	Initial Creation.
 * 08/09		#135		B. Yin		Put flight text under the wind barb
 * 08/09		#135		B. Yin		Added methods to handle relative locations
 * 10/05/09     #169        Greg Hull   integrate with NCMapEditor
 * 10/14/09		?			B. Yin		Added hashes from the max wind to two ends.
 * 12/10		#366		B. Yin		comment out adding hash
 *										fixed text location for SHEM. 
 * 04/11		?			B. Yin		fixed zoom/unzoom problem in R1G2-9
 * 04/11		?			B. Yin		Re-factor IAttribute
 * 10/11		?			B. Yin		Fixed the flight level text location issue.
 * 06/12		TTR102		B. Yin		Added addBarbHashFromAnotherJet() for 'DelPart'
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenSnapJet implements IJetTools{
	
	// Map editor is needed for zooming.
	// Map descriptor  is needed when projection changes
	private IMapDescriptor descriptor;
//	private NCMapEditor mapEditor;
	private AbstractEditor mapEditor;
	private JetAttrDlg jetDlg;
	private JetHash hashTmp;
	private static final double defaultZoomLevel = 0.24;

	/**
	 * public constructor
	 * @param mapEditor
	 */
//	public PgenSnapJet( IMapDescriptor des, NCMapEditor editor, JetAttrDlg dlg ){
	public PgenSnapJet( IMapDescriptor des, AbstractEditor editor, JetAttrDlg dlg ){
		this.descriptor = des;
		this.mapEditor = editor;
		this.jetDlg = dlg;
	}
	

	@Override
	/**
	 * Snap all barbs and FL info on the input jet
	 */
	public void snapJet(Jet aJet){
		
		double[][] lineCurvePts = getJetCurve( aJet );

		Iterator<AbstractDrawableComponent> it = aJet.getComponentIterator();
		
		while(it.hasNext()){
			
			AbstractDrawableComponent adc = it.next();
			if ( adc.getName().equalsIgnoreCase("WindInfo")){
				snapBarbOnJet(adc, lineCurvePts);
			}
		}
		
		snapAllHashs( aJet );
		//addHashOnJet(aJet, lineCurvePts);
		checkHashOnJet(aJet);
	}
	
	@Override
	/**
	 * Snap the input barb/FL info on the input jet
	 */
	public void snapJet(AbstractDrawableComponent barb, Jet aJet){
		
		double[][] lineCurvePts = getJetCurve( aJet );
		snapBarbOnJet(barb, lineCurvePts);
		
		//addHashOnJet(aJet, lineCurvePts);
		
		checkHashOnJet(aJet);

	}

	/**
	 * Translate direction between screen and map
	 * s2m = true : screen to map.
	 * s2m = false : map to screen
	 */
	private double translateDir(double dir, double lat, double lon, boolean s2m){
		
		double rv;
		
		double pt1[] = descriptor.worldToPixel(new double[]{lon, lat} );
		double pt2[] = descriptor.worldToPixel(new double[]{lon, lat +5} );
		
		double delx = pt2[0] - pt1[0];
		double dely = pt2[1] - pt1[1];

		if (( delx == 0 ) && (dely == 0 )){
			rv  = dir;
		}
		else {
			double rot = Math.atan2(delx, -dely);
			if ( s2m ){
				rv = dir - rot*180/Math.PI;
			}
			else {
				rv = dir + rot*180/Math.PI;
			}
		}
		
		if( rv > 360 ){
			rv -= 360;
		}
		else if (rv < 0 ){
			rv += 360;
		}
		
		return rv;
	}

	/**
	 *	Get all points from the curve fitter.
	 */
	private double[][] getJetCurve( Jet aJet){
		// convert lat/lon of the jet line points to screen coordinate 
		double[][] lineScnPts = new double[aJet.getJetLine().getLinePoints().length][2];
		
		int ii = 0;
		for (Coordinate loc : aJet.getJetLine().getLinePoints() ){
			double[] pix = {loc.x, loc.y };
			lineScnPts[ii] = descriptor.worldToPixel(pix);
			ii++;
		}
		
		return CurveFitter.fitParametricCurve( lineScnPts, .5f );

	}
	
	/**
	 *	Return the index of the nearest point on the curve.
	 */
	private int getNearestPtOnCurve( double[][] lineCurvePts, double pt[] ){
		double minDistance = -999;
		int nearestId = 0;
		
		for ( int ii = 0; ii < lineCurvePts.length; ii++ ){
		
			double dist = Math.sqrt( (pt[1] - lineCurvePts[ii][1])
								    *(pt[1] - lineCurvePts[ii][1]) 
								    +(pt[0] - lineCurvePts[ii][0])
								    *(pt[0] - lineCurvePts[ii][0]));

			if ( minDistance <  0 || dist < minDistance ) {
				minDistance = dist;
				nearestId = ii;
			}
		}
		
		return nearestId;
	}
	
	/**
	 * get the wind direction at the input point on the curve.
	 */
	private double getDirectionAtPoint(double[][] lineCurvePts, int id){
	
		double dir;
		if ( id == 0 ){
			
			dir = Math.atan2(lineCurvePts[id][1]- lineCurvePts[id+1][1], 
							 lineCurvePts[id][0]- lineCurvePts[id+1][0]);
			
		}
		else if ( id == lineCurvePts.length - 1  ){
			
			dir = Math.atan2(lineCurvePts[id-1][1]- lineCurvePts[id][1], 
							 lineCurvePts[id-1][0]- lineCurvePts[id][0]);
			
		}
		else {
			double dir1 = Math.atan2(lineCurvePts[id-1][1]- lineCurvePts[id][1], 
									 lineCurvePts[id-1][0]- lineCurvePts[id][0]);
			double dir2 = Math.atan2(lineCurvePts[id][1]- lineCurvePts[id+1][1], 
									 lineCurvePts[id][0]- lineCurvePts[id+1][0]);

			// get the average
			if ( dir1*dir2 < 0 ){
				dir = 0.5*(dir2-dir1);
			}
			else {
				dir = 0.5*(dir1+dir2);
			}
		}
		
		//output of Math.atan2 is from -180 to 180
		//convert direction to 0-360
		dir =dir*180/Math.PI;
		if (dir < 0 ) dir+=360;

		//for barb, 0 is north, 90 is east(positive x)
		//in screen coordinate 0 is east
		dir = 90 + dir;
		if (dir < 0 ) {
			dir+=360;
		}
		else if ( dir > 360 ){
			dir -=360;
		}
		
		return dir;
		
	}
	
	/**
	 * Set the direction for the barb and the FL
	 */
	private void updateWind(AbstractDrawableComponent wind, Coordinate loc,
				double dir, double mapDir){
				
		Iterator<DrawableElement> itde = wind.createDEIterator();
		
		while (itde.hasNext()){
			SinglePointElement spe = (SinglePointElement)itde.next();
			
			if( spe instanceof Jet.JetBarb ){
				((Jet.JetBarb)spe).setLocationOnly(loc);
				((Jet.JetBarb)spe).setDirection(mapDir);
				
			}
			else if ( spe instanceof JetText ){
					
				double txtDir = (dir>270)?270-dir+360:270-dir;
				
				//set default locations of the flight levels
				Coordinate txtLoc = latLon2Relative(((JetText)spe).getLocation(), (Vector)spe.getParent().getPrimaryDE());
				if ( txtLoc.x  < 0.001 ){
					
					// theta and distance define the relative location of fl text to wind barb
					double theta;
					double distance;	
					
					if ( dir > 0 && dir < 180 ) {
						//westbound jet
						//for two-line text, increase distance and theta
						if (((Text)spe).getText().length > 1 ){
							theta = 35;
							distance = 110;
						}
						else {
							theta = 26;
							distance = 95;
						}

					}
					else {
						//eastbound jet
						//for two-line text, increase distance and theta
						if (((Text)spe).getText().length > 1 ){
							theta = 33;
							distance = 110;

						}
						else {
							theta = 20;
							distance = 95;	
						}
					}
					
					//for SHEM
					if ( spe.getLocation().y < 0 ){
							theta *= -1;
							theta += -20;
					}
					
				//	distance /= mapEditor.getSelectedPane().getZoomLevel() / defaultZoomLevel;
					((JetText)spe).setLocation(relative2LatLon(new Coordinate(distance, theta),
							(Vector)spe.getParent().getPrimaryDE()));
				}
				
				// for jet from east to west, rotate fl text
				// so that it reads from left to right
				if ( dir > 0 && dir < 180 ) {
					txtDir += 180;
					if ( txtDir > 360 ) txtDir -= 360;
					//for SHEM????
					if ( spe.getLocation().y < 0 ) {
						txtDir += 180;
						if ( txtDir > 360 ) txtDir -= 360;
					}

					
				}
				
				
				((JetText)spe).setRotation( txtDir );
			}
		}
	}
	
	/**
	 * Snap the input barb on the jet curve
	 */
	private void snapBarbOnJet(AbstractDrawableComponent barb, double[][] lineCurvePts){
		
		Coordinate loc = ((ISinglePoint)(barb.getPrimaryDE())).getLocation();
		double barbPt[] = descriptor.worldToPixel(new double[]{loc.x,loc.y});

		int nearestId = getNearestPtOnCurve(lineCurvePts, barbPt );
		//barb direction in screen coordinate 
		double dir = getDirectionAtPoint(lineCurvePts, nearestId);
		double[] pix = descriptor.pixelToWorld(lineCurvePts[nearestId]);
		Coordinate nearestPt = new Coordinate(pix[0], pix[1]);
		//barb direction in map coordinate
		//translate direction from screen to map
		double mapDir = translateDir(dir, nearestPt.y, nearestPt.x, true);

		//set the direction and location for barb and FL text
		updateWind(barb, nearestPt, dir, mapDir);
		
	}

	/**
	 * Calculate hash locations and put them on the jet. 
	 * @param aJet
	 * @param lineCurvePts
	 */
	private void addHashOnJet(Jet aJet, double[][] lineCurvePts){
		
		if ( aJet.size() < 2 ) return;
		
		double[] speed = new double[aJet.size()+1];
		int[] index = new int[aJet.size()+1];
		
		speed[0] = 80;
		index[0] = 0;
		speed[1] = 80;
		index[1] = lineCurvePts.length;
		
		int counter = 2;

		//get existing hash attributes from the jet
		Iterator<DrawableElement> iterator = aJet.createDEIterator();
		while(iterator.hasNext()){
			DrawableElement de = iterator.next();
			if ( de instanceof JetHash ){
				hashTmp = (JetHash)de;
				break;
			}
		}
		
		aJet.removeAllHash();
		
		Iterator<DrawableElement> it = aJet.createDEIterator();
		int maxSpeedIdx = 0;
		double maxSpeed = -1;
		
		// get point index and speed of all barbs
		// also find the max wind speed and its array index.
		while(it.hasNext()){
			DrawableElement adc = it.next();
			if ( adc instanceof Jet.JetBarb ){
				Coordinate adcLoc = ((Jet.JetBarb)adc).getLocation();
				double barbPt[] = descriptor.worldToPixel(new double[]{adcLoc.x, adcLoc.y});

				int nearestId = getNearestPtOnCurve(lineCurvePts, barbPt );
				index[counter] = nearestId;
				speed[counter] = ((Jet.JetBarb)adc).getSpeed();
				if ( maxSpeed < 0 || speed[counter] > maxSpeed ){
					maxSpeed = speed[counter];
					maxSpeedIdx = counter;
				}
				counter++;
			}
		}
		
		// sort the index and speed arrays by index
		for ( int ii = 0; ii < counter - 1; ii++ ){
			for ( int jj = 0; jj < counter -1; jj ++ ){
				if ( index[jj] > index[jj+1] ){
					int hold1 = index[jj];
					index[jj] = index[jj+1];
					index[jj+1] = hold1;
					
					double hold2 = speed[jj];
					speed[jj] = speed[jj+1];
					speed[jj+1] = hold2;
					
					// keep the max wind speed array index.
					if ( maxSpeedIdx == (jj + 1) ){
						maxSpeedIdx = jj;
					}
					else if ( maxSpeedIdx == jj ){
						maxSpeedIdx = jj + 1;
					}
				}
			}
		}
		
	/*	
		// add hashes from the start to the end
		for ( int ii = 1; ii < counter; ii++ ){
			addHash(aJet, lineCurvePts, speed[ii-1], speed[ii], index[ii-1], index[ii]);
		}
	*/
		
		// add hashes from the max wind to the jet start
		for ( int ii = maxSpeedIdx; ii > 0; ii-- ){
			addHash(aJet, lineCurvePts, speed[ii], speed[ii-1], index[ii], index[ii-1]);
		}
		
		// add hashes from the max wind to the jet end
		for ( int ii = maxSpeedIdx; ii < counter-1; ii++ ){
			addHash(aJet, lineCurvePts, speed[ii], speed[ii+1], index[ii], index[ii+1]);
		}
		
		hashTmp = null;
	}
	
	/**
	 * Add hashes between two input points.
	 * @param aJet
	 * @param lineCurvePts
	 * @param speed1
	 * @param speed2
	 * @param id1
	 * @param id2
	 */
	private void addHash(Jet aJet, double[][] lineCurvePts, 
			double speed1, double speed2, int id1, int id2){
		
		double delta = 20;
		if(speed1>speed2) delta = -20;
		
		double rate = (id2 - id1)/(speed2 - speed1);
		double speed = speed1;
		
		// add a hash every 20 KT
		while( Math.abs(speed2 - speed) > Math.abs(delta) ){
			speed += delta;
			int id = (int) ((speed - speed1)*rate) + id1;
			JetHash hash = addHashAtIndex(aJet, lineCurvePts, id);
			
			double mapDir = hash.getDirection();
			
			if ( hashTmp == null && jetDlg != null ){
				IAttribute hashAttr = jetDlg.getHashAttr();

				if ( hashAttr != null ){
					hash.update(hashAttr);
				}	
			}
			hash.setDirection(mapDir);
			
		}
	}
	
	/**
	 * Add hash at the input point on the jet
	 * @param aJet
	 * @param lineCurvePts
	 * @param locId
	 * @return
	 */
	private JetHash addHashAtIndex(Jet aJet, double[][] lineCurvePts, int locId ){

		double[] pix = descriptor.pixelToWorld(new double[]{lineCurvePts[locId][0],lineCurvePts[locId][1]});
		Coordinate loc = new Coordinate(pix[0],pix[1]);
		
		double dir  = getDirectionAtPoint( lineCurvePts, locId);
		
		pix = descriptor.pixelToWorld(new double[]{lineCurvePts[locId][0],lineCurvePts[locId][1]});
		Coordinate nearestPt = new Coordinate(pix[0], pix[1]);
		
		double mapDir = translateDir(dir, nearestPt.y, nearestPt.x, true);
	
		mapDir = 180- mapDir;
		if( mapDir < 0 ) mapDir = 360+mapDir;
		
		JetHash hash;
		if ( hashTmp != null ){
			hash = aJet.new JetHash(null, hashTmp.getColors(),
					hashTmp.getLineWidth(), hashTmp.getSizeScale(), 
					hashTmp.isClear(), loc, VectorType.HASH_MARK,
					100, mapDir, hashTmp.getArrowHeadSize(), 
					false, "Vector", "Hash");
		}
		else {
			hash = aJet.new JetHash(null, new Color[]{ new Color(0,255,0), new Color(255,0,0)},
				2.0f, 2.0, true, loc, VectorType.HASH_MARK,
				100, mapDir, 1.0, false, "Vector", "Hash");
		}
		
		aJet.add(hash);
		return hash;
	}

	@Override
	/**
	 * Snap the input hash on the jet
	 */
	public void snapHash(JetHash hash, Coordinate loc, Jet aJet){
		double[][] lineCurvePts = getJetCurve( aJet );
		double hashPt[] = descriptor.worldToPixel(new double[]{loc.x, loc.y});

		// get the nearest location on the jet curve
		int nearestId = getNearestPtOnCurve(lineCurvePts, hashPt );
		
		double[] nearestPix = descriptor.pixelToWorld( new double[]{lineCurvePts[nearestId][0],lineCurvePts[nearestId][1]});
		Coordinate nearestPt = new Coordinate(nearestPix[0], nearestPix[1]);
		
		// get the hash direction
		double dir  = getDirectionAtPoint( lineCurvePts, nearestId);
		double mapDir = translateDir(dir, nearestPt.y, nearestPt.x, true);
		mapDir = 180- mapDir;
		if( mapDir < 0 ); mapDir = 360+mapDir;
		
		hash.setLocationOnly(nearestPt);
		hash.setDirection(mapDir);
		checkHashOnJet(aJet);

	}
	
	private void snapAllHashs( Jet aJet ){
		
		Iterator<AbstractDrawableComponent> it = aJet.getComponentIterator();
		while(it.hasNext()){
			AbstractDrawableComponent adc =it.next();
			if ( adc instanceof JetHash ) {
				snapHash((JetHash)adc, ((JetHash) adc).getLocation(), aJet);
			}
		}
	}

	/**
	 * Set the map descriptor for jet tool
	 * @param mapDescriptor
	 */
	public void setMapDescriptor(IMapDescriptor mapDescriptor){
		this.descriptor = mapDescriptor;
	}


	@Override
	/**
	 * Transform radius/theta relative to the barb to lat/lon
	 */
	public Coordinate relative2LatLon(Coordinate relativeLoc, Vector barb) {
		
//		double scaleFactor = mapEditor.getSelectedPane().getZoomLevel() / defaultZoomLevel;
		double scaleFactor = mapEditor.getActiveDisplayPane().getZoomLevel() / defaultZoomLevel;
		
		double [] pt0 = descriptor.worldToPixel( new double[]{barb.getPoints().get(0).x, barb.getPoints().get(0).y});

		double mdir = barb.getDirection();
		double dir = translateDir(mdir, barb.getPoints().get(0).y, barb.getPoints().get(0).x, false);

		double deltaX = Math.sin((dir-relativeLoc.y)*Math.PI/180)*relativeLoc.x*scaleFactor;
		double deltaY = Math.cos((dir-relativeLoc.y)*Math.PI/180)*relativeLoc.x*scaleFactor;
		
		double[] pt1 = {pt0[0]+deltaX, pt0[1]-deltaY};
		
		Coordinate loc;
		
		double world[] = descriptor.pixelToWorld( pt1 );
		loc =  new Coordinate(world[0], world[1], world[2]);
		
		return loc;
	}


	@Override
	/**
	 * Transform lat/lon to radius/theta relative to the location of input barb
	 */
	public Coordinate latLon2Relative(Coordinate loc, Vector barb) {
		
		Coordinate polar = new Coordinate();

		// transform lat/lon to screen coordinates 
		
		double [] pt0 = descriptor.worldToPixel( new double[]{barb.getPoints().get(0).x, barb.getPoints().get(0).y});
		double [] pt1 = descriptor.worldToPixel( new double[]{loc.x, loc.y});

		
		double deltaX = pt1[0] - pt0[0];
		double deltaY = pt1[1] - pt0[1];
		
		//get radius in the polar coordinate system
		polar.x = Math.sqrt(deltaX*deltaX+deltaY*deltaY);
//		double scaleFactor = mapEditor.getSelectedPane().getZoomLevel() / defaultZoomLevel;
		double scaleFactor = mapEditor.getActiveDisplayPane().getZoomLevel() / defaultZoomLevel;

		polar.x /= scaleFactor;
		//get angular wind barb and flight text
		//use -deltaY because in screen coordinate the original point is at upper-left.
		double tmp = Math.atan2(-deltaY, deltaX) * 180 /Math.PI;
		tmp = 90 - tmp;
		if (tmp < 0) tmp += 360;

		// get the wind barb direction in screen coordinate system
		double dir = barb.getDirection();
		double sDir = translateDir(dir, barb.getPoints().get(0).y, barb.getPoints().get(0).x, false);

		// get theta in polar coordinate system
		polar.y = sDir - tmp; 
		if ( polar.y > 180 ) polar.y -= 360;

		//for SHEM to put text under the bar
		if ( loc.y < 0 && polar.y > 0 ) {polar.y *= -1; };
		
		return polar;
		
	}
	
	/**
	 * Calculate hash locations and put them on the jet. 
	 * @param aJet
	 * @param lineCurvePts
	 */
	@Override
	public java.util.Vector<Integer> checkHashOnJet(Jet aJet){
		
		 double[][] lineCurvePts = this.getJetCurve(aJet);
		java.util.Vector<Integer> ret = new java.util.Vector<Integer>();

		if ( aJet.size() < 2 ) return ret;
		
		double[] speed = new double[aJet.size()+1];
		int[] barbIdx = new int[aJet.size()+1];
		
		int[] hashIdx = new int[aJet.size()+1];
		for ( int ii = 0; ii < hashIdx.length; ii++ ){
			hashIdx[ii] = -1;
		}

		speed[0] = 80;
		barbIdx[0] = 0;
		speed[1] = 80;
		barbIdx[1] = lineCurvePts.length;
		
		int counter = 2;

		//get existing hash attributes from the jet
//		Iterator<DrawableElement> iterator = aJet.createDEIterator();
//		while(iterator.hasNext()){
//			DrawableElement de = iterator.next();
//			if ( de instanceof JetHash ){
//				hashTmp = (JetHash)de;
//				break;
//			}
//		}
		
//		aJet.removeAllHash();
		
		Iterator<DrawableElement> it = aJet.createDEIterator();
		int maxSpeedIdx = 0;
		double maxSpeed = -1;
		
		// get point index and speed of all barbs
		// also find the max wind speed and its array index.
		int kk = 0;
		while(it.hasNext()){
			DrawableElement adc = it.next();
			if ( adc instanceof Jet.JetBarb ){
				Coordinate adcLoc = ((Jet.JetBarb)adc).getLocation();
				double barbPt[] = descriptor.worldToPixel(new double[]{adcLoc.x, adcLoc.y});

				int nearestId = getNearestPtOnCurve(lineCurvePts, barbPt );
				barbIdx[counter] = nearestId;
				speed[counter] = ((Jet.JetBarb)adc).getSpeed();
				if ( maxSpeed < 0 || speed[counter] > maxSpeed ){
					maxSpeed = speed[counter];
					maxSpeedIdx = counter;
				}
				counter++;
			}
			else if ( adc instanceof Jet.JetHash ){
				Coordinate adcLoc = ((Jet.JetHash)adc).getLocation();
				double hashPt[] = descriptor.worldToPixel(new double[]{adcLoc.x, adcLoc.y});
				int nearestId = getNearestPtOnCurve(lineCurvePts, hashPt );
				
				hashIdx[kk++] = nearestId; 
				
			}
		}
		
		// sort the index and speed arrays by index
		for ( int ii = 0; ii < counter - 1; ii++ ){
			for ( int jj = 0; jj < counter -1; jj ++ ){
				if ( barbIdx[jj] > barbIdx[jj+1] ){
					int hold1 = barbIdx[jj];
					barbIdx[jj] = barbIdx[jj+1];
					barbIdx[jj+1] = hold1;
					
					double hold2 = speed[jj];
					speed[jj] = speed[jj+1];
					speed[jj+1] = hold2;
					
					// keep the max wind speed array index.
					if ( maxSpeedIdx == (jj + 1) ){
						maxSpeedIdx = jj;
					}
					else if ( maxSpeedIdx == jj ){
						maxSpeedIdx = jj + 1;
					}
				}
			}
		}
		
		//check each segment and if the number of hashes are correct
		for ( int ii = 0; ii < counter - 1; ii++ ){
			double deltaSpeed = Math.abs(speed[ ii + 1 ] - speed[ ii]);
			int hashesNeeded = (int)(deltaSpeed/20);
			if ( hashesNeeded > 0 && deltaSpeed%20 < 0.01 ) hashesNeeded--;
			
			int hashesOnJet = 0;
			for ( int jj = 0; jj < hashIdx.length; jj++ ){
				if ( hashIdx[jj] >= barbIdx[ii] && hashIdx[jj] < barbIdx[ii+1] ) hashesOnJet++;
			}
			
			ret.add( hashesNeeded - hashesOnJet );
			
		}
		
		return ret;
	}
	
	/**
	 * Add hashes and barbs that belong to jet2, but lie on jet1, into jet1.
	 * This method is used to delete a part of one jet.  
	 * @param jet1
	 * @param jet2
	 */
	@Override
	public void addBarbHashFromAnotherJet(Jet jet1, Jet jet2 ){
		if (jet2.size() <=1 ) return;

		double[][] newpts = this.getJetCurve( jet1 );
   		Coordinate[] coords = new Coordinate[ newpts.length ];
		
		for (int k=0; k<newpts.length; k++) {
			coords[k] = new Coordinate( newpts[k][0], newpts[k][1]);
		}
		LineString	ls = new GeometryFactory().createLineString( coords );
		LocationIndexedLine lil = new LocationIndexedLine(ls);
		
		Iterator<DrawableElement>  it = jet2.createDEIterator();
		
		while ( it.hasNext() ){
			DrawableElement adc = it.next();
			
			if ( adc instanceof Jet.JetBarb ||
					adc instanceof Jet.JetHash ){

				double[] loc = {((SinglePointElement)adc).getLocation().x, ((SinglePointElement)adc).getLocation().y };
				double[] pix = descriptor.worldToPixel( loc );
				Coordinate screenPt = new Coordinate(pix[0], pix[1]);
				LinearLocation linloc = lil.project(screenPt);
				Coordinate screen2 = lil.extractPoint(linloc);
				
				if ( screenPt.distance(screen2) < 100 ){
					if ( adc instanceof Jet.JetBarb ){
						//add its parent windInfo which include the flight level text
						jet1.add(adc.getParent().copy());
					}
					else {
						jet1.add(adc.copy());
					}
				}
			}
		}
	}
}
