/*
 * gov.noaa.nws.ncep.ui.pgen.element.ISnapJet
 * 
 * 23 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.elements.Jet.JetHash;

/**
 * Interface to snap jet.
 * Use this interface to avoid map editors in the element package
 * @author bingfan
 *
 */
public interface IJetTools {
	
	public void snapJet(Jet aJet);
	public void snapJet(AbstractDrawableComponent adc, Jet aJet);
	public void snapHash(JetHash hash, Coordinate loc, Jet aJet);
	public Coordinate latLon2Relative( Coordinate loc, Vector barb);
	public Coordinate relative2LatLon( Coordinate relative, Vector barb);
	public java.util.Vector<Integer> checkHashOnJet(Jet aJet);
	//For 'DelPart'
	public void addBarbHashFromAnotherJet(Jet jet1, Jet jet2);

}
