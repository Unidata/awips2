package gov.noaa.nws.ncep.gempak.parameters.marker;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.common.ui.GempakMarkerType;

import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;

import org.eclipse.swt.graphics.RGB;


/**
 *   MARKER specifies the marker color, type, size, line width separated by slashes:
 *
 *               marker color / marker type / size / width
 *
 *    If the marker color is 0, no markers will be drawn.  If the marker
 *    color is not specified, a default of 1 will be used.
 *
 *    The marker type specifies the shape of the marker to be drawn.  If
 *    the type is unspecified or zero, the current marker type (usually 1)
 *    will be used.  The software marker types are:
 *
 *        1      plus sign               12      asterisk
 *        2      octagon                 13      hourglass X
 *        3      triangle                14      star
 *        4      box                     15      dot
 *        5      small X                 16      large X
 *        6      diamond                 17      filled octagon
 *        7      up arrow                18      filled triangle
 *        8      X with top bar          19      filled box
 *        9      Z with bar              20      filled diamond
 *       10      Y                       21      filled star
 *       11      box with diagonals      22      minus sign
 * 
 *    The marker size is a real number multiplier for the default marker
 *    size.  If the size is zero or unspecified, the current size will be
 *    used.
 *
 * <p>
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * July 2010                M. Li       Initial Creation
 *                                         
 * </pre>
 * @author mli
 * @version 1
 * @see $GEMPAK/help/hlx/marker.hl2 
 */

public class MARKER {

	// Marker Name
	private String markerName = "PLUS_SIGN";
	
	// Marker Color
	private RGB markerColor = new RGB(255, 0, 0);
	
    // Marker Size	
	private double markerSize = 1.0;
	
	// Marker Width
	private float markerWidth = 1.0f;
	

	public MARKER(String markerAttributeString) {
		parseMarkerAttributes(markerAttributeString);
	}

	private void parseMarkerAttributes(String markerAttributeString) {
		if (markerAttributeString == null || markerAttributeString.trim().length() <= 0) return;
		
		LinkedHashMap<Integer, String> markerTypeMaps = null;
		File markerTypeTable = 
    		NcPathManager.getInstance().getStaticFile(
    				NcPathConstants.GEMPAK_MARKER_TYPE );
		
        try {
			markerTypeMaps = GempakMarkerType.loadMarkerTypes(markerTypeTable.getAbsolutePath() );
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		String[] markerAttrs = markerAttributeString.trim().split("/");
		if (markerAttrs.length >= 1 && markerAttrs[0] != null) {
    		markerColor = GempakColor.convertToRGB(Integer.valueOf(markerAttrs[0].trim()));
    	}
    	if (markerAttrs.length >= 2 && markerAttrs[1] != null && markerTypeMaps != null) {
    		markerName = markerTypeMaps.get(Integer.valueOf(markerAttrs[1].trim()));
    	}
    	if (markerAttrs.length >= 3 && markerAttrs[2] != null) {
    		markerSize = Double.valueOf(markerAttrs[2].trim());
    	}
    	if (markerAttrs.length >= 4 && markerAttrs[3] != null) {
    		markerWidth = Float.valueOf(markerAttrs[3].trim());
    	}
	}

	public String getMarkerName() {
		return markerName;
	}

	public RGB getMarkerColor() {
		return markerColor;
	}

	public double getMarkerSize() {
		return markerSize;
	}

	public float getMarkerWidth() {
		return markerWidth;
	}
	
	
}
