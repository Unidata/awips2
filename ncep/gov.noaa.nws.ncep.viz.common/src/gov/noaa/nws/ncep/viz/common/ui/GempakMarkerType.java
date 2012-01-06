package gov.noaa.nws.ncep.viz.common.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedHashMap;

public class GempakMarkerType {

	public final static LinkedHashMap<Integer, String> loadMarkerTypes( String fileName ) throws IOException {
		LinkedHashMap<Integer, String> markerTypeMaps = new LinkedHashMap<Integer, String>();

		BufferedReader input = new BufferedReader(new FileReader(new File(fileName)));
		String lineStr = null;

		while( (lineStr=input.readLine()) != null ) {
			if( lineStr.startsWith("!") ) {
				continue;
			}

			String[] lv = lineStr.trim().split("\\s+");
			if (lv.length >= 2) {
				int markerType = Integer.valueOf(lv[0].trim());
				String markerName = lv[1].trim();
				
				markerTypeMaps.put(markerType, markerName);
			}

		} 	

		return markerTypeMaps;
	}
}
