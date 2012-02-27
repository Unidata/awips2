package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.eclipse.swt.graphics.RGB; 

// TODO ; 
public class RscParamsJaxBAdapter extends XmlAdapter<String, HashMap<String, String>> {
	@Override
	public String marshal( HashMap<String, String> rscPrms) throws Exception {
		if( rscPrms.isEmpty() ) {
			return "";
		}

		StringBuffer strBuf = new StringBuffer("\n");
		
		for( String prmName : rscPrms.keySet() ) {
			prmName = prmName.trim();
			
			if( prmName.startsWith("!") ) {
				strBuf.append( prmName +"\n");
			}
			else {
				strBuf.append( prmName+"="+ rscPrms.get( prmName ).trim() + "\n" );		
			}
		}
		strBuf.append( "\n" );
		return strBuf.toString();			
	}

	// assume that the input string is from the marshal method
	@Override
	public HashMap<String, String> unmarshal( String prmsStr ) throws Exception {
		HashMap<String, String>  rscPrms = new HashMap<String, String>();
		String[] prmStrs = prmsStr.split("\n");
		
		for( String prmStr : prmStrs ) {
			if( prmStr == null || prmStr.trim().isEmpty() ) {
				continue;
			}
			prmStr = prmStr.trim();
			
			if( prmStr.startsWith( "!" ) ) {
				rscPrms.put( prmStr, prmStr );
			}
			else {
				int equalsIndx = prmStr.indexOf("=");
				if( equalsIndx == -1 ) {
					System.out.println("Error unmarshalling resource Parameters( no '=' found\n"+prmStr );
					continue;
				}
				String prmName = prmStr.substring(0, equalsIndx );					
				String prmVal = prmStr.substring( equalsIndx+1, prmStr.length() );
				rscPrms.put( prmName.trim(), prmVal.trim() );
			}
		}

		return rscPrms;
	}
}