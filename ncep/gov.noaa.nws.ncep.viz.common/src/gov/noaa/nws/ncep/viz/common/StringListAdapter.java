package gov.noaa.nws.ncep.viz.common;

import java.util.ArrayList;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.eclipse.swt.graphics.RGB; 



public class StringListAdapter extends XmlAdapter<String, ArrayList<String>> {
	@Override
	public String marshal(ArrayList<String> v) throws Exception {
		if( v.isEmpty() ) {
			return new String("");
		}

		StringBuffer strb = new StringBuffer(v.get(0).toString());

		for( int i=1 ; i<v.size() ; i++ ) {
			strb.append( ","+v.get(i).toString());
		}
		return strb.toString();			
	}

	// assume that the input string is from the marshal method
	@Override
	public ArrayList<String> unmarshal(String v) throws Exception {
		ArrayList<String> strlist = new ArrayList<String>();
		if( v != null && !v.isEmpty() && !v.equals("[]") ) {
			String istrs[] = v.split(",");
			for( int i=0 ; i<istrs.length ; i++ ) {
				strlist.add( istrs[i].trim() );
			}
		}

		return strlist;
	}
}