package gov.noaa.nws.ncep.viz.common;

import java.util.ArrayList;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.eclipse.swt.graphics.RGB; 


public class IntegerListAdapter extends XmlAdapter<String, ArrayList<Integer>> {
	@Override
	public String marshal(ArrayList<Integer> v) throws Exception {
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
	public ArrayList<Integer> unmarshal(String v) throws Exception {
		ArrayList<Integer> intlist = new ArrayList<Integer>();
		if( v != null && !v.isEmpty() && !v.equals("[]") ) {
			String istrs[] = v.split(",");
			for( int i=0 ; i<istrs.length ; i++ ) {
				intlist.add( Integer.parseInt( istrs[i] ) );
			}
		}

		return intlist;
	}
}
