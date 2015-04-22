package gov.noaa.nws.ncep.ui.nsharp;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpConfigHashMapAdaptor
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/21/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import java.util.HashMap;
import java.util.Map.Entry;

import javax.xml.bind.annotation.adapters.XmlAdapter;


public class NsharpConfigHashMapAdaptor extends
    XmlAdapter<NsharpLinePropertySerializable, HashMap<String, NsharpLineProperty>> {

/*
 * (non-Javadoc)
 * 
 * @see
 * javax.xml.bind.annotation.adapters.XmlAdapter#marshal(java.lang.Object)
 */
	@Override
	public NsharpLinePropertySerializable marshal(
			HashMap<String, NsharpLineProperty> v) throws Exception {
		if (v == null) {
			return null;
		}
		NsharpLinePropertySerializable serializable = new NsharpLinePropertySerializable();
		NsharpLinePropertySerializable.LinePropertyItem[] items = new NsharpLinePropertySerializable.LinePropertyItem[v
		                                                                                                              .size()];
		int i = 0;
		for (Entry<String, NsharpLineProperty> entry : v.entrySet()) {
			items[i] = new NsharpLinePropertySerializable.LinePropertyItem();
			items[i].lineName = entry.getKey();
			items[i].lineProperty = entry.getValue();
			i++;
		}
		serializable.items = items;
		return serializable;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.xml.bind.annotation.adapters.XmlAdapter#unmarshal(java.lang.Object)
	 */
	@Override
	public HashMap<String, NsharpLineProperty> unmarshal(
			NsharpLinePropertySerializable v) throws Exception {
		HashMap<String, NsharpLineProperty> map = null;
		if (v.items == null) {
			map = new HashMap<String, NsharpLineProperty>(0);
		} else {
			map = new HashMap<String, NsharpLineProperty>(v.items.length);
			for (NsharpLinePropertySerializable.LinePropertyItem item : v.items) {
				map.put(item.lineName, item.lineProperty);
			}
		}

		return map;
	}
}
