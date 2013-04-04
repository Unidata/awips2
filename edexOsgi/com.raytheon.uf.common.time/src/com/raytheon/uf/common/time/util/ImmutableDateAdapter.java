package com.raytheon.uf.common.time.util;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * An {@link XmlAdapter} version that allows JABX marshaling/unmarshaling
 * of {@link ImmutableDate}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2012 726        jspinks     Initial creation
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class ImmutableDateAdapter extends XmlAdapter<Date, ImmutableDate> {

	@Override
	public ImmutableDate unmarshal(Date date) throws Exception {
		return new ImmutableDate(date);
	}

	/**
	 * 
	 * @return 
	 */
	@Override
	public Date marshal(ImmutableDate immutableDate) throws Exception {
		return immutableDate;
	}
}
