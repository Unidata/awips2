package com.raytheon.uf.edex.ebxml.query.matcher;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

public class AndMatcher extends CompositeMatcher {

	@Override
	public boolean matches(RegistryObjectType obj) {
		boolean isMatch = (matchers.size() > 0);
		for (IMatcher matcher : matchers) {
			isMatch = isMatch && matcher.matches(obj);
		}
		return isMatch;
	}

}
