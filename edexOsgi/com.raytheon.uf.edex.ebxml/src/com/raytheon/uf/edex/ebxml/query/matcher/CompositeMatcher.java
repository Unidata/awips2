package com.raytheon.uf.edex.ebxml.query.matcher;

import java.util.ArrayList;
import java.util.List;

public abstract class CompositeMatcher implements IMatcher {

	protected final List<IMatcher> matchers = new ArrayList<IMatcher>();

	public void add(IMatcher matcher) {
		matchers.add(matcher);
	}
}
