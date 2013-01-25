package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.adapters.XmlAdapter;

public class CoverageAdapter extends XmlAdapter<GriddedCoverage, Coverage> {

    @Override
    public Coverage unmarshal(GriddedCoverage v) throws Exception {
        return v;
    }

    @Override
    public GriddedCoverage marshal(Coverage v) throws Exception {
        return new GriddedCoverage(v); // you must provide such c-tor
    }

}
