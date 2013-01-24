package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.adapters.XmlAdapter;

public class ProjectionAdapter extends
        XmlAdapter<GriddedProjection, Projection> {

    @Override
    public Projection unmarshal(GriddedProjection v) throws Exception {
        return v;
    }

    @Override
    public GriddedProjection marshal(Projection v) throws Exception {
        return new GriddedProjection(v); // you must provide such c-tor
    }

}
