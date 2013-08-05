package com.raytheon.uf.edex.registry.ebxml.dao;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DynamicObjectRefType;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;

public class DynamicObjectRefDao extends
        SessionManagedDao<String, DynamicObjectRefType> {

    @Override
    protected Class<DynamicObjectRefType> getEntityClass() {
        return DynamicObjectRefType.class;
    }
}
