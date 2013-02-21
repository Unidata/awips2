package com.raytheon.uf.edex.ebxml.exception;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

public class QueryException extends RegistryException {

	private static final long serialVersionUID = -3278994859030039974L;

	public QueryException(RegistryExceptionType registryException) {
		super(registryException);
	}

	public QueryException(String message, RegistryException e) {
		super(message,e);
	}
}
