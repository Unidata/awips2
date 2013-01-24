package com.raytheon.uf.edex.ebxml.exception;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.edex.ebxml.util.EbxmlUtil;

public class RegistryException extends Exception {

	private static final long serialVersionUID = -7413493987092217686L;

	private RegistryExceptionType registryException;

	public RegistryException(RegistryExceptionType registryException) {
		super(EbxmlUtil.translateException(registryException));
	}

	public RegistryException(String message, RegistryException e) {
		super(message, e);
	}

	public RegistryExceptionType getRegistryException() {
		return registryException;
	}
}
