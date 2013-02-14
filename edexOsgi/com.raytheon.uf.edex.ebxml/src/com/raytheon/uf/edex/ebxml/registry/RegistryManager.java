package com.raytheon.uf.edex.ebxml.registry;

import com.raytheon.uf.edex.ebxml.registry.memory.MemoryRegistery;

public class RegistryManager {
	/** The singleton {@link IRegistry} instance. */
	protected static IRegistry registry = null;

	/**
	 * If not already initialized, this retrieves an {@link IRegistry} instance
	 * based on a configuration file. The object must have a default constructor
	 * and will be initialized with {@link IRegistry#init()}.
	 * 
	 * @return The {@link IRegistry} instance.
	 */
	public static synchronized IRegistry getRegistryInstance() {
	    
	    if (registry == null){
	        registry = new MemoryRegistery();
	    }
	    return registry;
	}

	/**
	 * Throw an {@link IllegalStateException} using the given error.
	 * 
	 * @param e
	 *            an {@link Exception}.
	 */
	private static void error(Exception e) {
		throw new IllegalStateException("Could not initialize registry", e);
	}
}
