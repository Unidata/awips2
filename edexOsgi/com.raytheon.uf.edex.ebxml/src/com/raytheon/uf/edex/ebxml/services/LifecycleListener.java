/*
* The following software products were developed by Raytheon:
*
* ADE (AWIPS Development Environment) software
* CAVE (Common AWIPS Visualization Environment) software
* EDEX (Environmental Data Exchange) software
* uFrame™ (Universal Framework) software
*
* Copyright (c) 2010 Raytheon Co.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/org/documents/epl-v10.php
*
*
* Contractor Name: Raytheon Company
* Contractor Address:
* 6825 Pine Street, Suite 340
* Mail Stop B8
* Omaha, NE 68106
* 402.291.0100
*
*
* SOFTWARE HISTORY
*
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* Apr 19, 2011            jsherida     Initial creation
*
*/ 
package com.raytheon.uf.edex.ebxml.services;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Interface for listening to lifecycle events.
 *
 * @author jsherida
 * @version 1.0	
 */
public interface LifecycleListener {
    
    /**
     * Notify the listener that one or more objects have been sumbitted.
     * @param objs The list of objects that were submitted.
     */
    public void objectsSubmitted(List<RegistryObjectType> objs);
    
    /**
     * Notify the listener that one or more objects have been updated.
     * @param refs The list of references to objects that were updated 
     */
    public void objectsUpdated(List<ObjectRefType> refs);

    /**
     * Notify the listener that one or more objects have been removed.
     * @param refs The list of references to objects that were removed.
     */
    public void objectsRemoved(List<ObjectRefType> refs);
}
