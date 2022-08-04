/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.edex.nswrctools;

import java.util.HashMap;

import ucar.nc2.Dimension;

/**
 * Storage place for NextGen Surveillance and Weather Radar Capability (NSWRC) data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */
public class NSWRCVariable {

    private String variableName = null;

    private HashMap<Dimension, Object> dimensionDataObjects;

    public NSWRCVariable() {
        dimensionDataObjects = new HashMap<Dimension, Object>();
    }

    public void addDataObject(Dimension dimension, Object data) {
        dimensionDataObjects.put(dimension, data);
    }

    public Object getDataObject(Dimension dimension) {
        return dimensionDataObjects.get(dimension);
    }

    public void setVariableName(String variableName) {
        this.variableName = variableName;
    }

    public String getVariableName() {
        return variableName;
    }
}
