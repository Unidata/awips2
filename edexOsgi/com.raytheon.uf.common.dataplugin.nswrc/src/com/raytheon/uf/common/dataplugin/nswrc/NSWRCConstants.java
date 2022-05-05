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
package com.raytheon.uf.common.dataplugin.nswrc;

/**
 *
 * Constants to be used for NextGen Surveillance and Weather 
 * Radar Capability (NSWRC) data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 13, 2014            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */
public class NSWRCConstants {

	public static final String NSWRC_RADIAL = "nswrc_radial";
	
	public static final String NSWRC_GRIDDED = "nswrc_";
	
    public static final float NOISE_THRESHOLD = 1.0f;

    public static final float POWER_THRESHOLD = 0.3f;

    public static final float FILL_VALUE = -99900.0f;

    public static final String NOISE_TITLE = "SNRData";

    public static final String POWER_TITLE = "NCPData";

}
