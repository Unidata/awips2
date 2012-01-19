/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.rcm.config;

/* Notion of a radar's type was never really well defined.  It does
 * not seem like a good idea for general code to be dependent on radar
 * type.  The details are hidden away in the configuration system. Both
 * the awips1 and std packages now use RadarType, so the definitions are
 * here and public, but they should not be used outside of config.*
 */
public enum RadarType {
	WSR, TDWR, ASR, ARSR
}
