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
/**
 * Provides classes for managing files and paths based on localization settings 
 * 
 * <p>Classes provided in this package are used by both EDEX and CAVE.  The 
 * {@link com.raytheon.uf.common.localization.PathManagerFactory} obtains the correct 
 * {@link com.raytheon.uf.common.localization.IPathManager} object depending on if the 
 * code is executing from the EDEX server or CAVE client. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 27, 2007             njensen     Initial creation
 * Jul 11, 2008 1250        jelkins     Implement ILocalizationAdapter for EDEX
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 * 
 */
package com.raytheon.edex.utility;

