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
package com.raytheon.uf.edex.plugin.acars.common;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2009       1939 jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface ACARSConstants {

    // BUFR Table 0-20-041
    int NO_ICING      =  0;
    int LGT_ICE       =  1; // Light icing
    int LGT_ICE_IC    =  2; // Light icing in cloud
    int LGT_ICE_PR    =  3; // Light icing in precipitation

    int MDT_ICE       =  4; // Moderate icing
    int MDT_ICE_IC    =  5; // Moderate icing in cloud
    int MDT_ICE_PR    =  6; // Moderate icing in precipitation

    int SVR_ICE       =  7; // Severe Icing
    int SVR_ICE_IC    =  8; // Severe icing in cloud
    int SVR_ICE_PR    =  9; // Severe icing in precipitation

    int TRC_ICE       = 10; // Trace icing    
    int TRC_ICE_IC    = 11; // Trace icing in cloud
    int TRC_ICE_PR    = 12; // Trace icing in precipitation

    int RESERVE_13    = 13; // 13 Reserved
    int RESERVE_14    = 14; // 14 Reserved
    int MSG_ICE       = 15; // Missing value

    
    
    
    
}
