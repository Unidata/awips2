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
package com.raytheon.uf.common.dataplugin.radar.level3;

import java.io.DataInputStream;
import java.io.IOException;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Hail probable uses the same decoder as TVS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 19, 2009           mnash       Initial creation
 * Jul 29, 2013  2148     mnash       Refactor registering of packets to Spring
 * Jun 04, 2014  3232     bsteffen    Add constructors.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@DynamicSerialize
public class HailProbablePacket extends HailPositivePacket {

    public HailProbablePacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public HailProbablePacket() {
        super();
    }

}