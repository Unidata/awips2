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
package com.raytheon.uf.common.plugin.nwsauth.user;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Implementation of IUserId
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2010            rgeorge     Initial creation
 * Oct 06, 2014 3398       bclement    moved to common.auth
 *                                      left extension for backwards compatibility
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 * @deprecated use com.raytheon.uf.common.auth.user.UserId
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@Deprecated
public class UserId extends com.raytheon.uf.common.auth.user.UserId {

}
