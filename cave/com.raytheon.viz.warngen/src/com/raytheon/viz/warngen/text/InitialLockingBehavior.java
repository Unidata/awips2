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
package com.raytheon.viz.warngen.text;

/**
 * Locks text patterns on initial or new warning.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012     15322  jsanchez     Initial creation
 * Jan  8, 2013     15664  Qinglu Lin   Removed the following methods (and moved them to AbstractLockingBehavior):
 *                                      body(), bulletIndices(), header(), firstBullet(), secondBullet(), getImmediateCausesPtrn();

 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class InitialLockingBehavior extends AbstractLockingBehavior {
}
