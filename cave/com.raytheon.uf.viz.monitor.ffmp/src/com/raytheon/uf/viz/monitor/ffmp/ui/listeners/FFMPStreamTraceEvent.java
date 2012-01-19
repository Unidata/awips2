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
package com.raytheon.uf.viz.monitor.ffmp.ui.listeners;

import java.util.EventObject;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;

/**
* 
* Stream Trace event
* 
* <pre>
* SOFTWARE HISTORY
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* Aug 06, 2009 2521       dhladky     Initial creation
* </pre>
* 
* @author dhladky
* @version 1.0
*
*/

public class FFMPStreamTraceEvent extends EventObject {

   /**
    * 
    */
   private static final long serialVersionUID = 18982020945418L;
   
   public FFMPStreamTraceEvent(FFMPRecord.CLICK_TYPE stream) {
      super(stream);
   }
}