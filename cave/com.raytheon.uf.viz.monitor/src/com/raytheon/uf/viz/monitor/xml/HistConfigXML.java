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
package com.raytheon.uf.viz.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Class containing the XML data specifying options in the ObsHistConfigDlg.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         	Ticket#    	Engineer    	Description
 * ------------ ---------- ----------- --------------------------
 * Feb. 18, 2010 	#43      	skorolev     	Initial creation
 *
 * </pre>
 *
 * @author skorolev
 * @version 1.0
 */
@XmlRootElement(name = "visiableColums")
@XmlAccessorType(XmlAccessType.NONE)
public class HistConfigXML implements ISerializableObject {
		@XmlElementWrapper(name = "metars")
    	private boolean[] metar = new boolean[0];
    	
    	@XmlElementWrapper(name = "maritimes")  
    	private boolean[] maritime = new boolean[0];

		public void setMetar(boolean[] metar) {
			this.metar = metar;
		}

		public boolean[] getMetar() {
			return metar;
		}

		public void setMaritime(boolean[] maritime) {
			this.maritime = maritime;
		}

		public boolean[] getMaritime() {
			return maritime;
		}
}
