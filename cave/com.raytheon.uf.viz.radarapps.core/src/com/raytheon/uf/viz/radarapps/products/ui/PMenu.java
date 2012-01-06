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
package com.raytheon.uf.viz.radarapps.products.ui;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.rcm.products.Usage;


@XmlRootElement(name="menu")
@XmlType(propOrder={})
@XmlSeeAlso({PMenu.PProductItem.class, PMenu.PSeparator.class,
	PMenu.PSubMenu.class})
public class PMenu {
	@XmlElementRef
	public PMenuItem[] items;
	
	private static JAXBContext context;
	private static Unmarshaller u;
	static {
		try {
			context = JAXBContext.newInstance(PMenu.class, 
					PSeparator.class, PProductItem.class, PSubMenu.class);
			u = context.createUnmarshaller();
		} catch (JAXBException e) {
			// TODO: ...
		}
	}
	
	public static Unmarshaller getUnmarshaller() {
		return u;
	}

	@XmlType(propOrder={})
	public abstract static class PMenuItem {
		@XmlAttribute
		public String name;
		@XmlAttribute(name="use")
		@XmlList
		public Usage[] use;
	}

	@XmlRootElement(name="sep")
	public static class PSeparator extends PMenuItem {
		// nothing
	}

	@XmlRootElement(name="item")
	@XmlType(propOrder={})
	public static class PProductItem extends PMenuItem {
		@XmlAttribute(name="mne")
		public String mnemonicReference;
	}

	@XmlRootElement(name="submenu")
	@XmlType(propOrder={})
	public static class PSubMenu extends PMenuItem {
		@XmlElement(required=true)
		PMenu menu;
	}
}
