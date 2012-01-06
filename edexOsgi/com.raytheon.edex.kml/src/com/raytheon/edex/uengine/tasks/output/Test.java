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
package com.raytheon.edex.uengine.tasks.output;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.google.earth.kml._2.ContainerType;
import com.google.earth.kml._2.DocumentType;
import com.google.earth.kml._2.FeatureType;
import com.google.earth.kml._2.GroundOverlayType;
import com.google.earth.kml._2.KmlType;
import com.google.earth.kml._2.LatLonBoxType;
import com.google.earth.kml._2.LinkType;
import com.google.earth.kml._2.ObjectFactory;

public class Test {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// KmlFile kmlFile = new KmlFile();
		ArrayList<FeatureType> featureList = new ArrayList<FeatureType>();

		File imageFile = new File("/home/ebabin/Desktop/test.png");

		LatLonBoxType llBox = new LatLonBoxType();
		// llBox.setNorth(54.00000000000001);
		// llBox.setSouth(22.417968750000007);
		// llBox.setEast(150.0);
		// llBox.setWest(120.0);
		llBox.setNorth(55.78508836640289);
		llBox.setSouth(16.280999999999086);
		llBox.setEast(-68.73732503893913);
		llBox.setWest(-126.13799999999999);

		GroundOverlayType overlay = new GroundOverlayType();
		overlay.setLatLonBox(llBox);

		LinkType lt = new LinkType();
		lt.setHref(imageFile.getAbsolutePath());
		overlay.setIcon(lt);

		featureList.add(overlay);

		DocumentType container = new DocumentType();

		List<JAXBElement<? extends FeatureType>> features = container
				.getFeature();
		ObjectFactory factory = new ObjectFactory();
		JAXBElement<? extends FeatureType> element = (JAXBElement<? extends FeatureType>) factory
				.createGroundOverlay(overlay);
		features.add(element);
		ArrayList<ContainerType> containList = new ArrayList<ContainerType>();
		containList.add(container);

		JAXBContext context;
		try {
			context = JAXBContext.newInstance(KmlType.class);

			KmlType kml = new KmlType();
			kml.setFeature(factory.createDocument(container));

			Marshaller marshaller = context.createMarshaller();
			try {
				PrintWriter writer = new PrintWriter(new File(
						"/home/ebabin/Desktop/test12.kml"));
				// marshaller.marshal(kml, System.out);
				marshaller.marshal(kml, writer);
				writer.close();
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
