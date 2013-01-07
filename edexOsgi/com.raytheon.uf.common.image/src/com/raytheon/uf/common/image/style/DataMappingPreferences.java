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
package com.raytheon.uf.common.image.style;

import java.util.ArrayList;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.image.units.PiecewisePixel;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataMappingPreferences {

	@XmlAccessorType(XmlAccessType.NONE)
	public static class DataMappingEntry {
		@XmlAttribute
		private Double displayValue;

		@XmlAttribute
		private Double pixelValue;

		@XmlAttribute
		private String label;

		@XmlAttribute
		private String sample;

		/**
		 * @return the displayValue
		 */
		public Double getDisplayValue() {
			return displayValue;
		}

		/**
		 * @param displayValue
		 *            the displayValue to set
		 */
		public void setDisplayValue(Double displayValue) {
			this.displayValue = displayValue;
		}

		/**
		 * @return the pixelValue
		 */
		public Double getPixelValue() {
			return pixelValue;
		}

		/**
		 * @param pixelValue
		 *            the pixelValue to set
		 */
		public void setPixelValue(Double pixelValue) {
			this.pixelValue = pixelValue;
		}

		/**
		 * @return the label
		 */
		public String getLabel() {
			return label;
		}

		/**
		 * @param label
		 *            the label to set
		 */
		public void setLabel(String label) {
			this.label = label;
		}

		/**
		 * @return the sample
		 */
		public String getSample() {
			return sample;
		}

		/**
		 * @param sample
		 *            the sample to set
		 */
		public void setSample(String sample) {
			this.sample = sample;
		}

	}

	@XmlElement(name = "entry")
	private final ArrayList<DataMappingEntry> entries = new ArrayList<DataMappingEntry>();

	private Unit<?> imageUnit;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Unit<?> getImageUnit(Unit<?> displayUnit) {
		if (imageUnit == null) {
			ArrayList<Double> pixelValues = new ArrayList<Double>();
			ArrayList<Double> dispValues = new ArrayList<Double>();
			for (DataMappingEntry entry : entries) {
				if (entry.getPixelValue() != null
						&& entry.getDisplayValue() != null) {
					pixelValues.add(entry.getPixelValue());
					dispValues.add(entry.getDisplayValue());
				}
			}

			int n = pixelValues.size();
			if (n > 1) {
				double[] pix = new double[n];
				double[] disp = new double[n];

				for (int i = 0; i < n; i++) {
					pix[i] = pixelValues.get(i);
					disp[i] = dispValues.get(i);
				}

				imageUnit = new PiecewisePixel(displayUnit, pix, disp);
			}
		}
		return imageUnit;
	}

	/**
	 * @return the entries
	 */
	public ArrayList<DataMappingEntry> getEntries() {
		return entries;
	}

	public void addEntry(DataMappingEntry entry) {
		entries.add(entry);
	}

}
