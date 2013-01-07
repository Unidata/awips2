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
package com.raytheon.uf.common.image.colormap;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.colormap.AbstractColorMap;
import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.image.style.DataMappingPreferences;
import com.raytheon.uf.common.image.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Colormap Parameters
 * 
 * Contains the parameters necessary to apply a colormap to a datatype
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 24, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ColorMapParameters implements Cloneable, ISerializableObject {

	protected IColorMapParametersListener listener;

	/** Units of the colormap parameters (min/max) */
	protected Unit<?> displayUnit;

	/** Units of the image pixel values */
	protected Unit<?> imageUnit;

	/** Units of the data values */
	protected Unit<?> dataUnit;

	/** The maximum value used to apply the colormap */
	protected float colorMapMax;

	/** The minimum value used to apply the colormap */
	protected float colorMapMin;

	/** The maximum (usually theoretical) value of the data */
	protected float dataMax;

	/** The minimum (usually theoretical) value of the data */
	protected float dataMin;

	/** The intervals upon which to apply labeling to the color bar */
	protected float[] colorBarIntervals;

	protected boolean logarithmic = false;

	protected ArrayList<LabelEntry> labels = new ArrayList<LabelEntry>();

	/** The colormap (ramp of colors) to use */
	protected IColorMap colorMap;

	/** The name of the colormap */
	@XmlAttribute
	protected String colorMapName;

	/** The converter that converts data values to display values * */
	protected UnitConverter dataToDisplayConverter;

	/** The converter that converts display values to data values * */
	protected UnitConverter displayToDataConverter;

	/** The converter that converts data values to image pixels */
	protected UnitConverter dataToImageConverter;

	/** The converter that converts image pixels to data values */
	protected UnitConverter imageToDataConverter;

	/** The converter that converts image pixels to display values */
	protected UnitConverter imageToDisplayConverter;

	/** The converter that converts display values to image pixels */
	protected UnitConverter displayToImageConverter;

	protected DataMappingPreferences dataMapping;

	protected boolean recomputeLabels = true;

	private String formatString = "0.###";

	private boolean dirty = false;

	private byte[] alphaMask = new byte[0];

	private boolean useMask = false;

	private boolean mirror;

	public static class LabelEntry {
		private float location;

		private String text;

		public float getLocation() {
			return location;
		}

		public String getText() {
			return text;
		}

		/**
		 * Sets a string label and it's location within the color bar.
		 * 
		 * @param label
		 *            String to be displayed
		 * @param location
		 *            Location on the color bar for the label in the range 0 to
		 *            1 where 0 is the far left of the color bar and 1 is the
		 *            far right
		 */
		public LabelEntry(String label, float location) {
			this.text = label;
			this.location = location;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + Float.floatToIntBits(location);
			result = prime * result + ((text == null) ? 0 : text.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			LabelEntry other = (LabelEntry) obj;
			if (Float.floatToIntBits(location) != Float
					.floatToIntBits(other.location))
				return false;
			if (text == null) {
				if (other.text != null)
					return false;
			} else if (!text.equals(other.text))
				return false;
			return true;
		}

	}

	private UnitConverter constructConverter(Unit<?> from, Unit<?> to) {
		UnitConverter converter = null;

		if ((from != null) && (to != null) && (to.isCompatible(from))) {
			converter = from.getConverterTo(to);
		}

		return converter;
	}

	private void addLabel(float dispValue, String s) {
		float index = getIndexByValue(dispValue);
		if (index > 1.0 || index < 0.0) {
			return;
		}

		labels.add(new LabelEntry(s, index));
	}

	private void addLabel(int pixelValue, String s) {
		float location = (float) pixelValue / 255;
		if (location > 1.0 || location < 0.0) {
			return;
		}

		labels.add(new LabelEntry(s, location));
	}

	private void computeLabels() {

		labels.clear();

		DecimalFormat format = new DecimalFormat(formatString);
		if (dataMapping != null) {
			for (DataMappingEntry entry : dataMapping.getEntries()) {
				String s = entry.getLabel();
				if (entry.getDisplayValue() != null) {
					float dispValue = entry.getDisplayValue().floatValue();
					if (s == null) {
						s = format.format(dispValue);
					}
					addLabel(dispValue, s);
				} else if (entry.getPixelValue() != null) {
					double pixelValue = entry.getPixelValue();
					addLabel((int) pixelValue, s);
				}
			}
		} else if (colorBarIntervals != null) {
			float colorMapRange = colorMapMax - colorMapMin;
			if (colorMapRange != 0.0) {
				for (float label : colorBarIntervals) {
					String s = format.format(label);
					addLabel(label, s);
				}
			}
		}
		recomputeLabels = false;
	}

	@XmlElement
	public AbstractColorMap getJaxbColorMap() {
		if ((colorMapName == null || dirty)
				&& colorMap instanceof AbstractColorMap) {
			return (AbstractColorMap) colorMap;
		} else {
			return null;
		}
	}

	public void setJaxbColorMap(AbstractColorMap colorMap) {
		this.colorMap = colorMap;
	}

	/**
	 * @return the colorMap
	 */
	public IColorMap getColorMap() {
		return colorMap;
	}

	/**
	 * @param colorMap
	 *            the colorMap to set
	 */
	public void setColorMap(IColorMap colorMap) {
		this.colorMap = colorMap;
		if (colorMap != null) {
			this.colorMapName = colorMap.getName();
			if (colorMap.getSize() != alphaMask.length) {
				alphaMask = Arrays.copyOf(alphaMask, colorMap.getSize());
			}
		}
		notifyListener();
	}

	/**
	 * @return the unit
	 */
	public Unit<?> getDisplayUnit() {
		return displayUnit;
	}

	/**
	 * @param unit
	 *            the unit to set
	 */
	public void setDisplayUnit(Unit<?> unit) {
		this.displayUnit = unit;
		recomputeLabels = true;

		displayToImageConverter = null;
		imageToDisplayConverter = null;

		displayToDataConverter = null;
		dataToDisplayConverter = null;
		notifyListener();
	}

	/**
	 * @return the colorMapMax
	 */
	public float getColorMapMax() {
		return colorMapMax;
	}

	/**
	 * @param colorMapMax
	 *            the colorMapMax to set
	 */
	public void setColorMapMax(float colorMapMax) {
		this.colorMapMax = colorMapMax;
		recomputeLabels = true;
		notifyListener();
	}

	/**
	 * @return the colorMapMin
	 */
	public float getColorMapMin() {
		return colorMapMin;
	}

	/**
	 * @param colorMapMin
	 *            the colorMapMin to set
	 */
	public void setColorMapMin(float colorMapMin) {
		this.colorMapMin = colorMapMin;
		recomputeLabels = true;
		notifyListener();
	}

	/**
	 * @return the dataMax
	 */
	public float getDataMax() {
		return dataMax;
	}

	/**
	 * @param dataMax
	 *            the dataMax to set
	 */
	public void setDataMax(float dataMax) {
		this.dataMax = dataMax;
		notifyListener();
	}

	/**
	 * @return the dataMin
	 */
	public float getDataMin() {
		return dataMin;
	}

	/**
	 * @param dataMin
	 *            the dataMin to set
	 */
	public void setDataMin(float dataMin) {
		this.dataMin = dataMin;
		notifyListener();
	}

	/**
	 * @return the colorBarIntervals
	 */
	public float[] getColorBarIntervals() {
		return colorBarIntervals;
	}

	/**
	 * @param colorBarIntervals
	 *            the colorBarIntervals to set
	 */
	public void setColorBarIntervals(float[] colorBarIntervals) {
		this.colorBarIntervals = colorBarIntervals;
		recomputeLabels = true;
		notifyListener();
	}

	/**
	 * @return the colorMapName
	 */
	public String getColorMapName() {
		return colorMapName;
	}

	/**
	 * @param colorMapName
	 *            the colorMapName to set
	 */
	public void setColorMapName(String colorMapName) {
		// assume we are not dirty if we set a name
		this.colorMapName = colorMapName;
		if (this.colorMap != null) {
			this.colorMap.setName(colorMapName);
		}
		notifyListener();
		dirty = false;
	}

	/**
	 * @return the dataUnit
	 */
	public Unit<?> getDataUnit() {
		return dataUnit;
	}

	/**
	 * @param dataUnit
	 *            the dataUnit to set
	 */
	public void setDataUnit(Unit<?> dataUnit) {
		this.dataUnit = dataUnit;

		if (imageUnit == null) {
			setImageUnit(dataUnit);
		}

		dataToImageConverter = null;
		imageToDataConverter = null;

		dataToDisplayConverter = null;
		displayToDataConverter = null;

		notifyListener();
	}

	/**
	 * @return the dataToDisplayConverter
	 */
	public UnitConverter getDataToDisplayConverter() {
		if (dataToDisplayConverter == null) {
			dataToDisplayConverter = constructConverter(dataUnit, displayUnit);
			if (dataToDisplayConverter != null) {
				notifyListener();
			}
		}
		return dataToDisplayConverter;
	}

	/**
	 * @return the displayToDataConverter
	 */
	public UnitConverter getDisplayToDataConverter() {
		if (displayToDataConverter == null) {
			displayToDataConverter = constructConverter(displayUnit, dataUnit);
			if (displayToDataConverter != null) {
				notifyListener();
			}
		}
		return displayToDataConverter;
	}

	public java.util.List<LabelEntry> getLabels() {
		if (recomputeLabels) {
			computeLabels();
			notifyListener();
		}
		return labels;
	}

	public boolean isLogarithmic() {
		return logarithmic;
	}

	public void setLogarithmic(boolean logarithmic) {
		recomputeLabels = recomputeLabels | this.logarithmic != logarithmic;
		this.logarithmic = logarithmic;
		notifyListener();
	}

	/**
	 * @return the dataToImageConverter
	 */
	public UnitConverter getDataToImageConverter() {
		if (dataToImageConverter == null) {
			dataToImageConverter = constructConverter(dataUnit, imageUnit);
			if (dataToImageConverter != null) {
				notifyListener();
			}
		}
		return dataToImageConverter;
	}

	/**
	 * @return the imageToDisplayConverter
	 */
	public UnitConverter getImageToDisplayConverter() {
		if (imageToDisplayConverter == null) {
			imageToDisplayConverter = constructConverter(imageUnit, displayUnit);
			if (imageToDisplayConverter != null) {
				notifyListener();
			}
		}
		return imageToDisplayConverter;
	}

	/**
	 * @return the imageUnit
	 */
	public Unit<?> getImageUnit() {
		return imageUnit;
	}

	/**
	 * @param imageUnit
	 *            the imageUnit to set
	 */
	public void setImageUnit(Unit<?> imageUnit) {
		this.imageUnit = imageUnit;
		recomputeLabels = true;

		imageToDataConverter = null;
		dataToImageConverter = null;

		imageToDisplayConverter = null;
		displayToImageConverter = null;
		notifyListener();
	}

	/**
	 * @return the imageToDataConverter
	 */
	public UnitConverter getImageToDataConverter() {
		if (imageToDataConverter == null) {
			imageToDataConverter = constructConverter(imageUnit, dataUnit);
			if (imageToDataConverter != null) {
				notifyListener();
			}
		}
		return imageToDataConverter;
	}

	/**
	 * @return the displayToImageConverter
	 */
	public UnitConverter getDisplayToImageConverter() {
		if (displayToImageConverter == null) {
			displayToImageConverter = constructConverter(displayUnit, imageUnit);
			if (displayToImageConverter != null) {
				notifyListener();
			}
		}
		return displayToImageConverter;
	}

	/**
	 * @param dataMapping
	 *            the dataMapping to set
	 */
	public void setDataMapping(DataMappingPreferences dataMapping) {
		this.dataMapping = dataMapping;
		recomputeLabels = true;
		if (dataMapping != null && displayUnit != null) {
			setImageUnit(dataMapping.getImageUnit(displayUnit));
		}
		notifyListener();
	}

	/**
	 * @return the dataMapping
	 */
	public DataMappingPreferences getDataMapping() {
		return dataMapping;
	}

	/**
	 * @return the formatString
	 */
	public String getFormatString() {
		return formatString;
	}

	/**
	 * @param formatString
	 *            the formatString to set
	 */
	public void setFormatString(String formatString) {
		this.formatString = formatString;
		notifyListener();
	}

	public ColorMapParameters clone() {
		ColorMapParameters cmp = new ColorMapParameters();
		cmp.colorBarIntervals = colorBarIntervals;
		cmp.colorMap = colorMap;
		cmp.colorMapMax = colorMapMax;
		cmp.colorMapMin = colorMapMin;
		cmp.colorMapName = colorMapName;
		cmp.dataMapping = dataMapping;
		cmp.dataMax = dataMax;
		cmp.dataMin = dataMin;
		cmp.dataToDisplayConverter = dataToDisplayConverter;
		cmp.dataToImageConverter = dataToImageConverter;
		cmp.dataUnit = dataUnit;
		cmp.formatString = formatString;
		cmp.imageToDataConverter = imageToDataConverter;
		cmp.imageToDisplayConverter = imageToDisplayConverter;
		cmp.imageUnit = imageUnit;
		cmp.labels = labels;
		cmp.recomputeLabels = recomputeLabels;
		return cmp;
	}

	public boolean isDirty() {
		return dirty;
	}

	public void setDirty(boolean dirty) {
		this.dirty = dirty;
		notifyListener();
	}

	private void notifyListener() {
		if (listener != null) {
			listener.colorMapChanged();
		}
	}

	public void setListener(IColorMapParametersListener listener) {
		this.listener = listener;
	}

	public void setAlphaMask(byte[] alphaMask) {
		this.alphaMask = alphaMask;
	}

	public byte[] getAlphaMask() {
		return alphaMask;
	}

	public boolean isUseMask() {
		return useMask;
	}

	public void setUseMask(boolean useMask) {
		this.useMask = useMask;
	}

	@Override
	public int hashCode() {
		if (listener != null) {
			return listener.hashCode();
		} else if (colorMap != null) {
			return colorMap.hashCode();
		} else {
			return super.hashCode();
		}
	}

	public void setMirror(boolean mirror) {
		this.mirror = mirror;
	}

	public boolean isMirror() {
		return mirror;
	}

	public static void main(String[] args) {
		float[] testValues = { -3.622782E-10f, -1.6778468E-8f, -3.3110254E-8f,
				-3.7973948E-8f, -2.7633986E-8f, -9.904648E-9f, -7.5229956E-10f,
				7.5189455E-10f, 1.20609505E-8f, 2.9364383E-8f, 2.7346168E-8f,
				1.3699442E-10f, -2.9937E-8f, -3.6884128E-8f, -1.6217847E-8f,
				1.1931893E-8f, 2.252287E-8f, 1.3290519E-8f, -2.4843547E-9f,
				-1.0386745E-8f, -6.1300884E-9f, 3.266153E-9f, 1.078073E-8f,
				1.2696603E-8f, 4.315502E-9f, -7.2422215E-9f, -1.2054819E-8f,
				-9.100724E-9f, -2.2520505E-9f, 4.031535E-9f, 6.908326E-9f,
				4.5536255E-9f, 5.3565863E-10f, -6.767242E-11f, 1.7508048E-9f,
				1.2594312E-9f, -3.5915362E-9f, -6.42762E-9f, -6.295979E-9f,
				-3.857202E-9f, -2.2052171E-9f, 6.51567E-10f, 3.1020404E-9f,
				3.2179108E-9f, 2.080211E-9f, 2.5326197E-10f, -2.181367E-9f,
				-3.2126295E-9f, -2.2162419E-9f, -5.682299E-10f, 7.367537E-11f,
				1.7393351E-9f, 3.2000091E-9f, 2.82146E-9f, 1.9214115E-9f,
				1.2637547E-9f, 1.1449517E-9f, 9.570286E-10f, 2.4719807E-10f,
				7.4189294E-11f, -1.9902982E-10f, -1.6482227E-9f, -2.645924E-9f,
				-1.4331059E-9f, -2.6336155E-10f, -1.5215194E-9f,
				-1.5059594E-9f, 2.014035E-10f, 3.565288E-10f, -1.5582429E-9f,
				-1.3200975E-9f, 1.2632209E-10f, 1.0882992E-9f, 1.5682087E-9f,
				4.24027E-9f, 3.1678654E-10f, -3.9073598E-9f, -3.4730894E-9f,
				-9.989449E-10f, -4.0364423E-9f, -6.3506116E-9f, -2.8335263E-9f,
				-3.1253768E-9f, -1.1271912E-9f, -3.0793168E-10f, 2.2091975E-9f,
				4.9031823E-9f, 1.0454194E-8f, 1.6462849E-8f, 1.339688E-8f,
				6.4451755E-9f, 3.820775E-9f, 1.7874671E-9f, -4.5423043E-9f,
				-4.7599173E-9f, -3.718451E-9f, 2.086526E-10f, 4.4792867E-9f,
				-5.1967337E-9f, -1.0332604E-8f };

		ColorMapParameters colorMapParameters = new ColorMapParameters();
		float min = -2.6945168e-7f;
		float max = 2.1146788e-7f;
		colorMapParameters.setColorMapMin(-2.6945168e-7f);
		colorMapParameters.setColorMapMax(2.1146788e-7f);
		colorMapParameters.setLogarithmic(true);
		DecimalFormat format = new DecimalFormat("0.###");
		int i = 0;
		for (float dispValue : testValues) {
			String s = format.format(dispValue);
			colorMapParameters.addLabel(dispValue, s);
			if (dispValue > max) {
				System.out.println(colorMapParameters.labels.get(i).location
						+ "to high");
			} else if (dispValue < min) {
				System.out.println(colorMapParameters.labels.get(i).location
						+ "to low");
			} else {
				System.out.println(colorMapParameters.labels.get(i).location);
			}
			i++;
		}
	}

	private float getIndexByValue(float dispValue) {
		UnitConverter displayToImage = getDisplayToImageConverter();
		float colorMapRange;
		if (logarithmic) {
			// float colorMapMin = this.colorMapMin * 1.25f;
			// float colorMapMax = this.colorMapMax * 1.25f;
			if (displayToImage != null) {
				dispValue = (float) displayToImage.convert(dispValue);
			}
			float index = 0.0f;
			// is this strictly negative, strictly positive or neg to pos
			// scaling?
			if (colorMapMin >= 0.0 && colorMapMax >= 0.0 && !isMirror()) {
				// simple calculation
				index = ((float) Math.log(dispValue) - (float) Math
						.log(colorMapMin))
						/ (float) Math.abs((float) Math.log(colorMapMax)
								- (float) Math.log(colorMapMin));
			} else if (colorMapMin <= 0.0 && colorMapMax <= 0.0 && !isMirror()) {
				index = ((float) (Math.log(dispValue) - (float) Math
						.log(colorMapMax)) / (float) Math.abs((float) Math
						.log(colorMapMin) - (float) Math.log(colorMapMax)));
			} else {
				// special case, neg to pos:
				float cmapMin = colorMapMin;
				float cmapMax = colorMapMax;
				float zeroVal = (float) Math.max(cmapMax,
						(float) Math.abs(cmapMin)) * 0.0001f;
				;

				if (isMirror() && (cmapMin > 0 || cmapMax < 0)) {
					if (cmapMax < 0) {
						cmapMax = -cmapMax;
						dispValue = -dispValue;
						zeroVal = -cmapMin;
					} else {
						zeroVal = cmapMin;
					}
					cmapMin = -cmapMax;

				}
				float leftZero = 0;
				float rightZero = 0;
				float absLogZeroVal = (float) Math.abs((float) Math
						.log(zeroVal));

				rightZero = absLogZeroVal + (float) Math.log(cmapMax);

				float cmapMax2 = -1 * cmapMin;

				leftZero = absLogZeroVal + (float) Math.log(cmapMax2);

				float zeroIndex = leftZero / (leftZero + rightZero);

				// figure out index for texture val
				float absTextureColor = (float) Math.abs(dispValue);
				if (dispValue == 0) {
					index = zeroIndex;
				} else if (absTextureColor <= zeroVal) {
					index = (float) zeroIndex;
				} else if (dispValue > 0.0) {
					// positive texture color value, find index from 0 to
					// cmapMax:
					float logTexColor = absLogZeroVal
							+ (float) Math.log(dispValue);

					float texIndex = logTexColor / rightZero;
					index = (float) (zeroIndex + ((1.0 - zeroIndex) * texIndex));
				} else {
					// negative texture color value, find index from 0 to
					// cmapMax:
					float logTexColor = absLogZeroVal
							+ (float) Math.log(absTextureColor);

					float texIndex = logTexColor / leftZero;
					index = (zeroIndex - (zeroIndex * texIndex));
				}
			}

			return index;
		} else {
			colorMapRange = colorMapMax - colorMapMin;

			if (colorMapRange != 0.0) {
				double pixelValue;
				if (displayToImage != null) {
					pixelValue = displayToImage.convert(dispValue);
				} else {
					pixelValue = dispValue;
				}
				float location;
				if (logarithmic) {
					location = (float) ((Math.log(pixelValue) - Math
							.log(colorMapMin)) / colorMapRange);
				} else {

					location = ((float) pixelValue - colorMapMin)
							/ colorMapRange;
				}
				return location;
			} else {
				return 0.0f;
			}
		}
	}

	/**
	 * Get the rgb color of the value
	 * 
	 * @param value
	 * @return
	 */
	// public RGB getRGBByValue(float value) {
	// float index = getIndexByValue(value);
	// if (index < 0.0f) {
	// index = 0.0f;
	// } else if (index > 1.0f) {
	// index = 1.0f;
	// }
	// return colorToRGB(colorMap.getColors().get(
	// (int) (index * colorMap.getSize())));
	// }

	/**
	 * Get the Color object of the value
	 * 
	 * @param value
	 * @return
	 */
	public Color getColorByValue(float value) {
		float index = getIndexByValue(value);
		if (index < 0.0f) {
			index = 0.0f;
		} else if (index > 1.0f) {
			index = 1.0f;
		}
		return colorMap.getColors().get((int) (index * colorMap.getSize()));
	}

	// public static RGB colorToRGB(Color c) {
	// return new RGB((int) (c.getRed() * 255f), (int) (c.getGreen() * 255f),
	// (int) (c.getBlue() * 255f));
	// }
	//
	// public static Color rgbToColor(RGB rgb) {
	// rg
	// return new Color(rgb.red / 255.0f, rgb.green / 255.0f,
	// rgb.blue / 255.0f);
	// }
}
