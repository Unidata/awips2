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
package com.raytheon.uf.viz.dataplugin.nswrc;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.nswrc.NSWRCConstants;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import org.locationtech.jts.geom.Coordinate;

/**
 * This class will calculate data values for radial radars.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */

public class RadialDataInterrogator {

	private NSWRCRadialRecord record;

	public RadialDataInterrogator(NSWRCRadialRecord record) {
		this.record = record;
	}

	public float getDataValue(NSWRCRadialRecord record, Coordinate latLon) {
		this.record = record;
		return getDataValue(latLon);
	}

	public float getDataValue(Coordinate latLon) {
		float[][] rval = getDataValues(new Coordinate[] { latLon });
		if (rval != null && rval.length > 0) {
			return rval[0][0];
		}

		return NSWRCConstants.FILL_VALUE;
	}

	public float[] getValueNoisePower(Coordinate latLon) {
		float[][] rval = getDataValues(new Coordinate[] { latLon });
		if (rval != null && rval.length > 0) {
			return rval[0];
		}

		return new float[] { NSWRCConstants.FILL_VALUE, NSWRCConstants.FILL_VALUE, NSWRCConstants.FILL_VALUE };
	}

	/**
	 * Used to return the data value for the lat/lon passed in. Used for radial
	 * data.
	 * 
	 * @param latLon
	 * @return
	 */
	public float[][] getDataValues(Coordinate[] latLonArray) {

		double[] input = new double[latLonArray.length * 2];
		double[] output = new double[input.length];

		int index = 0;
		for (Coordinate latLon : latLonArray) {
			input[index++] = latLon.x;
			input[index++] = latLon.y;
		}

		int x, y, lastRadialIndex, lastBin;
		double lastRange, lastAzimuth;
		float[][] rval = new float[latLonArray.length][3];

		try {
			ProjectedCRS crs = CRSCache.getInstance().constructStereographic(
					MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
					record.getLatitude(), record.getLongitude());
			MathTransform mt = CRSCache.getInstance().getTransformFromLatLon(crs);
			mt.transform(input, 0, output, 0, latLonArray.length);
		} catch (FactoryException fe) {
			return null;
		} catch (TransformException te) {
			return null;
		}

		for (index = 0; index < latLonArray.length; index++) {
			x = index * 2;
			y = x + 1;

			// Calculate azimuth
			lastAzimuth = Math.toDegrees(Math.atan2(output[x], output[y]));
			if (lastAzimuth < 0.0) {
				lastAzimuth += 360.0;
			}

			// Calculate range
			lastRange = Math.hypot(output[x], output[y]);

			if (record.getAngleData() != null) {
				// Find the correct Radial for the range and azimuth
				lastRadialIndex = -1;
				for (int i = 0; i < record.getNumRadials(); i++) {
					double thisAz = record.getAngleData()[i];
					double nextAz;
					if (i + 1 == record.getNumRadials()) {
						float totalDiff = record.getAngleData()[record.getNumRadials() - 1]
								- record.getAngleData()[0];
						if (totalDiff < 0) {
							totalDiff += 360;
						}

						float average = totalDiff / (record.getNumRadials() - 1);

						// determine if average dist is close enough to dist
						// at [0].
						float actualDiff = 360 - totalDiff;

						// if the difference between actual and calculated
						// is less than half a degree, we can assume the
						// radial data does in fact wrap all the way in a
						// circle and we can use the starting angle as our
						// ending angle. Otherwise, we must assume the data
						// is a slice of a circle and we should use our
						// calculated end location
						nextAz = record.getAngleData()[record.getNumRadials() - 1]
								+ average;
						if (Math.abs(actualDiff - average) < 0.5) {
							nextAz = record.getAngleData()[0] + 360.0f;
						}
					} else {
						nextAz = record.getAngleData()[i + 1];
					}
					if (nextAz < thisAz) {
						if (lastAzimuth > thisAz) {
							nextAz += 360.0;
						} else {
							thisAz -= 360.0;
						}
					}
					if ((lastAzimuth >= thisAz) && (lastAzimuth < nextAz)) {
						lastRadialIndex = i;
						break;
					}
				}

				// Get the bin number
				lastBin = (int) (lastRange / (record.getBinWidth() * 
						Math.cos(Math.toRadians(record.getElevationAngle().floatValue()))));

				// Get the data value if the bin is valid
				int startBin = 0;
				int endBin = record.getNumBins();
				if (record.getjStart() != null) {
					endBin += startBin = record.getjStart();
				}
				if (lastRadialIndex >= 0 && lastRadialIndex < record.getNumRadials()
						&& lastBin >= startBin && lastBin < endBin) {
					rval[index] = record.getDataNoisePower(lastRadialIndex, lastBin - startBin);
				}
			}
		}
		return rval;
	}

	/**
	 * @return the record
	 */
	public NSWRCRadialRecord getRecord() {
		return record;
	}

	/**
	 * @param record
	 *            the record to set
	 */
	public void setRecord(NSWRCRadialRecord record) {
		this.record = record;
	}
}
