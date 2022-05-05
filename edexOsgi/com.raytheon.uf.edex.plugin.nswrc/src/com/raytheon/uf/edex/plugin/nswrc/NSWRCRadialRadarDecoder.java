/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.edex.plugin.nswrc;

import java.io.File;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import ucar.ma2.Array;
import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.Group;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.nswrctools.DecoderTools;

/**
 * NSWRC (NextGen Surveillance and Weather Radar Capability) Radial file decoder 
 *   - Decodes 3-dimensional products from radial NSWRC files
 *   - Stores data in NSWRCRadialRecord objects
 *     - Creates one NSWRCRadialRecord object for each product, for each altitude value
 *       in the file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2013             ekladstrup  Initial creation
 * Apr 22, 2014 3048       mweeks      Updates for peer review and 13.5.4 baseline.
 * Jun 20, 2014	3048	   tkuster	   Clean up entry path into decoder
 * Aug 25, 2014 3555       mweeks      Updates for 14.3.1 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */
public class NSWRCRadialRadarDecoder extends AbstractDecoder {

	/**
	 * calls decode(File) from the filePath param
	 * @param filePath
	 * @return
	 * @throws DecoderException
	 */
	public PluginDataObject[] decode(String filePath) throws DecoderException {
		File file = new File(filePath);
		return decode(file);
	}
	
	/**
	 * attempts to open the file as a netcdf file and pass it to decode(NetcdfFile)
	 * @param file
	 * @return
	 * @throws DecoderException
	 */
	public PluginDataObject[] decode(File file) throws DecoderException {
		PluginDataObject[] rval = new PluginDataObject[0];
		NetcdfFile ncFile = null;
		
		// try to open the file as netcdf
		if (file.exists()) {
			String filePath = file.getAbsolutePath();
			try {
				ncFile = NetcdfFile.open(filePath);
				
				rval = decode(ncFile);
			} catch (IOException e ) {
				throw new DecoderException("Cannot open netcdf file: "
						+ file.getName(), e);
			} finally {
				if (ncFile != null) try {
					ncFile.close();
				} catch (IOException e) {
					throw new DecoderException("Cannot close netcdf file: "
							+ file.getName(), e);					
				}
			}
		}
		
		return rval;
	}
	

    public NSWRCRadialRecord[] decode(NetcdfFile ncFile) throws DecoderException {
        NSWRCRadialRecord[] rval = new NSWRCRadialRecord[0];

        Group rootGroup = ncFile.getRootGroup();

        /** Variables **/
        List<Variable> rootVariables = rootGroup.getVariables();
        List<Variable> dataVariables = getDataVariables(rootVariables);

        // grab the azimuths for the radials
        Variable startAzimuth = DecoderTools.getVariable(rootVariables,
                "Start_Azimuth");
        double stopAzimuth = -360.0;

        if (startAzimuth == null) {
            startAzimuth = DecoderTools.getVariable(rootVariables, "Azimuth");
            stopAzimuth = getLastStopAzimuth(startAzimuth);
        } else {
            stopAzimuth = getLastStopAzimuth(DecoderTools.getVariable(
                rootVariables, "Stop_Azimuth"));
        }

        // Compute the Elevation
        double elevation = computeElevation(DecoderTools
                .getVariable(rootVariables, "Elevation"));
        int firstRadial = computeFirstRadialIndex(startAzimuth);

        /** Dimensions **/
        List<Dimension> rootDimensions = rootGroup.getDimensions();

        // get the gate count
        Dimension gateDimension = DecoderTools.getDimension(rootDimensions, "Gate");
        int gateCount = gateDimension.getLength();

        // get the real radial count
        Dimension radialDimension = DecoderTools.getDimension(rootDimensions, "Radial");
        int radialCount = radialDimension.getLength();
        int realRadialCount = radialCount - firstRadial;

        // convert the angle data
        float[] angleData = createAngleData(startAzimuth, stopAzimuth,
                firstRadial, realRadialCount);

        // get the gate resolution ( mm in data file, convert to m )
        Variable gateWidthVar = DecoderTools.getVariable(rootVariables, "GateWidth");
        double rawGateResolution = 0;
        int gateResolution = 0;
		if (gateWidthVar == null) {
			throw new DecoderException("Unable to read null gate width variable");
		}

		try {
			Array array = gateWidthVar.read();

			for (Attribute att : gateWidthVar.getAttributes()) {
				if("Units".equalsIgnoreCase(att.getName())) {
					if ("Millimeters".equalsIgnoreCase((String)att.getValue(0))) {
						rawGateResolution = array.getDouble(0);
				        gateResolution = (int) Math.round(rawGateResolution / 1000.0);
					} else {
						throw new DecoderException("Unexpected units value for gate width variable");
					}
					break;
				}
			}
		} catch (IOException e) {
			throw new DecoderException("Unable to read gate width variable");
		}

        // noise and power variables for filtering
        float[] ncp = null;
        float[] snr = null;
        try {
            // power
            Variable power = DecoderTools.getVariable(rootVariables,
                    "NormalizedCoherentPower");
            if (power != null) {
                Array array = power.read();
                FloatBuffer floatBuffer = array.getDataAsByteBuffer()
                        .asFloatBuffer();
                floatBuffer.position(gateCount * firstRadial);
                int remaining = floatBuffer.remaining();
                ncp = new float[remaining];
                floatBuffer.get(ncp);
            }
        } catch (IOException e) {
            throw new DecoderException(
                    "Unable to read the NormalizedCoherentPower variable", e);
        }

        try {
            // noise
            Variable noise = DecoderTools.getVariable(rootVariables,
                    "SignalToNoiseRatio");
            if (noise != null) {
                Array array = noise.read();
                FloatBuffer floatBuffer = array.getDataAsByteBuffer()
                        .asFloatBuffer();
                floatBuffer.position(gateCount * firstRadial);
                int remaining = floatBuffer.remaining();
                snr = new float[remaining];
                floatBuffer.get(snr);
            }
        } catch (IOException e) {
            throw new DecoderException(
                    "Unable to read the SignalToNoiseRatio variable", e);
        }

        /** Attributes **/
        List<Attribute> rootAttributes = rootGroup.getAttributes();

        // get location data from the attributes
        float latitude = DecoderTools.getAttribute(rootAttributes, "Latitude")
                .getNumericValue().floatValue();
        float longitude = DecoderTools
                .getAttribute(rootAttributes, "Longitude").getNumericValue()
                .floatValue();
        float height = DecoderTools.getAttribute(rootAttributes, "Height")
                .getNumericValue().floatValue();

        ArrayList<PluginDataObject> records = new ArrayList<PluginDataObject>(
                dataVariables.size());

        // get the icao
        String locationName = getRadarLocation(rootAttributes);

        // get the time
        long time = getTime(DecoderTools.getVariable(rootVariables, "Time"));

        // get the start index ( StartRange / gateWidth rounded to integer )
        int jstart = getJStart(rawGateResolution,
                DecoderTools.getVariable(rootVariables, "StartRange"));

        for (Variable dataVariable : dataVariables) {
            NSWRCRadialRecord record = createRadarRecord(dataVariable,
                    gateCount, angleData, elevation, firstRadial,
                    realRadialCount, gateResolution, height, latitude,
                    longitude, time, locationName, jstart, snr, ncp);
            records.add(record);
        }

        rval = records.toArray(new NSWRCRadialRecord[records.size()]);
        return rval;
    }

	/**
	 * Returns the start index ( StartRange / gateWidth rounded to integer ).
	 *
	 * @param rawGateResolution
	 * @param variable
	 * @return
	 * @throws DecoderException
	 */
    protected int getJStart(double rawGateResolution, Variable startRange)
            throws DecoderException {
        if (startRange == null) {
            throw new DecoderException(
                    "Unable to read null start range variable");
        }

        try {
            double averageRange = Math.abs(startRange.read().getDouble(0));

            int jstart = (int) Math.round(averageRange / rawGateResolution);

            return jstart;
        } catch (IOException e) {
            throw new DecoderException("Unable to read start range array");
        }
    }

    protected String getRadarLocation(List<Attribute> rootAttributes) {
		Attribute name = DecoderTools.getAttribute(rootAttributes, "RadarName");
		String radarName = name.getStringValue();
		return radarName;
	}

    protected List<Variable> getDataVariables(List<Variable> rootVariables) {
        List<Variable> rval = new ArrayList<Variable>();

        for (Variable var : rootVariables) {
            // look for 2-dimensional data ( Radial and Gate )
            if (var.getRank() == 2) {
                List<Dimension> dimensions = var.getDimensions();
                if (DecoderTools.getDimension(dimensions, "Radial") != null
                        && DecoderTools.getDimension(dimensions, "Gate") != null) {
                    if (!(var.getShortName().equalsIgnoreCase(
                            "SignalToNoiseRatio") || var.getShortName()
                            .equalsIgnoreCase("NormalizedCoherentPower"))) {
                        rval.add(var);
                    }
                }
            }
        }

        return rval;
    }

    protected long getTime(Variable variable) throws DecoderException {
		if (variable == null) {
			throw new DecoderException("Unable to read null time variable");
		}

		try {
			// need to convert from seconds to milliseconds
			return (long) variable.read().getInt(0) * 1000;
		} catch (IOException e) {
			throw new DecoderException("Unable to read time variable", e);
		}
	}

	protected double computeElevation(Variable radialElevation)
			throws DecoderException {
		double elevation = -180.0;
		boolean found = false;
		if (radialElevation != null) {
			try {
				Array elevationArray = radialElevation.read();
				// assume double for now, but check and complain if it isn't
				if (radialElevation.getDataType().equals(DataType.DOUBLE)) {
					// loop over elements until 3 concurrent values are the same
					double[] elev = (double[]) elevationArray
							.copyTo1DJavaArray();
                    double back1 = elev[0];
                    double back2 = elev[1];
                    for (int i = 2; i < elev.length && !found; ++i) {
						if (elev[i] == back1 && back1 == back2) {
							found = true;
							elevation = elev[i];
						} else {
							back2 = back1;
							back1 = elev[i];
						}
					}
                } else if (radialElevation.getDataType().equals(DataType.FLOAT)) {
                    // loop over elements until 3 concurrent values are the same
                    float[] elev = (float[]) elevationArray.copyTo1DJavaArray();
                    float back1 = elev[0];
                    float back2 = elev[1];
                    for (int i = 2; i < elev.length && !found; ++i) {
                        if (elev[i] == back1 && back1 == back2) {
                            found = true;
                            elevation = elev[i];
                        } else {
                            back2 = back1;
                            back1 = elev[i];
                        }
                    }
				} else {
					throw new DecoderException(
                            "Elevation data was not in a supported format (double, float)");
				}
			} catch (IOException e) {
				throw new DecoderException("Unable to read elevation variable",
						e);
			}
		} else {
			throw new DecoderException("Unable to find elevation variable");
		}

		if (found == false) {
			throw new DecoderException(
					"Unable to find 3 concurrent elevations that are the same value");
		}
		return elevation;
	}

    protected float[] createAngleData(Variable startAzimuth,
            double stopAzimith, int firstRadial, int realRadialCount)
            throws DecoderException {
        if (startAzimuth == null) {
            throw new DecoderException(
                    "Unable to read null start azimuth variable");
        }

        try {
            Array array = startAzimuth.read();
            if (startAzimuth.getDataType().equals(DataType.DOUBLE)) {
                double[] start = (double[]) array.copyTo1DJavaArray();

                float[] angleData = new float[realRadialCount];
                // read all but the last start, use the end for the last element
                for (int i = firstRadial; i < start.length - 1; ++i) {
                    angleData[i - firstRadial] = (float) start[i];
                }

                angleData[angleData.length - 1] = (float) stopAzimith;

                return angleData;
            } else if (startAzimuth.getDataType().equals(DataType.FLOAT)) {
                float[] start = (float[]) array.copyTo1DJavaArray();

                float[] angleData = new float[realRadialCount];
                // read all but the last start, use the end for the last element
                for (int i = firstRadial; i < start.length - 1; ++i) {
                    angleData[i - firstRadial] = start[i];
                }

                angleData[angleData.length - 1] = (float) stopAzimith;

                return angleData;
            } else {
                throw new DecoderException(
                        "start azimuth data was not in a supported format (double, float)");
            }
        } catch (IOException e) {
            throw new DecoderException("Unable to read startAzimuth variable",
                    e);
        }
    }

    protected double getLastStopAzimuth(Variable variable)
            throws DecoderException {
        if (variable == null) {
            throw new DecoderException("Unable to read null variable");
        }
        try {
            Array array = variable.read();
            double double1 = array.getDouble((int) array.getSize() - 1);
            return double1;
        } catch (IOException e) {
            throw new DecoderException("Unable to read variable", e);
        }
    }

    protected int computeFirstRadialIndex(Variable startAzimuth)
            throws DecoderException {
        int radialIndex = -1;
        boolean found = false;

        if (startAzimuth != null) {
            try {
                Array array = startAzimuth.read();
                // make sure it is a double array
                if (startAzimuth.getDataType().equals(DataType.DOUBLE)) {
                    double[] azimuths = (double[]) array.copyTo1DJavaArray();
                    double back3 = azimuths[0];
                    double back2 = azimuths[1];
                    double back1 = azimuths[2];
                    // loop until 4 concurrent increasing azimuths are found
                    for (int i = 3; i < azimuths.length && !found; ++i) {
                        if (azimuths[i] > back1 && back1 > back2
                                && back2 > back3) {
                            found = true;
                            radialIndex = i - 3;
                        } else {
                            back3 = back2;
                            back2 = back1;
                            back1 = azimuths[i];
                        }
                    }
                } else if (startAzimuth.getDataType().equals(DataType.FLOAT)) {
                    float[] azimuths = (float[]) array.copyTo1DJavaArray();
                    float back3 = azimuths[0];
                    float back2 = azimuths[1];
                    float back1 = azimuths[2];
                    // loop until 4 concurrent increasing azimuths are found
                    for (int i = 3; i < azimuths.length && !found; ++i) {
                        if (azimuths[i] > back1 && back1 > back2
                                && back2 > back3) {
                            found = true;
                            radialIndex = i - 3;
                        } else {
                            back3 = back2;
                            back2 = back1;
                            back1 = azimuths[i];
                        }
                    }
                } else {
                    throw new DecoderException(
                            "start azimuth variable was not a supported type (double, float)");
                }
            } catch (IOException e) {
                throw new DecoderException(
                        "Unable to read start azimuth variable", e);
            }
        } else {
            throw new DecoderException("Unable to find start azimuth variable");
        }

        if (found == false) {
            throw new DecoderException(
                    "Unable to find 4 increasing azimuth starts");
        }
        return radialIndex;
    }

    protected NSWRCRadialRecord createRadarRecord(Variable dataVariable,
            int gateCount, float[] angleData, double radialElevation,
            int firstRadial, int realRadialCount, int gateResolution,
            float height, float latitude, float longitude, long time,
            String locationName, int jstart, float[] snr, float[] ncp)
            throws DecoderException {

        NSWRCRadialRecord record = new NSWRCRadialRecord();

        record.setDataTime(new DataTime(time, new TimeRange(time, time)));
        record.setAngleData(angleData);
        record.setNumBins(new Integer(gateCount));
        record.setNumRadials(new Integer(realRadialCount));
        record.setLongitude(new Float(longitude));
        record.setLatitude(new Float(latitude));
        
        for (Attribute att : dataVariable.getAttributes()) {
	        if (att.getName().contains("Unit")) {
	        	String unitString = att.getStringValue();
	        	if ("MetersPerSecond".equalsIgnoreCase(unitString)) {
	        		unitString = "m/s";
	        	}
	        	record.setUnit(unitString);
	        	break;
	       	}
	    }

        record.setjStart(new Integer(jstart));

        // raw data
        try {
            Array array = dataVariable.read();

            if (dataVariable.getDimension(0).getName().equals("Gate")) {
                // switch order of the array
                array = array.permute(new int[] { 1, 0 });
            }
            FloatBuffer floatBuffer = array.getDataAsByteBuffer()
                    .asFloatBuffer();
            floatBuffer.position(gateCount * firstRadial);
            int remaining = floatBuffer.remaining();
            float[] data = new float[remaining];
            floatBuffer.get(data);

            record.setData(data);
            record.setSignal_to_noise(snr);
            record.setNormalized_coherent_power(ncp);
        } catch (IOException e) {
            throw new DecoderException("Unable to read data variable");
        }
        // gate resolution in m
        record.setBinWidth(new Integer(gateResolution));
        // true elevation angle
        record.setElevationAngle(new Double(radialElevation));

        DecimalFormat format = new DecimalFormat("0.0#");
        record.setAngleIdentifier(format.format(radialElevation));

        // some cleanup for the persistance
        record.setProductName(dataVariable.getShortName());
        record.setLocationName(locationName);

        return record;
    }
}
