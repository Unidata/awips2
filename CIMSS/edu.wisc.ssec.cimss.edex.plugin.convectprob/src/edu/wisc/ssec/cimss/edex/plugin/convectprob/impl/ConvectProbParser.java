package edu.wisc.ssec.cimss.edex.plugin.convectprob.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import edu.wisc.ssec.cimss.common.dataplugin.convectprob.impl.ShapeObject;

/**
 * NOAA/CIMSS Prob Severe Model Data Parser
 *
 * Data parser that parses shapefile records of NOAA/CIMSS Prob Severe Model
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   lcronce     Initial Creation.
 *
 * </pre
 *
 * @author Lee Cronce
 * @version 1.0
 *
 */
public class ConvectProbParser {

    private final IUFStatusHandler statusHandler = UFStatus.getHandler(ConvectProbParser.class);

    int currentShape = -1;

    private List<ShapeObject> shapes;

    private String validTime;

    private List<String> fileLines;

    /**
     * Default empty constructor
     */
    public ConvectProbParser() {
    }

    /**
     * Calls setData method for defining shape data
     *
     * @param File object passed on from EDEX
     */
    public ConvectProbParser(File file) {
        setData(file);
    }

    /**
     * Calls findShapes method to define shape data
     *
     * @param File object passed on from EDEX
     */
    private void setData(File file) {
        currentShape = -1;
        fileLines = separateLines(file);
        validTime = findTime(fileLines, file);
        shapes = findShapes(fileLines);
        if((shapes != null) && (shapes.size() > 0)) {
            currentShape = 0;
        }
    }

    /**
     * Returns convectprob data file valid time reference
     *
     * @return String object defining valid time reference
     */
    public String getValidTime() {
        return validTime;
    }

    /**
     * Determines if parser contains any more reports
     *
     * @return Boolean object defining "Does this parser contain any more reports?"
     */
    public boolean hasNext() {
        boolean next = (shapes != null);
        if(next) {
            next = ((currentShape >= 0) && (currentShape < shapes.size()));
        }
        if(!next) {
            shapes = null;
            currentShape = -1;
        }
        return next;
    }

    /**
     * Gets the next available report and returns a null reference if no
     * more reports are available.
     *
     * @return Next available shape object
     */
    public ShapeObject next() {

        ShapeObject shape = null;
        if(currentShape < 0) {
            return shape;
        }
        if(currentShape >= shapes.size()) {
            shapes = null;
            currentShape = -1;
        } else {
            shape = shapes.get(currentShape++);
        }
        return shape;
    }


    /**
     * Parses out the data from the passed file object
     *
     * @param ArrayList of String type containing individual lines of a convectprob data file
     * @return String object containing convectprob data valid time
     */
    private String findTime(List<String> fileLines, File file) {
        String validTime = null;

        if (fileLines != null) {
            try {
                String vTime = fileLines.get(0);
                if (!vTime.substring(0,5).equals("Valid")) {
                    if (file.getName().substring(0,4).equals("SSEC")) {
                        validTime = file.getName().substring(23,38);
                    } else {
                        validTime = null;
                    }
                } else{
                    validTime = vTime.split(" ")[1];
                }
            } catch (Exception e) {
                statusHandler.error("Problem acquiring convectprob data valid date and time", e);
            }
        }

        return validTime;
    }

    /**
     * Parses out the data from the passed file object
     *
     * @param File object passed on from EDEX
     * @return ArrayList containing the shape data
     */
    private List<ShapeObject> findShapes(List<String> fileLines) {
        List<ShapeObject> shapes = new ArrayList<ShapeObject>();

        if (fileLines != null) {
            for (String line : fileLines) {
                if (!line.substring(0,5).equals("Valid")) {
                    try {
                        ShapeObject shape = new ShapeObject();
                        String[] shapeAttributes = line.split(":");
                        String type = shapeAttributes[0];
                        shape.setType(type);
                        int probability = Integer.parseInt(shapeAttributes[1].replaceAll("\\D", ""));
                        shape.setProbability(probability);
                        String mucape = shapeAttributes[2];
                        shape.setMucape(mucape);
                        String ebshear = shapeAttributes[3];
                        shape.setEbshear(ebshear);
                        String mesh = shapeAttributes[4];
                        shape.setMesh(mesh);
                        String rcemiss = shapeAttributes[5];
                        shape.setRcemiss(rcemiss);
                        String rcicecf = shapeAttributes[6];
                        shape.setRcicecf(rcicecf);
                        String[] points = shapeAttributes[7].split(",");
                        float[] latitudes = new float[points.length/2];
                        float[] longitudes = new float[points.length/2];
                        for (int i=0; i < points.length; i++) {
                            if ((i == 0) || ((i%2) == 0)) {
                                latitudes[i/2] = Float.parseFloat(points[i]);
                            } else {
                                longitudes[(i-1)/2] = Float.parseFloat(points[i]);
                            }
                        }
                        StringBuffer strbuf = new StringBuffer();
                        strbuf.append("POLYGON((");
                        for (int j=0; j < latitudes.length; j++) {
                            strbuf.append(Float.toString(longitudes[j]));
                            strbuf.append(" ");
                            strbuf.append(Float.toString(latitudes[j]));
                            if (j < latitudes.length-1) {
                                strbuf.append(",");
                                strbuf.append(" ");
                            }
                        }
                        strbuf.append("))");
                        shape.setPolygon(strbuf.toString());
                        String objectid = shapeAttributes[8];
                        shape.setObjectid(objectid);
                        if (latitudes.length >= 4 || longitudes.length >= 4) {
                            shapes.add(shape);
                        }
                    } catch (Exception e) {
                        statusHandler.error("Problem defining convectprob shape object from read line: " + line, e);
                    }
                }
            }
        }

        return shapes;
    }

    /**
     * Separates out lines of data from the input data file
     *
     * @param File passed on by EDEX
     * @return ArrayList of lines of data from data file
     */
    private List<String> separateLines(File file) {
        List<String> fileLines = null;

        try {
            if (file != null) {
                BufferedReader reader = new BufferedReader(new FileReader(file));
                fileLines = new ArrayList<String>();
                String line;
                while ((line = reader.readLine()) != null) {
                    fileLines.add(line);
                }
                reader.close();
            }
        } catch (Exception e) {
            statusHandler.error("Problem with reading lines of the convectprob input file: " + file.getName(), e);
        }
        return fileLines;
    }

}
