package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.List;

/**
 * 
 * Provides a single sampled data set of all members having the same level and
 * same unit at the location of the cursor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2015  12301       jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class SingleSampleInfo {
    /**
     * The data name, e.g. high, temperature
     */
    private String dataName = "";

    /**
     * The location string of the sampled data, 'long-lat'
     */
    private String location = "";

    /**
     * The level string of the data, e.g. 500mb, sfc
     */
    private String level = "";

    /**
     * The data display unit, e.g. m, f, c
     */
    private String unit = "";

    /**
     * The sampled values of the current location of all members
     */
    private List<Float> values = null;

    /**
     * Constructor.
     * 
     * @param dataName
     *            -The data name
     * @param location
     *            -The location string of the sampled data
     * @param level
     *            -The level string of the data
     * @param unit
     *            -The data display unit
     * @param values
     *            -The sampled values
     */
    public SingleSampleInfo(String dataName, String location, String level,
            String unit, List<Float> values) {
        super();
        this.dataName = dataName;
        this.location = location;
        this.level = level;
        this.unit = unit;
        this.values = values;
    }

    /**
     * Gets the data name string.
     * 
     * @return The data name
     */
    public String getDataName() {
        return dataName;
    }

    /**
     * Gets the location string.
     * 
     * @return The location string.
     */
    public String getLocation() {
        return location;
    }

    /**
     * Gets the level String.
     * 
     * @return The level String
     */
    public String getLevel() {
        return level;
    }

    /**
     * Gets the unit String.
     * 
     * @return The unit String.
     */
    public String getUnit() {
        return unit;
    }

    /**
     * Gets the sampled Values.
     * 
     * @return Values as float list
     */
    public List<Float> getValues() {
        return values;
    }

    /**
     * Gets the sampled Values.
     * 
     * @return Values as float array
     */
    public float[] getValuesFloatArray() {
        if (values.size() == 0) {
            return null;
        }
        float[] valuesFloat = new float[values.size()];
        for (int i = 0; i < values.size(); i++) {
            valuesFloat[i] = values.get(i);
        }
        return valuesFloat;
    }

}
