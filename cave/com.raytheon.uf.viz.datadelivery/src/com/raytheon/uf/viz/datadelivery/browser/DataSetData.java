package com.raytheon.uf.viz.datadelivery.browser;

import java.util.ArrayList;

public class DataSetData {

    private String name;

    private String times;

    private String levels;

    private ArrayList<String> parameters;

    public DataSetData() {

    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTimes() {
        return times;
    }

    public void setTimes(String times) {
        this.times = times;
    }

    public String getLevels() {
        return levels;
    }

    public void setLevels(String levels) {
        this.levels = levels;
    }

    public ArrayList<String> getParameters() {
        return parameters;
    }

    public void setParameters(ArrayList<String> parameters) {
        this.parameters = parameters;
    }

    public String getFormattedData() {
        StringBuilder sb = new StringBuilder();

        sb.append("Name: ").append(getName()).append("\n");
        sb.append("Times: ").append(getTimes()).append("\n");
        sb.append("Levels: ").append(getLevels()).append("\n");
        sb.append("Parameters: ").append("\n");

        for (String s : getParameters()) {
            sb.append("\t").append(s).append("\n");
        }

        return sb.toString();
    }
}
