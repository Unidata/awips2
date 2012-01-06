package com.raytheon.viz.pointdata.lookup;

public interface IAbstractLookupTable {

    /**
     * lookup the key from the table, numerical values should be converted to a
     * string before lookup.
     * 
     * @param key
     * @return
     */
    public abstract String lookup(String key);

    /**
     * set the mode of the table ( the plotMode attribute )
     * 
     * @param mode
     */
    public abstract void setMode(String mode);
}
