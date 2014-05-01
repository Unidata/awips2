
package gov.noaa.nws.ncep.edex.plugin.mosaic.common;

/**
 * Represents an i,j point on the Mosaic screen. Used to sort and maintain the
 * relationship of mosaic data.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class MosaicDataKey {

    private int i;

    private int j;

    /**
     * @return the i
     */
    public int getI() {
        return i;
    }

    /**
     * @param i
     *            the i to set
     */
    public void setI(int i) {
        this.i = i;
    }

    /**
     * @return the j
     */
    public int getJ() {
        return j;
    }

    /**
     * @param j
     *            the j to set
     */
    public void setJ(int j) {
        this.j = j;
    }

    @Override
    public boolean equals(Object o) {
        boolean rval = false;
    	System.out.println("In MosaicDataKey equals");

        MosaicDataKey that = (MosaicDataKey) o;

        if (this == that) {
            rval = true;
        } else if (i == that.i && j == that.j) {
            rval = true;
        }

        return rval;

    }

    @Override
    public int hashCode() {
    	System.out.println("In MosaicDataKey hashCode");

        int hash = 7;
        hash = 31 * hash + ((Integer) i).hashCode();
        hash = 31 * hash + ((Integer) j).hashCode();
        return hash;
    }

    @Override
    public String toString() {
    	System.out.println("In MosaicDataKey toString");

        return String.format("i: %s j: %s", i, j);
    }
}
